/* -----------------------------------------------------------------------------
 *
 * Memory layout of continuation frames vs. AP_STACK closures.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include <stddef.h>
#include <string.h>

/*
Note [Ask the RTS for memory layout of AP_STACK]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The bytecode compiler in ghc currently assigns to each breakpoint captured variable
an offset(free_var_i_AP_STACK_offset) to read that variable off of a pointer into
an AP_STACK constructed when we stop at that breakpoint.

However, when we want to read the variables of frames from breakpoints of other
frames on the stack, that offset can't be used to index other frame's free variables,
because it is meant specifically as an offset into the AP_STACK constructed when
we stop at a breakpoint.

While ghc doesn't provide more information regarding a variable offset into a frame's
free vars on the stack, rather than into the stopped-at-breakpoint-AP_STACK, we
reverse engineer the offset of the frame's payload into AP_STACK (bco_args_1_AP_STACK_offset)
so that on the Haskell side we can calculate the i-th free var index by
`(free_var_i_AP_STACK_offset - bco_args_1_AP_STACK_offset)`.

There are two main cases we need to cover to reverse engineer the offset into
a frame's variables correctly:

    - The frame is a function or case continuation (type: RET_BCO),
        then the offset we want is fixed.
    - The frame is a case continuation for an unboxed tuple-scrutinee
        (type: RET_BCO, header == &stg_ctoi_t[N]_info), then the offset we want
        depends on the tuple size which is available from the frame.

In both cases we manage to rely on the RTS to do most size calculations for us.

## RET_BCO frames reminder

RET_BCO frames all have the same general shape
```
bco_arg_M
...
bco_arg_2
bco_arg_1
&bco
&stg_$name_info
```
where the `bco_arg_i` words are generally referred as the `bitmap`
because their size and pointerness is described by a bitmap object inside `bco`.
The bitmap allows GC and other code to process them uniformly.

## Plan
What we do is:
  1. Identify which RET_BCO frame we are dealing with by the InfoTable pointer in it.
  2. Predict which kind of return frame it would have if we stop there.
  3. Call `stack_frame_sizeW` on a stub of that frame.
       - Here we rely on `stack_frame_sizeW` only looking at the first two words.
  4. Add 2 to account for the offset of the bitmap words into the continuation frame itself.

## Complications

### `stg_apply_interp`

This is the "apply BCO as function" frame.

In this case we skip step 3 because there is
no return frame stored for this frame.

### `stg_ctoi_t[N]`

These are the case continuation frames for unboxed tuples,
note the `N` is not the size of the tuple.

Their shape is
```
.. free vars ..
&tuple_bco
call_info        -- bco_arg_0
&bco
&stg_ctoi_t[N]_info
```
with return frames of shape
```
tuple_data_M
...
tuple_data_2
tuple_data_1
call_info
&tuple_bco
&stg_ret_t[N]_info
```
these are RET_BCO frames too, so their size is given
by the bitmap info in the tuple_bco object, which
we then need to include in the stub we create in step 3.
As shown above though, &tuple_bco is stored in the
stg_ctoi_t[N] frame we started with, so we have all we need.

Note that in the `ctoi` frames both call_info and &tuple_bco
are part of the frame's bitmap like everything above them,
so those words do not affect offset calculations.

*/

#define BCO_FRAME_NOT_BCO        0
#define BCO_FRAME_UNKNOWN        1
#define BCO_FRAME_APPLY_INTERP   2
#define BCO_FRAME_CTOI_R1P       3
#define BCO_FRAME_CTOI_R1N       4
#define BCO_FRAME_CTOI_F1        5
#define BCO_FRAME_CTOI_D1        6
#define BCO_FRAME_CTOI_L1        7
#define BCO_FRAME_CTOI_V         8
#define BCO_FRAME_CTOI_T         9

#define BCO_FRAME_CTOI_TN_BASE  100
#define BCO_FRAME_CTOI_TN_MAX    62          /* stg_ctoi_t62 is the last in 9.14 */
#define BCO_FRAME_CTOI_TN(n)    (BCO_FRAME_CTOI_TN_BASE + (n))

#define BCO_FRAME_IS_CTOI_TN(k) \
    ((k) >= BCO_FRAME_CTOI_TN_BASE && \
     (k) <= BCO_FRAME_CTOI_TN_BASE + BCO_FRAME_CTOI_TN_MAX)

// BcoFrameEntry represents continuation/apply frames we care about.
typedef struct {
    const void *info;           // the stg_$name_info pointer of the frame.
    StgWord     kind;           // enum tag, only used for error reporting atm.
    const char *name;           // readable name for debugging
    const void *return_info;       // expected return frame info pointer (e.g. stg_ret_v_info, stg_ret_t_info)
    StgWord     return_bco_offset; // If the return frame is of type RET_BCO as well, where to find its (StgBCO*) in this frame. 0 otherwise. e.g. tells you where `tuple_bco` is in `stg_ctoi_t[N]` frames.
} BcoFrameEntry;


#define ENT_BCO(sym, k, r, o)   { &sym##_info, (k), #sym, r, o },
#define ENT(sym, k, r)   ENT_BCO(sym, k, r, 0)
#define ENT_T(n)      { &stg_ctoi_t##n##_info, BCO_FRAME_CTOI_TN(n), "stg_ctoi_t" #n, &stg_ret_t_info, 3 },

// Mapping from continuation/apply frame to its return frame (info pointers) and other metadata.
static const BcoFrameEntry bco_frame_table[] = {
    ENT(stg_apply_interp, BCO_FRAME_APPLY_INTERP, NULL) //frame used to suspend run_BCO, not to wait for a return.
    ENT(stg_ctoi_R1p,     BCO_FRAME_CTOI_R1P,     &stg_ret_p_info)
    ENT(stg_ctoi_R1n,     BCO_FRAME_CTOI_R1N,     &stg_ret_n_info)
    ENT(stg_ctoi_F1,      BCO_FRAME_CTOI_F1,      &stg_ret_f_info)
    ENT(stg_ctoi_D1,      BCO_FRAME_CTOI_D1,      &stg_ret_d_info)
    ENT(stg_ctoi_L1,      BCO_FRAME_CTOI_L1,      &stg_ret_l_info)
    ENT(stg_ctoi_V,       BCO_FRAME_CTOI_V,       &stg_ret_v_info)
    ENT_T(0)  ENT_T(1)  ENT_T(2)  ENT_T(3)  ENT_T(4)
    ENT_T(5)  ENT_T(6)  ENT_T(7)  ENT_T(8)
#if FEW_TUPLE_FRAMES
    ENT_BCO(stg_ctoi_t,   BCO_FRAME_CTOI_T, &stg_ret_t_info, 3)
#else
    ENT_T(9)
    ENT_T(10) ENT_T(11) ENT_T(12) ENT_T(13) ENT_T(14)
    ENT_T(15) ENT_T(16) ENT_T(17) ENT_T(18) ENT_T(19)
    ENT_T(20) ENT_T(21) ENT_T(22) ENT_T(23) ENT_T(24)
    ENT_T(25) ENT_T(26) ENT_T(27) ENT_T(28) ENT_T(29)
    ENT_T(30) ENT_T(31) ENT_T(32) ENT_T(33) ENT_T(34)
    ENT_T(35) ENT_T(36) ENT_T(37) ENT_T(38) ENT_T(39)
    ENT_T(40) ENT_T(41) ENT_T(42) ENT_T(43) ENT_T(44)
    ENT_T(45) ENT_T(46) ENT_T(47) ENT_T(48) ENT_T(49)
    ENT_T(50) ENT_T(51) ENT_T(52) ENT_T(53) ENT_T(54)
    ENT_T(55) ENT_T(56) ENT_T(57) ENT_T(58) ENT_T(59)
    ENT_T(60) ENT_T(61) ENT_T(62)
#endif
};

#undef ENT_T
#undef ENT

#define BCO_FRAME_TABLE_LEN \
    (sizeof(bco_frame_table) / sizeof(bco_frame_table[0]))

// Checks the frames listed are actually RET_BCO and other consistency properties.
bool stack_bco_frame_selftest(void)
{
    bool flag = true;
    for (size_t i = 0; i < BCO_FRAME_TABLE_LEN; i++) {
        const StgInfoTable *it =
            INFO_PTR_TO_STRUCT((const StgInfoTable *)bco_frame_table[i].info);
        if (it->type != RET_BCO) {
            errorBelch("stack_bco_frame_selftest: %s has closure type %d, not RET_BCO\n",
                 bco_frame_table[i].name, (int)it->type);
            flag = false;
        }

        if (bco_frame_table[i].return_info == NULL && bco_frame_table[i].return_bco_offset != 0) {
            errorBelch("stack_bco_frame_selftest: return frame of %s is NULL, but offset is %d not 0\n",
                 bco_frame_table[i].name, (int)bco_frame_table[i].return_bco_offset);
                flag = false;

        } else if (bco_frame_table[i].return_info != NULL){
        const StgInfoTable *return_it =
               INFO_PTR_TO_STRUCT((const StgInfoTable *)bco_frame_table[i].return_info);

        if (return_it->type == RET_BCO &&
            bco_frame_table[i].return_bco_offset == 0) {
            errorBelch("stack_bco_frame_selftest: return frame of %s has closure type RET_BCO but offset 0\n",
                 bco_frame_table[i].name);
            flag = false;

        } else if (return_it->type != RET_BCO &&
                   bco_frame_table[i].return_bco_offset != 0) {
            errorBelch("stack_bco_frame_selftest: return frame of %s has closure type %d, not RET_BCO, but offset %d instead of 0\n",
                 bco_frame_table[i].name, (int)return_it->type,
                 (int)bco_frame_table[i].return_bco_offset);
            flag = false;
        }}
        for (size_t j = 0; j < i; j++) {
            if (bco_frame_table[i].info == bco_frame_table[j].info) {
                errorBelch("stack_bco_frame_selftest: %s and %s alias\n",
                     bco_frame_table[i].name, bco_frame_table[j].name);
                flag = false;
            }
        }
    }
    return flag;
}

// Returns BcoFrameEntry for given stack frame.
// In can be of the entries in bco_frame_table or one of the BCO_FRAME_NOT_BCO, BCO_FRAME_UNKNOWN kinds.
BcoFrameEntry stack_bco_frame_entry(const StgClosure *frame)
{
    const StgInfoTable *info = frame->header.info;

    if (get_itbl(frame)->type != RET_BCO) {
        return (BcoFrameEntry){NULL, BCO_FRAME_NOT_BCO, "not_bco", NULL, 0};
    }

    for (size_t i = 0; i < BCO_FRAME_TABLE_LEN; i++) {
        if ((const void *)info == bco_frame_table[i].info) {
            IF_DEBUG(sanity,debugBelch("%s\n",bco_frame_table[i].name););
            return bco_frame_table[i];
        }
    }
    return (BcoFrameEntry){NULL, BCO_FRAME_UNKNOWN, "unknown", NULL, 0};
}

// Returns offset to bco_args_0 in the expected AP_STACK closure,
// or -1 if the stack frame cannot be identified.
//
// See Note [Ask the RTS for memory layout of AP_STACK].
StgInt bco_frame_args_offset(StgClosure * frame)
{
    BcoFrameEntry frame_entry = stack_bco_frame_entry(frame);

    switch (frame_entry.kind){
        case BCO_FRAME_NOT_BCO:
        case BCO_FRAME_UNKNOWN:
            return -1;
        default:
            const void * return_frame_info = frame_entry.return_info;
            if (return_frame_info == NULL){
                // No return frame used (e.g. stg_apply_interp), so constant offset.
                return 2;
            } else {
                StgBCO * return_bco;

                if(frame_entry.return_bco_offset > 0){
                    return_bco = (StgBCO *)((StgPtr)frame)[frame_entry.return_bco_offset];
                }
                else{
                    return_bco = NULL;
                }
                // We create a mocked return frame `stub` that contains the right header.
                // The second word is only relevant if the return frame is RET_BCO too (i.e. stg_ret_t[N] ones), and will be ignored for the others.
                StgWord stub[2] = { (StgWord)return_frame_info, (StgWord)return_bco };
                return (StgInt)stack_frame_sizeW((StgClosure *)stub) + 2;
            }
    }

}


StgInt bco_args_offset(StgStack* stack, StgWord frame_index){
    IF_DEBUG(sanity,
        debugBelch("ENTERED bco_args_offset=%" FMT_Word "\n", index);
        stack_bco_frame_selftest();
    );
    StgPtr sp = stack->sp;
    StgPtr spBottom = stack->stack + stack->stack_size;

    StgWord i = 0;
    for (;i < frame_index && sp < spBottom;i++) {
        ASSERT(sp >= stack->stack && sp < spBottom);
        const StgInfoTable *info = get_itbl((StgClosure *)sp);
        IF_DEBUG(sanity,
            debugBelch("i=%" FMT_Word " sp=%p bottom=%p type=%d size=%lu\n",
            i, sp, spBottom, (int)info->type,
            (unsigned long)stack_frame_sizeW((StgClosure *)sp));
        );
        if (info->type == UNDERFLOW_FRAME) {
            stack = ((StgUnderflowFrame *)sp)->next_chunk;
            sp = stack->sp;
            spBottom = stack->stack + stack->stack_size;
            continue;        /* or restructure as a do/while */
        }
        if (info->type == STOP_FRAME) break;
        sp += stack_frame_sizeW((StgClosure *)sp);
    }

    if(i != frame_index){
        IF_DEBUG(sanity,debugBelch("bco_args_offset: bottomed out i=%" FMT_Word "index=%" FMT_Word  "\n",i,index););
        return -1;
    } else {
        IF_DEBUG(sanity,debugBelch("bco_args_offset: found frame\n"););
        return bco_frame_args_offset((StgClosure *)sp);
    }
}
