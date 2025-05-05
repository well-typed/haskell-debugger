import { DebugClient } from '@vscode/debugadapter-testsupport';
import * as cp from 'child_process';
import * as net from 'net';
// import assert = require('assert')

function getFreePort() {
	return new Promise((resolve, reject) => {
		const server = net.createServer();
		server.listen(0, () => {
			const address = server.address();
			if (typeof address === 'object' && address?.port) {
				const port = address.port;
				server.close(() => resolve(port));
			} else {
				reject(new Error('Could not get free port'));
			}
		});
		server.on('error', reject);
	});
}

var dc;
let debuggerProcess;

describe("Debug Adapter Tests", function () {
    this.timeout(20000); // 30s
    const cwd = process.cwd();

    beforeEach( () => getFreePort().then(port => {

        debuggerProcess = cp.spawn('ghc-debug-adapter', ['--port', port.toString()]);

        const ready = new Promise((resolve, reject) => {
            const timeout = setTimeout(() => {
                reject(new Error("ghc-debugger did not signal readiness in time"));
            }, 15000); // 15 second timeout

            debuggerProcess.stdout.on('data', data => {
                // console.log(data.toString())
                const text = data.toString();
                if (text.includes('Running')) {
                    clearTimeout(timeout);

                    // Just a little delay, 50ms
                    // Otherwise it may not yet be up as we only set up the
                    // server directly after the message.
                    // Alternatively, we could try-catch connect and retry a few times.
                    setTimeout(() => {
                        resolve();
                    }, 50)
                }
            });

            debuggerProcess.on('error', reject);
        });

        return ready.then(function () {
            dc = new DebugClient(undefined, undefined, 'haskell');
            return dc.start(port)
        })

    }));

    afterEach( () => {
      if (!debuggerProcess.killed)
        debuggerProcess.kill();
      dc.stop()
    });


    // Broken!!!
    // let defaultMainLaunch = {
    //         type: "ghc-debugger",
    //         request: "launch",
    //         name: "Haskell Debugger",
    //         projectRoot: "data",
    //         entryFile: "Main.hs",
    //         entryPoint: "main",
    //         entryArgs: ["some", "args"],
    //         extraGhcArgs: []
    //     }

    let defaultCabalLaunch = {
            type: "ghc-debugger",
            request: "launch",
            name: "Haskell Debugger",
            projectRoot: cwd + "/data/cabal1",
            entryFile: "app/Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
        }

    it('should run program to the end', () => {
        return Promise.all([
            dc.configurationSequence(),
            dc.launch(defaultCabalLaunch),
            dc.waitForEvent('exited').then(e => new Promise((resolve, reject) => {
                if (e.body.exitCode == 0)
                    resolve(e)
                else
                    reject(new Error("Expecting ExitCode 0"))
            }))
        ]);
    });

    it('should stop on a breakpoint', () => {
        return dc.hitBreakpoint(defaultCabalLaunch, { path: "app/Main.hs", line: 6 }, { path: defaultCabalLaunch.projectRoot + "/app/Main.hs", line: 6 }, { path: defaultCabalLaunch.projectRoot + "/app/Main.hs", line: 6 });
    });

})

// test('should stop on entry', () => {
//     return Promise.all([
//         dc.configurationSequence(),
//         dc.launch(defaultMainLaunch + {stopOnEntry: true}...),
//         dc.assertStoppedLocation('entry', 1)
//     ]);
// });

// import assert = require('assert');
// import * as Path from 'path';

// suite('Node Debug Adapter', () => {

// 	const DEBUG_ADAPTER = './out/debugAdapter.js';

// 	const PROJECT_ROOT = Path.join(__dirname, '../../');
// 	const DATA_ROOT = Path.join(PROJECT_ROOT, 'src/tests/data/');


// 	let dc: DebugClient;

// 	setup( () => {
// 		dc = new DebugClient('node', DEBUG_ADAPTER, 'mock');
// 		return dc.start();
// 	});

// 	teardown( () => dc.stop() );


// 	suite('basic', () => {

// 		test('unknown request should produce error', done => {
// 			dc.send('illegal_request').then(() => {
// 				done(new Error("does not report error on unknown request"));
// 			}).catch(() => {
// 				done();
// 			});
// 		});
// 	});

// 	suite('initialize', () => {

// 		test('should return supported features', () => {
// 			return dc.initializeRequest().then(response => {
// 				response.body = response.body || {};
// 				assert.equal(response.body.supportsConfigurationDoneRequest, true);
// 			});
// 		});

// 		test('should produce error for invalid \'pathFormat\'', done => {
// 			dc.initializeRequest({
// 				adapterID: 'mock',
// 				linesStartAt1: true,
// 				columnsStartAt1: true,
// 				pathFormat: 'url'
// 			}).then(response => {
// 				done(new Error("does not report error on invalid 'pathFormat' attribute"));
// 			}).catch(err => {
// 				// error expected
// 				done();
// 			});
// 		});
// 	});

// 	suite('launch', () => {

// 		test('should run program to the end', () => {

// 			const PROGRAM = Path.join(DATA_ROOT, 'test.md');

// 			return Promise.all([
// 				dc.configurationSequence(),
// 				dc.launch({ program: PROGRAM }),
// 				dc.waitForEvent('terminated')
// 			]);
// 		});

// 		test('should stop on entry', () => {

// 			const PROGRAM = Path.join(DATA_ROOT, 'test.md');
// 			const ENTRY_LINE = 1;

// 			return Promise.all([
// 				dc.configurationSequence(),
// 				dc.launch({ program: PROGRAM, stopOnEntry: true }),
// 				dc.assertStoppedLocation('entry', { line: ENTRY_LINE } )
// 			]);
// 		});
// 	});

// 	suite('setBreakpoints', () => {

// 		test('should stop on a breakpoint', () => {

// 			const PROGRAM = Path.join(DATA_ROOT, 'test.md');
// 			const BREAKPOINT_LINE = 2;

// 			return dc.hitBreakpoint({ program: PROGRAM }, { path: PROGRAM, line: BREAKPOINT_LINE } );
// 		});

// 		test('hitting a lazy breakpoint should send a breakpoint event', () => {

// 			const PROGRAM = Path.join(DATA_ROOT, 'testLazyBreakpoint.md');
// 			const BREAKPOINT_LINE = 3;

// 			return Promise.all([

// 				dc.hitBreakpoint({ program: PROGRAM }, { path: PROGRAM, line: BREAKPOINT_LINE, verified: false } ),

// 				dc.waitForEvent('breakpoint').then(event => {
// 					const bpevent = event as DebugProtocol.BreakpointEvent;
// 					assert.strictEqual(bpevent.body.breakpoint.verified, true, "event mismatch: verified");
// 				})
// 			]);
// 		});
// 	});

// 	suite('setExceptionBreakpoints', () => {

// 		test('should stop on an exception', () => {

// 			const PROGRAM_WITH_EXCEPTION = Path.join(DATA_ROOT, 'testWithException.md');
// 			const EXCEPTION_LINE = 4;

// 			return Promise.all([

// 				dc.waitForEvent('initialized').then(event => {
// 					return dc.setExceptionBreakpointsRequest({
// 						filters: [ 'otherExceptions' ]
// 					});
// 				}).then(response => {
// 					return dc.configurationDoneRequest();
// 				}),

// 				dc.launch({ program: PROGRAM_WITH_EXCEPTION }),

// 				dc.assertStoppedLocation('exception', { line: EXCEPTION_LINE } )
// 			]);
// 		});
// 	});
// });
