import { DebugClient } from '@vscode/debugadapter-testsupport';
import * as cp from 'child_process';
import * as net from 'net';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { mkdtempSync, cpSync, realpathSync } from 'node:fs';
import assert from 'assert';
import { describe, beforeEach, afterEach, it } from 'mocha';

function getFreePort(): Promise<number> {
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

var dc: DebugClient;
let debuggerProcess;

describe("Debug Adapter Tests", function () {
    this.timeout(15000); // 15s
    const cwd = process.cwd();
    const ghc_version = cp.execSync('ghc --numeric-version').toString().trim()

    beforeEach( () => getFreePort().then(port => {
        const cacheDir = mkHermeticCacheDir();
        const hdbEnv = {
            env: {
                ...process.env, HDB_CACHE_DIR: cacheDir
            }
        };
        debuggerProcess = cp.spawn('hdb', ['server', '--port', port.toString()], hdbEnv);

        const ready: Promise<void> = new Promise((resolve, reject) => {
            const timeout = setTimeout(() => {
                reject(new Error("haskell-debugger did not signal readiness in time"));
            }, 15000); // 15 second timeout

            debuggerProcess.stdout.on('data', data => {
                // NOTE: UNCOMMENT ME TO DEBUG
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

    const mkHermetic = (path: string) => {
        const tmp = mkdtempSync(join(tmpdir(), "hdb-")) + path
        const data = process.cwd() + path;
        cpSync(data, tmp, { recursive: true }) // Copy data contents to temp directory

        // Copy dependency too for now (until it's available on hackage)
        const hsDebugView = process.cwd() + "/../../haskell-debugger-view";
        cpSync(hsDebugView, tmp + "/haskell-debugger-view", { recursive: true }) // Copy data contents to temp directory
        return realpathSync(tmp)
    }

    const mkHermeticCacheDir = () => {
        const tmp = mkdtempSync(join(tmpdir(), "hdb-cache-"))
        return realpathSync(tmp)
    }

    const mkConfig = config => {

        // Run tests on the temporary directory. This avoids issues with
        // hie-bios finding bad project roots because of cabal.projects in the
        // file system.
        const tmp = mkHermetic(config.projectRoot)
        config.projectRoot = tmp
        return config
    }

    const simpleLaunchConfigs = [
        { name: "Vanilla config (no package)",
          config: mkConfig({
            projectRoot: "/data/simple",
            entryFile: "Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          })
        },
        { name: "Cabal config",
          config : mkConfig({
            projectRoot: "/data/cabal1",
            entryFile: "app/Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          })
        }
        // todo: Stack config?
        // todo: hie.yaml config?
    ]

    const basicTests = (launchCfg) => {

        describe(launchCfg.name, function () {

            it('should run program to the end', () => {
                return Promise.all([
                    dc.configurationSequence(),
                    dc.launch(launchCfg.config),
                    dc.waitForEvent('exited').then(e => new Promise((resolve, reject) => {
                        if (e.body.exitCode == 0)
                            resolve(e)
                        else
                            reject(new Error("Expecting ExitCode 0"))
                    }))
                ]);
            });

            it('should stop on a breakpoint', () => {
                const expected = { path: launchCfg.config.projectRoot + "/" + launchCfg.config.entryFile, line: 6 }
                return dc.hitBreakpoint(launchCfg.config, { path: launchCfg.config.entryFile, line: 6 }, expected, expected);
            });

            it('should stop on an exception', () => {

                // const expected = { path: launchCfg.config.projectRoot + "/" + launchCfg.config.entryFile, line: 10 }
                // Currently, no information is provided when stopped at an exception:
                const expected = { }
                return Promise.all([

                    dc.waitForEvent('initialized').then(event => {
                        return dc.setExceptionBreakpointsRequest({
                            filters: [ 'break-on-exception' ]
                        });
                    }).then(response => {
                        return dc.configurationDoneRequest();
                    }),

                    dc.launch(launchCfg.config),

                    dc.assertStoppedLocation('exception', expected)
                ]);
            });
        })
    }

    const fetchLocalVars = async () => {
        return fetchScopeVars("Locals")
    }

    const fetchModuleVars = async () => {
        return fetchScopeVars("Module")
    }

    const fetchScopeVars = async (scopeName) => {
        const stResp = await dc.stackTraceRequest({ threadId: 0 });
        const sf0 = stResp.body.stackFrames[0];
        const scResp = await dc.scopesRequest({ frameId: sf0.id });
        const someScope = scResp.body.scopes.find(scope => scope.name == scopeName)!!;
        const variablesResp = await dc.variablesRequest({ variablesReference: someScope.variablesReference });
        const variables = variablesResp.body.variables;
        return {
            all: variables,
            get: function (name) {
                const r = variables.find(v => v.name == name);
                assert(r, `Variable ${name} not found`);
                return r;
            }
        };
    }

    const forceLazy = async (v) => {
        assert.strictEqual(v.presentationHint.lazy, true, `Variable ${v.name} should be lazy`);
        assert.strictEqual(v.value, '_', `Variable ${v.name} should be "_" because it is lazy`);
        assert.notStrictEqual(v.variablesReference, 0, `Variable ${v.name} should be expandable (because it is lazy)`);

        // Force a lazy variable
        const forceResp = await dc.variablesRequest({ variablesReference: v.variablesReference });
        const forcedVar = forceResp.body.variables[0]
        return forcedVar
    }

    const expandVar = async (v : {variablesReference: number, name: string}) => {
        assert.notStrictEqual(v.variablesReference, 0, `Variable ${v.name} should be expandable (because it is a structure)`);

        // Expand a structure (similarly to forcing a lazy variable, but because it is not lazy it will fetch the fields)
        const childrenResp = await dc.variablesRequest({ variablesReference: v.variablesReference });
        const children = childrenResp.body.variables;
        return {
            all: children,
            get: function (name) {
                const r = children.find(v => v.name == name);
                assert(r, `Variable ${name} not found`);
                return r;
            }
        };
    }

    const assertIsString = (v, expected) => {
        assert.strictEqual(v.type, "String", `Variable ${v.name} should be a String`);
        assert.strictEqual(v.value, expected, `Variable ${v.name} should be "${expected}"`);
        assert.strictEqual(v.variablesReference, 0, `Variable ${v.name} should not be expandable (because it is a String)`);
    }

    // The most basic functionality we test on various different
    // configurations (such as Cabal vs without project vs Stack)
    // The remaining tests are run only on one config since the set up
    // starts being more specific (e.g. multiple home units with Cabal)
    describe("Most basic functionality", function () {
        simpleLaunchConfigs.forEach(basicTests);

        describe("Other basic tests", function () {
            it('report error on missing "entryFile"', () => {
                let config = mkConfig({
                      projectRoot: "/data/T71",
                    })

                const expected = { path: config.projectRoot + "/" + config.entryFile, line: 1 }
                // 'Launch' the debugger using this config and expect the response to the launch request to be a ResponseError with message "Missing \"entryFile\""
                return dc.launch(config).then(
                        () => Promise.reject(new Error("Expecting launch to fail")),
                        (err) => {
                            assert.strictEqual(err.message, 'Missing "entryFile" key in debugger configuration');
                        }
                    )
            })

            it('minimal configuration with just entryFile', async () => {
                let config = mkConfig({
                      projectRoot: "/data/T71",
                      entryFile: "Main.hs",
                    })

                const expected = { path: config.projectRoot + "/" + config.entryFile, line: 2 }
                return dc.hitBreakpoint(config, { path: config.entryFile, line: 2 }, expected, expected)
            })
        })

    })

    describe("Multiple main function tests", function () {
        const multiMainConfig = mkConfig({
            projectRoot: "/data/multi-mains",
            entryFile: "app/Main.hs",
            entryPoint: "main",
            entryArgs: [],
            extraGhcArgs: [],
        });

        it("should run program to the end", () => {
            return Promise.all([
                dc.configurationSequence(),
                dc.launch(multiMainConfig),
                dc.waitForEvent("exited").then(
                    (e) =>
                    new Promise((resolve, reject) => {
                        if (e.body.exitCode == 0) resolve(e);
                        else reject(new Error("Expecting ExitCode 0"));
                    })
                ),
            ]);
        });
        it("should stop at break-point", () => {
            const expected = { path: multiMainConfig.projectRoot + "/" + multiMainConfig.entryFile, line: 6 };
            return dc.hitBreakpoint(multiMainConfig, { path: multiMainConfig.entryFile, line: 6 }, expected, expected);
        });

        it("should run the right main", async () => {

            const multiMainConfig = mkConfig({
                projectRoot: "/data/multi-mains",
                entryFile: "second/Main.hs",
                entryPoint: "main",
                entryArgs: [],
                extraGhcArgs: [],
            });

            let outputEvents = [];
            dc.on("output", (event) => {
              outputEvents.push(event.body);
            });

            return Promise.all([
                dc.configurationSequence(),
                dc.launch(multiMainConfig),
                dc.waitForEvent("exited").then(
                    (e) => {
                        const allOutput = outputEvents.map((o) => o.output).join("");
                        assert.ok(allOutput.includes("The right main"), "Expecting the right Main");
                        assert.ok(!allOutput.includes("The bad main"), "Not expecting the bad main");
                        return new Promise((resolve, reject) => {
                            if (e.body.exitCode == 0) {
                                resolve(e);
                            }
                            else reject(new Error("Expecting ExitCode 0"));
                        });
                    })
                ]);
        });
    });

    describe("Multiple home unit tests", function () {
      const mhuConfig = mkConfig({
        projectRoot: "/data/cabal-mhu1",
        entryFile: "bar/app/Main.hs",
        entryPoint: "main",
        entryArgs: [],
        extraGhcArgs: [],
      });

      it("should run program to the end", () => {
        return Promise.all([
          dc.configurationSequence(),
          dc.launch(mhuConfig),
          dc.waitForEvent("exited").then(
            (e) =>
              new Promise((resolve, reject) => {
                if (e.body.exitCode == 0) resolve(e);
                else reject(new Error("Expecting ExitCode 1"));
              })
          ),
        ]);
      });

      it("should stop at break-point in the same home unit", () => {
        const expected = { path: mhuConfig.projectRoot + "/" + mhuConfig.entryFile, line: 8 };
        return dc.hitBreakpoint(mhuConfig, { path: mhuConfig.entryFile, line: 8 }, expected, expected);
      });

      it("should stop at break-point in different home unit 1", () => {
        const path = mhuConfig.projectRoot + "/./bar/src/Bar.hs"
        const expected = { path: path, line: 8 };
        return dc.hitBreakpoint(mhuConfig, { path: path, line: 8 }, expected, expected);
      });

      it("should stop at break-point in different home unit 2", () => {
        const path = mhuConfig.projectRoot + "/./foo/src/Foo.hs"
        const expected = { path: path, line: 6 };
        return dc.hitBreakpoint(mhuConfig, { path: path, line: 6 }, expected, expected);
      });
    });
    describe("Variable inspection tests", function () {

        it('ints and strings should be displayed as values', async () => {

            let config = mkConfig({
                  projectRoot: "/data/cabal1",
                  entryFile: "app/Main.hs",
                  entryPoint: "main",
                  entryArgs: ["some", "args"],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 15 }

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 15 }, expected, expected);

            const variables = await fetchLocalVars()

            // Int variables are displayed as ints
            const aVar = variables.get('a');
            const bVar = variables.get('b');

            assert.strictEqual(aVar.value, '2');
            assert.strictEqual(bVar.value, '4');

            // Force lazy variable 'c'
            // Strings are forced and displayed whole rather than as a structure
            const cVar = variables.get('c');
            const cVarForced = await forceLazy(cVar);

            assertIsString(cVarForced, '"call_fxxx"');

            // After a variable is forced, a new locals request is done. Check again for c == call_fxxx afterwards
            const refreshedVars = await fetchLocalVars();
            const refreshedCVar = refreshedVars.get('c');

            assertIsString(refreshedCVar, '"call_fxxx"')
        })

        it('allow arbitrarily deep inspection and strings are displayed as values arbitrarily deep when forced (issue #8 and #9)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/simple2",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })


            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 19 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 19 }, expected, expected);

            // get the locals
            let locals = await fetchLocalVars();
            const pVar = await forceLazy(locals.get('p'));
            const pChild = await expandVar(pVar);
            const _1Var = await forceLazy(pChild.get('_1'));
            assertIsString(_1Var, '"d=1"');

            let focus = pChild.get('_2');

            // Now walk the spine from d=2 through d=6
            for (let expectedLevel = 2; expectedLevel <= 6; expectedLevel++) {
                const focusF = await forceLazy(focus);
                const children = await expandVar(focusF);
                const dChild = await forceLazy(children.get('_1'));
                assertIsString(dChild, `"d=${expectedLevel}"`);

                focus = children.get('_2');
            }

            // Finally, we should be at the OK constructor
            assert.strictEqual(focus.value, 'OK');
        })

        it('strings that are fields of the expanded vars and are not thunks are fully evaluated (issue #11)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/repeat",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 5 }

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 5 }, expected, expected)

            // Force only the 2nd "hello" and check the third is already there.
            // (Mimics reproducer in #11)
            let locals = await fetchLocalVars();
            const xVar = await forceLazy(locals.get('x'));
            const xChild = await expandVar(xVar);
            const _2Var = await xChild.get('_2'); // NOTE: Doesn't need to be forced because of this seemingly weird `repeat` behavior where it looks like every other binding is shared but the others are not
            const _2Child = await expandVar(_2Var);
            const _2_1Var = await forceLazy(_2Child.get('_1'));
            const _2_2Var = await _2Child.get('_2');
            const _2_2Child = await expandVar(_2_2Var);
            const _2_2_1Var = await _2_2Child.get('_1') // NOTE: doesn't need to be forced as above
            assertIsString(_2_2_1Var, '"hello"');
        })

        it('labeled data structures can be expanded (issue #18)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/labeled",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 8 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 8 }, expected, expected);

            // Force only the 2nd "hello" and check the third is already there.
            // It relies on repeat seemingly only re-using every other thunk?!!?
            // (Mimics reproducer in #11)
            let locals = await fetchLocalVars();
            const xVar = await forceLazy(locals.get('x'));
            const xChild = await expandVar(xVar);
            const s_newVar = await xChild.get('new'); // No force
            assert.strictEqual(s_newVar.value, '3456');
            const s_labVar = await forceLazy(xChild.get("lab"));
            assertIsString(s_labVar, '"label"');
        })

        it('newtype vars not broken (issue #55)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T55",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 40 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 40 }, expected, expected);

            let locals = await fetchLocalVars();
            const y2Var = await forceLazy(locals.get('y2'));
            assert.strictEqual(y2Var.value, "Y2 (MyIntX (X _))")
            const y2Child = await expandVar(y2Var);
            const y2_1_Var = await y2Child.get('_1'); // No force
            assert.strictEqual(y2_1_Var.value, 'MyIntX (X _)');
            const y2_1_Child = await expandVar(y2_1_Var);
            const y2_1_1_Var = await y2_1_Child.get("_1");
            assert.strictEqual(y2_1_1_Var.value, 'X');
            const y2_1_1_Child = await expandVar(y2_1_1_Var);
            const y2_1_1_1_Var = await forceLazy(y2_1_1_Child.get("_1"));
            assert.strictEqual(y2_1_1_1_Var.value, 'MyInt 42');
        })

        it('dont crash on inspect newtype con (issue #64)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T64",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 4 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 4 }, expected, expected);

            let moduleVars = await fetchModuleVars();
            const myintCon = await moduleVars.get("MyIntCon")
            assert.strictEqual(myintCon.value, "Data constructor ‘MyIntCon’")
        })

        it('expand iteratively s.t. forced structure appears as forced (issue #97)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T97",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 12 }

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 12 }, expected, expected)

            let locals = await fetchLocalVars();
            const xVar = await locals.get('y');
            // NOTE: This tests that we don't have to force a single variable
            // as we expand because the data should be fully forced at this
            // point.
            const xChild = await expandVar(xVar);
            const _1Var = await xChild.get('_1');
            const _1Child = await expandVar(_1Var);
            const _1_1Var = await _1Child.get('_1');
            assert.strictEqual(_1_1Var.value, 'T97'); // just check
            const _1_1Child = await expandVar(_1_1Var);
            const _1_1_1Var = await _1_1Child.get('_1');
            const _1_1_1Child = await expandVar(_1_1_1Var);
            const _1_1_1_1Var = await _1_1_1Child.get('_1');
            const _1_1_1_1Child = await expandVar(_1_1_1_1Var);
            const _1_1_1_1_1Var = await _1_1_1_1Child.get('_1');
            const _1_1_1_1_1Child = await expandVar(_1_1_1_1_1Var);
            const _1_1_1_1_1_1Var = await _1_1_1_1_1Child.get('_1');
            assertIsString(_1_1_1_1_1_1Var, '"hello"');
        })

        it('inspect mutable variables (#92)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T92",
                  entryFile: "T92.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 7 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 7 }, expected, expected);

            async function getMutVarValue(val) {
                let locals = await fetchLocalVars();
                const rVar = await locals.get('r');
                assert.strictEqual(rVar.value, "IORef (STRef (GHC.Prim.MutVar# _))")
                const rChild = await expandVar(rVar);
                const r_1_Var = await rChild.get('_1'); // No force
                assert.strictEqual(r_1_Var.value, 'STRef');
                const r_1_Child = await expandVar(r_1_Var);
                const r_1_1_Var = await r_1_Child.get("_1"); // No force
                assert.strictEqual(r_1_1_Var.value, 'GHC.Prim.MutVar# '+val);
                const r_1_1_Child = await expandVar(r_1_1_Var);
                const r_1_1_1_Var = await r_1_1_Child.get("_1");
                return r_1_1_1_Var
            }

            const m1 = await getMutVarValue("False")
            assert.strictEqual(m1.value, "False")
            await dc.nextRequest({ threadId: 0 });
            await dc.nextRequest({ threadId: 0 });
            await dc.nextRequest({ threadId: 0 });
            // Now we're at the start of the last line, where the ref should be True
            // Note how we get it from scratch, the content of the ref must be
            // forced again (the forceLazy call in getMutVarValue)
            const m2 = await getMutVarValue("True")
            assert.strictEqual(m2.value, "True")
        })

        it('inspect fully evaluated type (issue #110)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T110",
                  entryFile: "T110.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 11 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 11 }, expected, expected);

            let locals = await fetchLocalVars();
            const tVar = await forceLazy(locals.get('t'));
            assert.strictEqual(tVar.value, "T")
            const tChild = await expandVar(tVar); // No force needed, is fully evaluated and can expand
            const _1Var = await tChild.get('_1');
            const _2Var = await tChild.get('_2');
            assert.strictEqual(_1Var.value, '333');
            assert.strictEqual(_2Var.value, '34');
        })

        it('user-defined custom instance (issue #47)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T47a",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 26 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 26 }, expected, expected);

            let locals = await fetchLocalVars();
            const tVar = await forceLazy(locals.get('action'));
            assert.strictEqual(tVar.value, "SDJFLSKDJFLKSDJFLSJDKFL")
            const tChild = await expandVar(tVar);
            const _1Var = await forceLazy(tChild.get('field1'));
            assert.strictEqual(_1Var.value, '"A33"');
            const _2Var = await forceLazy(tChild.get('myfield2'));
            assert.strictEqual(_2Var.value, '3');
            const _3Var = await forceLazy(tChild.get('field3'));
            assert.strictEqual(_3Var.value, 'Y');
            const _3Child = await expandVar(_3Var);
            const _3_1Var = await forceLazy(_3Child.get("_1"))
            assert.strictEqual(_3_1Var.value, '"7"');
            const _4Var = await tChild.get('field4');
            assert.strictEqual(_4Var.value, '2345');
            const _5Var = await tChild.get('field5');
            assert.strictEqual(_5Var.value, '2345.0');
        })

        it('built-in custom instance without haskell-debugger-view dependency (issue #47)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T47b",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 11 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 11 }, expected, expected);

            let locals = await fetchLocalVars();
            const tVar = await forceLazy(locals.get('action'));
            assert.strictEqual(tVar.value, "X")
            const tChild = await expandVar(tVar);
            const _1Var = await tChild.get('_1');
            assert.strictEqual(_1Var.value, '( , )');
            const _1Child = await expandVar(_1Var);
            const _2Var = await forceLazy(_1Child.get('fst'));
            assert.strictEqual(_2Var.value, '"A33"');
            const _3Var = await _1Child.get('snd');
            assert.strictEqual(_3Var.value, '3456.0');
        })

        it('hdv dependency with containers module (issue #47)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T47c",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 13 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 13 }, expected, expected);

            // Check IntMap custom view
            let locals = await fetchLocalVars();
            const tVar = await forceLazy(locals.get('action'));
            assert.strictEqual(tVar.value, "IntMap")
            const tChild = await expandVar(tVar);
            const _1Var = await forceLazy(tChild.get('3'));
            assert.strictEqual(_1Var.value, '"one"');
            const _2Var = await forceLazy(tChild.get('2'));
            assert.strictEqual(_2Var.value, '"two"');
        })

        it('hdv in-memory with containers module (issue #47)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T47d",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 11 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 11 }, expected, expected);

            // Check IntMap custom view
            let locals = await fetchLocalVars();
            const tVar = await forceLazy(locals.get('action'));
            assert.strictEqual(tVar.value, "IntMap")
            const tChild = await expandVar(tVar);
            const _1Var = await forceLazy(tChild.get('3'));
            assert.strictEqual(_1Var.value, '"one"');
            const _2Var = await forceLazy(tChild.get('2'));
            assert.strictEqual(_2Var.value, '"two"');
        })

        // Not needed! Tested in multiple places that this still works.
        // it('hdv in-memory without containers module (issue #47)', async () => {
        // ...
        // })

        it('hdv in-memory with text module (issue #47)', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T47e",
                  entryFile: "Main.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 11 }
            await dc.hitBreakpoint(config, { path: config.entryFile, line: 11 }, expected, expected);

            // Check IntMap custom view
            let locals = await fetchLocalVars();
            const tVar = await forceLazy(locals.get('action'));
            assert.strictEqual(tVar.value, "\"this should be displayed as a simple string\"")
        })
    })
    describe("Stepping out (step-out)", function () {

        it('simple step-out to case', async () => {
            let config = mkConfig({
                  projectRoot: "/data/T6",
                  entryFile: "MainC.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: []
                })

            const expected = (line) => ({ path: config.projectRoot + "/" + config.entryFile, line: line });

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 9 }, expected(9), expected(9));

            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(5));
        })

        let need_opt = true // Currently we depend on this to work around the fact that >>= is in library code because base is not being interpreted

        // Mimics GHC's T26042b
        it('without tail calls', async () => {

            let config = mkConfig({
                  projectRoot: "/data/T6",
                  entryFile: "MainA.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: need_opt ? ["-O", "-fno-unoptimized-core-for-interpreter"] : []
                })

            const expected = (line) => ({ path: config.projectRoot + "/" + config.entryFile, line: line });

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 10 }, expected(10), expected(10));

            // foo to bar
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(20));

            // bar back to foo
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(14));

            // back to main
            await dc.stepOutRequest({threadId: 0});
            await dc.assertStoppedLocation('step', expected(5));

            // exit
            await dc.stepOutRequest({threadId: 0});
        })

        // Mimics GHC's T26042c
        it('with tail calls', async () => {

            let config = mkConfig({
                  projectRoot: "/data/T6",
                  entryFile: "MainB.hs",
                  entryPoint: "main",
                  entryArgs: [],
                  extraGhcArgs: need_opt ? ["-O", "-fno-unoptimized-core-for-interpreter"] : []
                })

            const expected = (line) => ({ path: config.projectRoot + "/" + config.entryFile, line: line });

            await dc.hitBreakpoint(config, { path: config.entryFile, line: 10 }, expected(10), expected(10));

            // step out of foo True and observe that we have skipped its call in bar,
            // and the call of bar in foo False.
            // we go straight to `main`.
            await dc.stepOutRequest({threadId: 0});

            await dc.assertStoppedLocation('step', expected(5))

            // stepping out again exits
            await dc.stepOutRequest({threadId: 0});

        })
    })
    describe("Stdout tests", function () {
      it("basic output and stderr", async () => {

        let config = mkConfig({
              projectRoot: "/data/T45",
              entryFile: "Main.hs",
              entryPoint: "main",
              entryArgs: [], // no args
              extraGhcArgs: []
            })

        const expected = (line) => ({ path: config.projectRoot + "/" + config.entryFile, line: line });

        // Capture output events
        let outputEvents = [];
        dc.on("output", (event) => {
          outputEvents.push(event.body);
        });

        // Hit breakpoint at line 3 before program runs through
        await dc.hitBreakpoint(config, { path: config.entryFile, line: 3 }, expected(3), expected(3));

        // Print line
        await dc.nextRequest({ threadId: 0 });

        // Reach crash line
        await dc.nextRequest({ threadId: 0 });

        // Crash it
        await dc.nextRequest({ threadId: 0 });

        await dc.waitForEvent("terminated");

        // Now assert the collected output
        const allOutput = outputEvents.map((o) => o.output).join("");

        // stdout should include the first message
        assert.ok(
          allOutput.includes("Going to read args"),
          "Expected stdout message not found"
        );

        // stderr should include an error from getArgs
        const stderrEvents = outputEvents.filter((e) => e.category === "stderr");
        const stderrText = stderrEvents.map((e) => e.output).join("");

        assert.ok(
          stderrText.includes("Uncaught exception"),
          "Expected stderr output from getArgs failure"
        );

      })
    })

    describe("Conditional breakpoints (issue #111)", function () {
      it("Conditional expression breakpoints", async () => {
        let config = mkConfig({
              projectRoot: "/data/T111",
              entryFile: "T111.hs",
              entryPoint: "main",
              entryArgs: [],
              extraGhcArgs: []
            })

        const expected = { path: config.projectRoot + "/" + config.entryFile, line: 13 }

        await Promise.all([
            dc.waitForEvent('initialized').then(event => {
                return dc.setBreakpointsRequest({
                  lines: [ 13 ],
                  breakpoints: [ { line: 13, condition: "im IM.! 0 == 2" } ],
                  source: { path: config.entryFile }
                });
            }).then(response => {
                return dc.configurationDoneRequest();
            }),

            dc.launch(config),

            dc.assertStoppedLocation('breakpoint', expected)
        ]);

        // Assert we only stopped when im IM.! 0 == 2 (and not earlier)
        const variables = await fetchLocalVars()
        const imVar = variables.get('im');

        assert.strictEqual(imVar.value, 'IntMap');
        const imChild = await expandVar(imVar);
        const _2Var = await imChild.get("0");
        const _3Var = await imChild.get("2");
        assert.strictEqual(_2Var.value, '2');
        assert.strictEqual(_3Var.value, '4');

        // And doesn't stop again
        await dc.continueRequest({threadId: 0, singleThread: false});

        await dc.waitForEvent('exited').then(e => new Promise((resolve, reject) => {
            if (e.body.exitCode == 0)
                resolve(e)
            else
                reject(new Error("Expecting ExitCode 0"))
        }))
      })

      it("Hit count breakpoints", async () => {
        let config = mkConfig({
              projectRoot: "/data/T111",
              entryFile: "T111.hs",
              entryPoint: "main",
              entryArgs: [],
              extraGhcArgs: []
            })

        const expected = { path: config.projectRoot + "/" + config.entryFile, line: 13 }
        const ignoreCount = 2 // ignore 2 hits and stop at third

        await Promise.all([
            dc.waitForEvent('initialized').then(event => {
                return dc.setBreakpointsRequest({
                  lines: [ 13 ],
                  breakpoints: [ { line: 13, hitCondition: `${ignoreCount}` } ],
                  source: { path: config.entryFile }
                });
            }).then(response => {
                return dc.configurationDoneRequest();
            }),

            dc.launch(config),

            dc.assertStoppedLocation('breakpoint', expected)
        ]);

        // Assert we only stopped at the 3rd hit
        const variables = await fetchLocalVars()
        const imVar = await forceLazy(variables.get('im'));

        assert.strictEqual(imVar.value, 'IntMap');
        const imChild = await expandVar(imVar);
        const _2Var = await imChild.get("0");
        const _3Var = await imChild.get("2");
        assert.strictEqual(_2Var.value, '2');
        assert.strictEqual(_3Var.value, '4');

        // Unlike conditional expression, we hit the breakpoint every time
        // after the ignore count, so run this twice
        await dc.continueRequest({threadId: 0, singleThread: false});
        await dc.continueRequest({threadId: 0, singleThread: false});

        await dc.waitForEvent('exited').then(e => new Promise((resolve, reject) => {
            if (e.body.exitCode == 0)
                resolve(e)
            else
                reject(new Error("Expecting ExitCode 0"))
        }))
      })
    })
    describe("Evaluate", function () {
      it("Return structured representation for evaluated expressions (issue #116)", async () => {
        let config = mkConfig({
              projectRoot: "/data/T116",
              entryFile: "T116.hs",
              entryPoint: "main",
              entryArgs: [],
              extraGhcArgs: []
            })

        const expected = { path: config.projectRoot + "/" + config.entryFile, line: 13 }

        await dc.hitBreakpoint(config, { path: config.entryFile, line: 13 }, expected, expected);

        let resp = await dc.evaluateRequest({expression: "IM.delete 0 (IM.insert 0 'a' (IM.insert 1 'b' IM.empty))"} )

        assert.strictEqual(resp.body.result, 'IntMap');
        const respChild = await expandVar({...resp.body, name: resp.body.result})
        const _1Var = await respChild.get("1")
        assert.strictEqual(_1Var.value, '\'b\'');
      })
    })
})

