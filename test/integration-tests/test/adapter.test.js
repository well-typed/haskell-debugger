import { DebugClient } from '@vscode/debugadapter-testsupport';
import * as cp from 'child_process';
import * as net from 'net';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { mkdtempSync, cpSync } from 'node:fs';
import assert from 'assert';

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
    this.timeout(5000); // 5s
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

    const simpleLaunchConfigs = [
        { name: "Vanilla config (no package)",
          config: {
            projectRoot: "/data/simple",
            entryFile: "Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          }
        },
        { name: "Cabal config",
          config : {
            projectRoot: "/data/cabal1",
            entryFile: "app/Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          }
        }
        // todo: Stack config?
        // todo: hie.yaml config?
    ]

    const mkHermetic = (path) => {
        const tmp = mkdtempSync(join(tmpdir(), "ghc-debugger-")) + path
        const data = process.cwd() + path;
        cpSync(data, tmp, { recursive: true }) // Copy data contents to temp directory
        return tmp
    }

    const basicTests = (launchCfg) => {

        // Run tests on the temporary directory. This avoids issues with
        // hie-bios finding bad project roots because of cabal.projects in the
        // file system.
        const tmp = mkHermetic(launchCfg.config.projectRoot)
        launchCfg.config.projectRoot = tmp;

        // The most basic functionality we test on various different
        // configurations (such as Cabal vs without project vs Stack)
        // The remaining tests are run only on one config since the set up
        // starts being more specific (e.g. multiple home units with Cabal)
        describe("Most basic functionality", function () {

          describe(launchCfg.name, function () {

              it('should run program to the end', () => {
                  return Promise.all([
                      dc.configurationSequence(),
                      dc.launch(launchCfg.config),
                      dc.waitForEvent('exited').then(e => new Promise((resolve, reject) => {
                          if (e.body.exitCode == 0)
                              resolve(e)
                          else
                              reject(new Error("Expecting ExitCode 1"))
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

              // TODO: Break on a different module

              // TODO: Step in, step next, ...
              // TODO: Resume and hit next breakpoint
              // ...
          })
        })
    }

    simpleLaunchConfigs.forEach(basicTests);

    describe("Variable inspection tests", function () {

        it('ints and strings should be displayed as values', () => {

            let config = {
                  projectRoot: "/data/cabal1",
                  entryFile: "app/Main.hs",
                  entryPoint: "main",
                  entryArgs: ["some", "args"],
                  extraGhcArgs: []
                }

            const tmp = mkHermetic(config.projectRoot)
            config.projectRoot = tmp;

            const expected = { path: config.projectRoot + "/" + config.entryFile, line: 15 }

            return Promise.all([
                dc.configurationSequence(),
                dc.launch(config),
                dc.hitBreakpoint(config, { path: config.entryFile, line: 15 }, expected, expected).then(() => {
                    return dc.stackTraceRequest({ threadId: 0 })
                }).then(stResp => {
                    let sf0 = stResp.body.stackFrames[0]
                    return dc.scopesRequest({ frameId: sf0.id })
                }).then(scResp => {
                    let localsScope = scResp.body.scopes.find(scope => scope.name == "Locals");
                    return dc.variablesRequest({ variablesReference: localsScope.variablesReference }).then(variablesResp => {
                        let variables = variablesResp.body.variables;

                        // Int variables are displayed as ints
                        const aVar = variables.find(v => v.name == 'a');
                        const bVar = variables.find(v => v.name == 'b');

                        // Strings are forced and displayed whole rather than as a structure
                        const cVar = variables.find(v => v.name == 'c');

                        assert(aVar.value == '2', `Expected a to be 2, got ${aVar.value}`);
                        assert(bVar.value == '4', `Expected b to be 4, got ${bVar.value}`);

                        // Force lazy variable 'c'
                        return dc.variablesRequest({ variablesReference: cVar.variablesReference });
                    }).then(cResp => {
                        const cVar = cResp.body.variables[0];
                        assert(cVar.value == '"call_fxxx"', `Expected c to be 'call_fxxx', got ${cVar.value}`);
                        assert(cVar.variablesReference == 0, `Because c is a string (boring type), it shouldn't be expandable`);

                        // After a variable is forced, a new locals request is done. Check again for c == call_fxxx afterwards
                        return dc.variablesRequest({ variablesReference: localsScope.variablesReference })
                    }).then(variablesResp => {
                        const cVar = variablesResp.body.variables.find(v => v.name == 'c');
                        assert(cVar.value == '"call_fxxx"', `Expected c to be 'call_fxxx' after refreshing the local scope, got ${cVar.value}`);
                        assert(cVar.variablesReference == 0, `Because c is a string (boring type), it shouldn't be expandable after refreshing the local scope`);
                    })
                })
              ])
        })
    })
})

