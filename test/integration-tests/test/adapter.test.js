import { DebugClient } from '@vscode/debugadapter-testsupport';
import * as cp from 'child_process';
import * as net from 'net';

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
                console.log(data.toString())
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
        { name: "Vanilla config (single file)",
          config: {
            type: "ghc-debugger",
            request: "launch",
            name: "Haskell Debugger",
            projectRoot: "data",
            entryFile: "Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          }
        },
        { name: "Cabal config",
          config : {
            type: "ghc-debugger",
            request: "launch",
            name: "Haskell Debugger",
            projectRoot: cwd + "/data/cabal1",
            entryFile: "app/Main.hs",
            entryPoint: "main",
            entryArgs: ["some", "args"],
            extraGhcArgs: []
          }
        }
        // todo: Stack config?
        // todo: hie.yaml config?
    ]

    const basicTests = (launchCfg) => {

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

            })
        })
    }

    simpleLaunchConfigs.forEach(basicTests);
})

