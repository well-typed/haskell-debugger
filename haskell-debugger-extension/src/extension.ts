/*---------------------------------------------------------
 * Copyright (C) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------*/
/*
 * extension.ts contains code for launching the debug adapter in three different ways:
 * - as an external program communicating with VS Code via stdin/stdout,
 * - as a server process communicating with VS Code via sockets or named pipes, or
 * - as inlined code running in the extension itself (default).
 * 
 * Since the code in extension.ts uses node.js APIs it cannot run in the browser.
 */

'use strict';

import { activateMockDebug } from './activateMockDebug';
import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as net from 'net';

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

let logger : vscode.OutputChannel = vscode.window.createOutputChannel("haskell-debug-adapter");

export function activate(context: vscode.ExtensionContext) {

    // run the debug adapter as a server inside the extension and communicate via a socket
    activateMockDebug(context, new MockDebugAdapterServerDescriptorFactory(context));

}

export function deactivate() {
	// nothing to do
}

class MockDebugAdapterServerDescriptorFactory implements vscode.DebugAdapterDescriptorFactory {

    private processes = new Map<string, cp.ChildProcess>();
	private logger: vscode.OutputChannel;

	constructor(context: vscode.ExtensionContext) {
		this.logger = vscode.window.createOutputChannel("Haskell Debugger");
		this.logger.appendLine("[Factory] Initialized");

        context.subscriptions.push(
            vscode.debug.onDidTerminateDebugSession((session) => {
                const proc = this.processes.get(session.id);
                if (proc && !proc.killed) {
                    this.logger.appendLine(`[Factory] Killing process for session ${session.id}`);
                    proc.kill();
                }
                this.processes.delete(session.id);
            })
        );
	}

	async createDebugAdapterDescriptor(
		session: vscode.DebugSession,
		executable: vscode.DebugAdapterExecutable | undefined
	): Promise<vscode.DebugAdapterDescriptor> {

		// return new vscode.DebugAdapterServer(4717, 'localhost');

		const port = await getFreePort();
		this.logger.appendLine(`[Factory] Launching ghc-debugger on port ${port}`);

		const debuggerProcess = cp.spawn('haskell-debugger-server', ['--port', port.toString()]);

        debuggerProcess.on('spawn', () => {
            this.logger.appendLine('[Factory] ghc-debugger spawned...');
        });

		debuggerProcess.stdout.on('data', (data) => {
			this.logger.appendLine(`[stdout] ${data}`);
		});

		debuggerProcess.stderr.on('data', (data) => {
			this.logger.appendLine(`[stderr] ${data}`);
		});

		debuggerProcess.on('exit', (code, signal) => {
			this.logger.appendLine(`[exit] ghc-debugger exited with code ${code} and signal ${signal}`);
		});

		debuggerProcess.on('error', (err) => {
			this.logger.appendLine(`[error] Failed to start ghc-debugger: ${err.message}`);
		});

		this.processes.set(session.id, debuggerProcess);

        const ready = new Promise<void>((resolve, reject) => {
            const timeout = setTimeout(() => {
                reject(new Error("ghc-debugger did not signal readiness in time"));
            }, 15000); // 15 second timeout

		    this.logger.appendLine(`[Factory] Waiting for debugger to be ready...`);
            debuggerProcess.stdout.on('data', (data: Buffer) => {
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

        await ready;
		this.logger.appendLine(`[Factory] Debugger is ready on port ${port}`);
		return new vscode.DebugAdapterServer(port, 'localhost');
	}

	dispose() {
		this.logger.appendLine("[Factory] Disposing and cleaning up processes...");
		for (const proc of this.processes.values()) {
			if (!proc.killed) {
				proc.kill();
			}
		}
		this.processes.clear();
		this.logger.dispose();
	}

}

