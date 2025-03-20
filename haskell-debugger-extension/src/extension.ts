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

import * as vscode from 'vscode';
import { activateMockDebug } from './activateMockDebug';

export function activate(context: vscode.ExtensionContext) {

    // run the debug adapter as a server inside the extension and communicate via a socket
    activateMockDebug(context, new MockDebugAdapterServerDescriptorFactory());

}

export function deactivate() {
	// nothing to do
}

class MockDebugAdapterServerDescriptorFactory implements vscode.DebugAdapterDescriptorFactory {

	createDebugAdapterDescriptor(session: vscode.DebugSession, executable: vscode.DebugAdapterExecutable | undefined): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {

		return new vscode.DebugAdapterServer(4711, 'localhost'); // Hardcoded port for now
	}

	dispose() {
	}
}

