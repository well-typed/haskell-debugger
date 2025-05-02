const { execSync, spawn } = require('child_process');

function findExecutable(executable) {
  try {
    return execSync(process.platform === 'win32' ? `where ${executable}` : `which ${executable}`)
      .toString()
      .trim();
  } catch (error) {
    return null;
  }
}

const HDA_PATH = findExecutable('haskell-debugger-server');
console.log(HDA_PATH)

// For debugging the plugin and DA, it's very useful to disable this entirely
// and run the debugger process in a separate terminal.

// if (HDA_PATH) {
//   const hdaProcess = spawn('haskell-debugger-server', { stdio: 'inherit', shell: true });
//   hdaProcess.on('exit', (code) => process.exit(code));
// } else {
//   const errorMessage = {
//     command: 'initialize',
//     success: false,
//     request_seq: 1,
//     seq: 1,
//     type: 'response',
//     message: 'haskell-debugger-server is not found. Does your shell recognize haskell-debugger-server?'
//   };
  
//   const response = `Content-Length: ${JSON.stringify(errorMessage).length}\r\n\r\n${JSON.stringify(errorMessage)}`;
//   console.error(response);
//   process.exit(1);
// }

