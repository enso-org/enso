/** @file Exit with a success exit code if and only if this code is running on Linux CI. */
import * as childProcess from 'node:child_process'
import * as os from 'node:os'

const IS_IN_LINUX_CI =
    os.platform() !== 'win32' && os.platform() !== 'darwin' && process.env.CI === 'true'

let command = 'playwright test -c playwright-e2e.config.ts'
if (IS_IN_LINUX_CI) {
    command = `xvfb-run ${command}`
}

console.log('> ' + command)

try {
    childProcess.execSync(command, { stdio: 'inherit' })
} catch (error) {
    if (
        typeof error === 'object' &&
        error != null &&
        'status' in error &&
        typeof error.status === 'number'
    ) {
        process.exit(error.status)
    }
}
