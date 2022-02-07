/// This script will trigger the notarisation process for macOS and trigger our pre-processing
/// and signing of the engine.
require('dotenv').config()
const { notarize } = require('electron-notarize')

exports.default = async function notarizing(context) {
    const { electronPlatformName, appOutDir } = context
    if (electronPlatformName !== 'darwin') {
        return
    }
    // We need to manually re-sign our build artifacts before notarisation.
    // See the script for more information.
    console.log('  • Performing additional signing of dependencies.')
    await require('./signArchivesMacOs').default()

    // Notarize the application.
    const appName = context.packager.appInfo.productFilename
    console.log('  • Notarizing.')
    return await notarize({
        appBundleId: 'com.enso.ide',
        appPath: `${appOutDir}/${appName}.app`,
        appleId: process.env.APPLEID,
        appleIdPassword: process.env.APPLEIDPASS,
    })
}
