const { beforeSign } = require('./signArchivesMacOs')

// ================
// === Callback ===
// ================

exports.default = async function (context) {
    if (context.electronPlatformName === 'darwin') {
        beforeSign()
    }
}
