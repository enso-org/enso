const { beforeSign } = require('./signArchives')

// ================
// === Callback ===
// ================

exports.default = async function (context) {
    if (context.electronPlatformName === 'darwin') {
        beforeSign()
    }
}
