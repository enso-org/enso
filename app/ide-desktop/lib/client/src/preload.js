const remote = require('electron').remote

let win
if (remote !== undefined) {
    win = remote.getCurrentWindow()
}

if (win === undefined) {
    console.warn('Could not get current window object for window startup animation.')
}

// =============================
// === Window Show Animation ===
// =============================

function ease_in_out_quad(t) {
    return t < 0.5 ? 2 * t * t : 1 - ((-2 * t + 2) * (-2 * t + 2)) / 2
}

function animate_show(target) {
    return new Promise(function (resolve, reject) {
        let opacity = 0
        function show_step(timestamp) {
            opacity += 0.02
            if (opacity > 1) {
                opacity = 1
            }
            target.setOpacity(ease_in_out_quad(opacity))
            if (opacity < 1) {
                window.requestAnimationFrame(show_step)
            } else {
                resolve()
            }
        }
        window.requestAnimationFrame(show_step)
    })
}

if (win !== undefined) {
    window.showAnimation = animate_show(win)
}

// ===================
// === Debug Tools ===
// ===================

// TODO[WD] Enable after making preload configurable (we do not want to load it always)
// require('devtron').install()
