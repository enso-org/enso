let win = require('electron').remote.getCurrentWindow()


// =============================
// === Window Show Animation ===
// =============================

function ease_in_out_quad(t) {
    return t<.5 ? 2*t*t : 1 - (-2*t+2)*(-2*t+2) / 2
}

function animate_show(target) {
    return new Promise(function(resolve, reject) {
        let opacity = 0
        function show_step(timestamp) {
            opacity += 0.02
            if (opacity > 1) { opacity = 1 }
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

window.showAnimation = animate_show(win)



// ===================
// === Debug Tools ===
// ===================

// TODO[WD] Enable after making preload configurable (we do not want to load it always)
// require('devtron').install()
