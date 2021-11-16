// Text input event handlers.
//
// The "text input event" is the normal keyup/keydown event, or cut/copy/paste event. The handling of these events is
// done by creating an invisible focused textarea html element and intercepting events emitted to this element.
// We do it this way, because this is so far the only way a website may affect the clipboard which works on each
// browser.
export class TextInputHandlers {
    // Constructor creates the textarea element and put it into document body.
    constructor() {
        this.event_handlers = {}
        this.text_area = create_invisible_text_area()
        document.body.appendChild(this.text_area)
        this.text_area.focus()
        this.bind_text_area_events()
        this.bind_window_events()
    }

    // Set event handler. The name can be 'keyup' or 'keydown'.
    set_event_handler(name, callback) {
        this.event_handlers[name] = callback
    }

    // Set copy handler. The copy handler takes bool as first argument denoting if it is cut operation, and returns
    // string which should be actually copied to clipboard.
    set_copy_handler(handler) {
        this.copy_handler = handler
    }

    // Set paste handler. The paste handler takes the text from clipboard as the only argument.
    set_paste_handler(handler) {
        this.paste_handler = handler
    }

    // Set callback called each time the browser window lose focus.
    set_window_defocus_handler(handler) {
        this.window_defocus_handler = handler
    }

    // Remove the textarea element and stop handling any events.
    stop_handling() {
        this.text_area.remove()
    }

    // This is private function being a construction stage.
    bind_text_area_events() {
        this.text_area.addEventListener('cut', e => {
            // Clear textarea in next frame (after cutting).
            setTimeout(_ => {
                this.text_area.value = ''
            }, 0)
        })
        this.text_area.addEventListener('copy', e => {
            // Clear textarea in next frame (after copying).
            setTimeout(_ => {
                this.text_area.value = ''
            }, 0)
        })
        this.text_area.addEventListener('paste', e => {
            if (typeof this.paste_handler !== 'undefined') {
                let paste = (event.clipboardData || window.clipboardData).getData('text')
                this.paste_handler(paste)
            } else {
                e.preventDefault()
            }
        })
        this.text_area.addEventListener('contextmenu', e => {
            e.preventDefault()
        })
        this.text_area.addEventListener('blur', e => {
            this.text_area.focus()
        })
        this.text_area.addEventListener('keydown', e => {
            let code = e.keyCode

            let is_cut = code === 88 && (e.metaKey || e.ctrlKey)
            let is_copy = code === 67 && (e.metaKey || e.ctrlKey)
            let is_paste = code === 86 && (e.metaKey || e.ctrlKey)
            if (is_copy || is_cut) {
                if (typeof this.copy_handler !== 'undefined') {
                    this.text_area.value = this.copy_handler(is_cut)
                    this.text_area.selectionStart = 0
                    this.text_area.selectionEnd = this.text_area.value.length
                } else {
                    e.preventDefault()
                }
            } else if (!is_paste) {
                e.preventDefault()
            }

            if (typeof this.event_handlers['keydown'] !== 'undefined') {
                this.event_handlers['keydown'](e)
            }
        })
        this.text_area.addEventListener('keyup', e => {
            e.preventDefault()
            if (typeof this.event_handlers['keyup'] !== 'undefined') {
                this.event_handlers['keyup'](e)
            }
        })
    }

    bind_window_events() {
        window.addEventListener('blur', e => {
            if (typeof this.window_defocus_handler !== 'undefined') {
                this.window_defocus_handler()
            }
        })
    }
}

// Creates invisible textarea.
function create_invisible_text_area() {
    const css_class_name = 'enso'

    let text_area = document.createElement('textarea')
    text_area.className = css_class_name
    text_area.setAttribute('autocomplete', 'off')
    text_area.setAttribute('autocorrect', 'off')
    text_area.setAttribute('autocapitalize', 'off')
    text_area.setAttribute('spellcheck', 'false')
    let style = document.createElement('style')
    style.innerHTML = `
       textarea.${css_class_name} {
       z-index: 100000;
       position: absolute;
       opacity: 0;
       border-radius: 4px;
       color:white;
       font-size: 6;
       background: gray;
       -moz-appearance: none;
       appearance:none;
       border:none;
       resize: none;
       outline: none;
       overflow: hidden;
       text-indent: 0px;
       padding: 0 0px;
       margin: 0 -1px;
       text-indent: 0px;
       -ms-user-select: text;
       -moz-user-select: text;
       -webkit-user-select: text;
       user-select: text;
       white-space: pre!important;
       }
       textarea: focus." + css_class_name + " {
       outline: 0px !important;
       -webkit-appearance: none;
       }`
    document.body.appendChild(style)
    text_area.style.left = -100 + 'px'
    text_area.style.top = -100 + 'px'
    text_area.style.height = 1
    text_area.style.width = 1
    return text_area
}
