//! JavaScript bindings to the theme engine. They allow the inspection and modification of themes
//! directly from the JavaScript console.

use crate::prelude::*;
use wasm_bindgen::prelude::*;

use super::sheet::Data;
use super::sheet::Value;
use super::theme::Manager;

use crate::system::web;
use js_sys;
use wasm_bindgen::prelude::Closure;



// ===========================
// === JavaScript Bindings ===
// ===========================

mod js {
    use super::*;
    #[wasm_bindgen(inline_js = "
        function interactiveModeDivId(name) {
            return `theme-interactive-mode-${name}`
        }

        export function create_theme_manager_ref(list,choose,get,snapshot,diff) {
            return {list,choose,get,snapshot,diff}
        }

        export function add_interactive_mode_style(name,path,value) {
            let element = document.getElementById(interactiveModeDivId(name))
            if (element) {
                let key = `--${path.replaceAll('.','-')}`
                element.style.cssText += `${key}: ${value}`
            }
        }

        export function create_theme_ref(name,set,populate_styles) {
            let interactiveMode = () => {
                let id       = interactiveModeDivId(name)
                let element  = document.getElementById(id)
                if (!element) {
                    element = document.createElement('div')
                    element.id = id
                    document.body.appendChild(element)
                }
                populate_styles()
                let observer = new MutationObserver(() => {
                    let entries = element.style.cssText.split(';')
                    entries.pop() // last empty one
                    entries = entries.map((t) => t.trim().slice(2).split(':'))
                    entries = entries.map(([key,val]) => [key.replaceAll('-','.'),val])
                    let changes = []
                    for(let [key,val] of entries) {
                        let num = parseFloat(val)
                        if(isNaN(num)) {
                            // Chrome inspector returns colors in the form of 'rgb(255 0 0 / 26%)'
                            let colorMatch = val.split('(')[1].split(')')[0].split(/[, ]+/)

                            let r = parseInt(colorMatch[0])/255
                            let g = parseInt(colorMatch[1])/255
                            let b = parseInt(colorMatch[2])/255
                            let a = 1

                            if (colorMatch.length === 5) {
                                // Format of 'rgb(255 0 0 / 26%)'.
                                colorMatch.splice(3,1) // removing '/'
                                a = parseFloat(colorMatch[3].replace('%',''))/100
                            } else if (colorMatch.length === 4) {
                                // Format of 'rgb(255,0,0,0.2)'.
                                a = parseFloat(colorMatch[3])
                            }

                            if(!isNaN(r) && !isNaN(g) && !isNaN(b) && !isNaN(a)) {
                                let normColor = `rgba(${r},${g},${b},${a})`
                                changes.push([key,normColor])
                            }
                        } else {
                            changes.push([key,`${num}`])
                        }
                    }
                    for(let [key,val] of changes) {
                        set(key,val)
                    }
                })
                observer.observe(element,{attributes:true})
            }
            return {set,interactiveMode}
        }
    ")]
    extern "C" {
        #[allow(unsafe_code)]
        pub fn create_theme_manager_ref(
            list: &List,
            choose: &Choose,
            get: &Get,
            snapshot: &Snapshot,
            diff: &Diff,
        ) -> JsValue;

        #[allow(unsafe_code)]
        pub fn create_theme_ref(
            name: String,
            set: &Set,
            interactive_mode: &InteractiveMode,
        ) -> JsValue;

        #[allow(unsafe_code)]
        pub fn add_interactive_mode_style(name: String, path: String, value: String);
    }

    pub type List = Closure<dyn Fn() -> String>;
    pub type Choose = Closure<dyn Fn(String)>;
    pub type Snapshot = Closure<dyn Fn(String)>;
    pub type Diff = Closure<dyn Fn(String, String) -> String>;
    pub type Get = Closure<dyn Fn(String) -> JsValue>;
    pub type Set = Closure<dyn Fn(String, String)>;
    pub type InteractiveMode = Closure<dyn Fn()>;
}


// TODO[WD]
//     There is a better way than all memory leaks introduced by `mem::forget` after we update
//     wasm-bindgen. There is a function now `Closure::into_js_value` which passes its memory
//     management to JS GC. See https://github.com/enso-org/ide/issues/1028
/// Expose the `window.theme` variable which can be used to inspect and change the theme directly
/// from the JavaScript console.
pub fn expose_to_window(manager: &Manager) {
    let window = web::window();

    let list: js::List = Closure::new(f!([manager]() format!("{:?}",manager.keys())));
    let choose: js::Choose = Closure::new(f!((name) manager.set_enabled(&[name])));
    let snapshot: js::Snapshot = Closure::new(f!((name) manager.snapshot(name)));

    let diff: js::Diff = Closure::new(f!([manager](src:String,tgt:String) {
        let diff = manager.diff(&src,&tgt);
        diff.iter().map(|t| format!("{} = {:?}",t.path,t.value)).join("\n")
    }));

    let owned_manager = manager.clone_ref();
    let get: js::Get = Closure::new(move |name: String| {
        let theme = owned_manager.get(&name).unwrap();
        let owned_theme = theme.clone_ref();
        let set: js::Set = Closure::new(move |name, value| {
            owned_theme.set(name, value);
        });
        let name2 = name.clone();
        let interactive_mode: js::InteractiveMode = Closure::new(move || {
            let mut values = theme.values();
            values.sort_by_key(|(path, _)| path.clone());
            for (path, value) in values {
                match value {
                    Value::Data(Data::Color(color)) => {
                        let js_color = color.to_javascript_string();
                        js::add_interactive_mode_style(name2.clone(), path, js_color)
                    }
                    Value::Data(Data::Number(f)) =>
                        js::add_interactive_mode_style(name2.clone(), path, f.to_string()),
                    _ => {}
                }
            }
        });
        let theme_ref = js::create_theme_ref(name, &set, &interactive_mode);
        mem::forget(set);
        mem::forget(interactive_mode);
        theme_ref
    });

    let theme_manger_ref = js::create_theme_manager_ref(&list, &choose, &get, &snapshot, &diff);

    mem::forget(list);
    mem::forget(choose);
    mem::forget(snapshot);
    mem::forget(diff);
    mem::forget(get);

    js_sys::Reflect::set(&window, &"theme".into(), &theme_manger_ref).ok();
}
