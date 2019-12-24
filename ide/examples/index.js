import * as wasm from "basegl";

let pfx = "run_example_"

function hello_screen(msg) {
    let names = []
    for (let fn of Object.getOwnPropertyNames(wasm)) {
        if (fn.startsWith(pfx)) {
            let name = fn.replace(pfx,"")
            names.push(name)
        }
    }

    if(msg==="" && msg===null && msg===undefined) {
        msg = ""
    }
    let newDiv     = document.createElement("div");
    let newContent = document.createTextNode(msg + "Choose an example:");
    let currentDiv = document.getElementById("app");
    let ul         = document.createElement('ul');
    newDiv.appendChild(newContent);
    document.body.insertBefore(newDiv, currentDiv);
    newDiv.appendChild(ul);

    for (let name of names) {
        let li       = document.createElement('li');
        let a        = document.createElement('a');
        let linkText = document.createTextNode(name);
        ul.appendChild(li);
        a.appendChild(linkText);
        a.title = name;
        a.href  = "/" + name;
        li.appendChild(a);
    }
}

function main() {
    let target = window.location.href.split('/')[3];
    if (target === "") {
        hello_screen()
    } else {
        let fn_name = pfx + target
        let fn      = wasm[fn_name]
        if (fn) { fn() } else {
            hello_screen("WASM function '" + fn_name + "' not found! ")
        }
    }
}

main()
