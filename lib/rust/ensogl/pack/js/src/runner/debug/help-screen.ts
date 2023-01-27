/** @file Help screen implementation. It displays a table of the provided entries. */

import * as dom from '../dom/dom'

// =======================
// === HelpScreenEntry ===
// =======================

/** A single entry in the help screen view. If assigned with `href`, it will be displayed as a
 * link. */
export class HelpScreenEntry {
    name: string
    values: string[]
    href?: string

    constructor(name: string, values: string[], href?: string) {
        this.name = name
        this.values = values
        this.href = href
    }
}

// ==================
// === HelpScreen ===
// ==================

/** A visual help screen. It displays a table of the provided entries. */
export class HelpScreen {
    /** Displays a debug screen which allows the user to run one of predefined debug examples. */
    display(cfg: { title: string; headers: string[]; entries: HelpScreenEntry[] }) {
        const padding = '8px'
        const backgroundRadius = '8px'
        const div = dom.newTopLevelDiv()
        div.style.fontFamily =
            '"SF Pro Text","SF Pro Icons","Helvetica Neue","Helvetica","Arial",sans-serif'
        div.style.fontSize = '14px'
        div.style.overflow = 'scroll'
        const div2 = document.createElement('div')
        div2.style.padding = '10px'
        div.appendChild(div2)

        const title = document.createElement('div')
        title.style.fontWeight = 'bold'
        title.style.padding = '8px'
        const content = document.createTextNode(cfg.title)
        const table = document.createElement('table')
        table.style.paddingTop = '20px'
        table.style.borderCollapse = 'collapse'
        table.style.maxWidth = '800px'
        div2.style.position = 'absolute'
        div2.style.zIndex = '1'
        title.appendChild(content)
        div2.appendChild(title)
        div2.appendChild(table)

        const tr = document.createElement('tr')
        for (const header of cfg.headers) {
            const th = document.createElement('th')
            th.innerText = header
            th.style.textAlign = 'left'
            th.style.padding = padding
            tr.appendChild(th)
        }
        table.appendChild(tr)

        let rowWithBg = true
        for (const entry of cfg.entries) {
            const tr = document.createElement('tr')
            table.appendChild(tr)

            const last = cfg.headers.length - 1
            for (let i = 0; i < cfg.headers.length; i++) {
                const td = document.createElement('td')
                td.style.padding = padding
                tr.appendChild(td)
                if (rowWithBg) {
                    td.style.background = '#00000010'
                    if (i == 0) {
                        td.style.borderTopLeftRadius = backgroundRadius
                        td.style.borderBottomLeftRadius = backgroundRadius
                    } else if (i == last) {
                        td.style.borderTopRightRadius = backgroundRadius
                        td.style.borderBottomRightRadius = backgroundRadius
                    }
                }
                if (i == 0) {
                    const a = document.createElement('a')
                    const linkText = document.createTextNode(entry.name)
                    a.appendChild(linkText)
                    a.title = entry.name
                    if (entry.href) {
                        a.href = entry.href
                    }
                    td.appendChild(a)
                } else {
                    const value = entry.values[i - 1]
                    if (value != null) {
                        td.innerText = value
                    }
                }
            }
            rowWithBg = !rowWithBg
        }
    }
}
