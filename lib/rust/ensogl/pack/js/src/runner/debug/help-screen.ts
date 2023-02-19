/** @file Help screen implementation. It displays a table of the provided entries. */

import * as dom from '../dom/dom'

// =========================
// === HelpScreenSection ===
// =========================

export class HelpScreenSection {
    description?: string
    name: string
    entries: HelpScreenEntry[]
    constructor(cfg: { name: string; description?: string; entries: HelpScreenEntry[] }) {
        this.name = cfg.name
        this.description = cfg.description
        this.entries = cfg.entries
    }
}

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
    display(cfg: { title: string; headers: string[]; sections: HelpScreenSection[] }) {
        const padding = '8px'
        const backgroundRadiusPx = 8
        const sectionPaddingPx = 10
        const backgroundRadius = `${backgroundRadiusPx}px`
        const sectionBackgroundRadius = `${backgroundRadiusPx + sectionPaddingPx}px`
        const div = dom.newTopLevelDiv()
        div.style.fontFamily =
            '"SF Pro Text","SF Pro Icons","Helvetica Neue","Helvetica","Arial",sans-serif'
        div.style.fontSize = '14px'
        div.style.overflow = 'scroll'
        div.style.color = '#000000c9'
        const div2 = document.createElement('div')
        div2.style.padding = '10px'
        div.appendChild(div2)

        const title = document.createElement('div')
        title.style.fontWeight = 'bold'
        title.style.padding = '8px'
        title.style.fontSize = '16px'
        title.style.color = 'white'
        title.style.background = '#d5461e'
        title.style.borderRadius = '8px'
        const content = document.createTextNode(cfg.title)

        div2.style.position = 'absolute'
        div2.style.zIndex = '1'
        title.appendChild(content)
        div2.appendChild(title)

        for (const section of cfg.sections) {
            const sectionDiv = document.createElement('div')
            sectionDiv.style.marginTop = '16px'
            sectionDiv.style.padding = `${sectionPaddingPx}px`
            sectionDiv.style.border = '3px solid #000000c9'
            sectionDiv.style.borderRadius = sectionBackgroundRadius
            sectionDiv.style.maxWidth = '800px'

            const sectionTitleDiv = document.createElement('div')
            const sectionTitle = document.createTextNode(section.name)
            sectionTitleDiv.style.fontWeight = 'bold'
            sectionTitleDiv.style.fontSize = '16px'
            sectionTitleDiv.style.marginBottom = '6px'
            sectionTitleDiv.style.marginLeft = '2px'
            sectionDiv.appendChild(sectionTitleDiv)
            sectionTitleDiv.appendChild(sectionTitle)

            if (section.description) {
                const sectionDescriptionDiv = document.createElement('div')
                const sectionDescription = document.createTextNode(section.description)
                sectionDescriptionDiv.style.marginBottom = '16px'
                sectionDescriptionDiv.style.marginLeft = '2px'
                sectionDiv.appendChild(sectionDescriptionDiv)
                sectionDescriptionDiv.appendChild(sectionDescription)
            }

            const table = document.createElement('table')
            table.style.paddingTop = '20px'
            table.style.borderCollapse = 'collapse'

            const tr = document.createElement('tr')
            for (const header of cfg.headers) {
                const th = document.createElement('th')
                th.innerText = header
                th.style.textAlign = 'left'
                th.style.padding = padding
                tr.appendChild(th)
            }
            table.appendChild(tr)

            sectionDiv.appendChild(table)
            div2.appendChild(sectionDiv)

            let rowWithBg = true
            for (const entry of section.entries) {
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
}
