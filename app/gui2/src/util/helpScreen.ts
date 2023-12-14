/** @file Help screen implementation. It displays a table of the provided entries. */

// =========================
// === HelpScreenSection ===
// =========================

export class HelpScreenSection {
  description?: string | undefined
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
  href?: string | undefined

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
  display(config: { title: string; headers: string[]; sections: HelpScreenSection[] }) {
    const padding = '8px'
    const backgroundRadiusPx = 8
    const sectionPaddingPx = 10
    const backgroundRadius = `${backgroundRadiusPx}px`
    const sectionBackgroundRadius = `${backgroundRadiusPx + sectionPaddingPx}px`
    const div = document.body.appendChild(document.createElement('div'))
    div.style.width = '100%'
    div.style.height = '100%'
    div.style.fontFamily =
      '"SF Pro Text","SF Pro Icons","Helvetica Neue","Helvetica","Arial",sans-serif'
    div.style.fontSize = '14px'
    div.style.overflow = 'scroll'
    div.style.color = '#000000c9'

    const div2 = div.appendChild(document.createElement('div'))
    div2.style.padding = '10px'

    const title = div2.appendChild(document.createElement('div'))
    title.style.fontWeight = 'bold'
    title.style.padding = '8px'
    title.style.fontSize = '16px'
    title.style.color = 'white'
    title.style.background = '#d5461e'
    title.style.borderRadius = '8px'

    title.appendChild(document.createTextNode(config.title))

    div2.style.position = 'absolute'
    div2.style.zIndex = '1'
    for (const section of config.sections) {
      const sectionDiv = div2.appendChild(document.createElement('div'))
      sectionDiv.style.marginTop = '16px'
      sectionDiv.style.padding = `${sectionPaddingPx}px`
      sectionDiv.style.border = '3px solid #000000c9'
      sectionDiv.style.borderRadius = sectionBackgroundRadius
      sectionDiv.style.maxWidth = '800px'

      const sectionTitleDiv = sectionDiv.appendChild(document.createElement('div'))
      sectionTitleDiv.style.fontWeight = 'bold'
      sectionTitleDiv.style.fontSize = '16px'
      sectionTitleDiv.style.marginBottom = '6px'
      sectionTitleDiv.style.marginLeft = '2px'

      sectionTitleDiv.appendChild(document.createTextNode(section.name))

      if (section.description) {
        const sectionDescriptionDiv = document.createElement('div')
        const sectionDescription = document.createTextNode(section.description)
        sectionDescriptionDiv.style.marginBottom = '16px'
        sectionDescriptionDiv.style.marginLeft = '2px'
        sectionDiv.appendChild(sectionDescriptionDiv)
        sectionDescriptionDiv.appendChild(sectionDescription)
      }

      const table = sectionDiv.appendChild(document.createElement('table'))
      table.style.paddingTop = '20px'
      table.style.borderCollapse = 'collapse'

      const tr = table.appendChild(document.createElement('tr'))
      for (const header of config.headers) {
        const th = document.createElement('th')
        th.innerText = header
        th.style.textAlign = 'left'
        th.style.padding = padding
        tr.appendChild(th)
      }
      let rowWithBg = true
      for (const entry of section.entries) {
        const tr = table.appendChild(document.createElement('tr'))
        const last = config.headers.length - 1
        for (let i = 0; i < config.headers.length; i++) {
          const td = tr.appendChild(document.createElement('td'))
          td.style.padding = padding
          if (rowWithBg) {
            td.style.background = '#00000010'
            if (i === 0) {
              td.style.borderTopLeftRadius = backgroundRadius
              td.style.borderBottomLeftRadius = backgroundRadius
            } else if (i === last) {
              td.style.borderTopRightRadius = backgroundRadius
              td.style.borderBottomRightRadius = backgroundRadius
            }
          }
          if (i === 0) {
            const a = td.appendChild(document.createElement('a'))
            a.title = entry.name
            if (entry.href) a.href = entry.href

            a.appendChild(document.createTextNode(entry.name))
          } else {
            const value = entry.values[i - 1]
            if (value != null) td.innerText = value
          }
        }
        rowWithBg = !rowWithBg
      }
    }
  }
}
