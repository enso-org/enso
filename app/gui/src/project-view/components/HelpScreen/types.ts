export interface HelpInfo {
  title: string
  headers: string[]
  sections: HelpScreenSection[]
}

export interface HelpScreenSection {
  description?: string | undefined
  name: string
  entries: HelpScreenEntry[]
}

export interface HelpScreenEntry {
  name: string
  values: string[]
  href?: string | undefined
}
