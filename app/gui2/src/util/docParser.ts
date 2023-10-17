import { parse_doc_to_json } from '@/util/ffi'

export function parseDocs(docs: string): Doc.Section[] {
  const json = parse_doc_to_json(docs)
  return JSON.parse(json)
}

export namespace Doc {
  export type HtmlString = string
  export type Tag =
    | 'Added'
    | 'Advanced'
    | 'Alias'
    | 'Deprecated'
    | 'Icon'
    | 'Group'
    | 'Modified'
    | 'Private'
    | 'Removed'
    | 'TextOnly'
    | 'Unstable'
    | 'Upcoming'
  export type Mark = 'Important' | 'Info' | 'Example'

  export interface Argument {
    name: string
    description: HtmlString
  }

  export type Section =
    | { Tag: Section.Tag }
    | { Paragraph: Section.Paragraph }
    | { List: Section.List }
    | { Arguments: Section.Arguments }
    | { Keyed: Section.Keyed }
    | { Marked: Section.Marked }

  export namespace Section {
    /** The documentation tag. */
    export interface Tag {
      tag: Doc.Tag
      body: HtmlString
    }

    /** The paragraph of the text. */
    export interface Paragraph {
      body: HtmlString
    }

    /** A list of items. Each item starts with a dash (`-`). */
    export interface List {
      items: HtmlString[]
    }

    /** A list of items, but each item is an [`Argument`]. Starts with `Arguments:` keyword. */
    export interface Arguments {
      args: Argument[]
    }

    /** The section that starts with the key followed by the colon and the body. */
    export interface Keyed {
      key: String
      body: HtmlString
    }
    /** The section that starts with the mark followed by the header and the body. */
    export interface Marked {
      mark: Mark
      header?: string
      body: HtmlString
    }
  }
}
