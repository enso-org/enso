import { type Extension } from '@codemirror/state'
import { tableExpression } from 'lezer-enso-table-expr'

/** If the given name identifies a supported language, return a CodeMirror extension for it. */
export function languageExtension(languageName: string | undefined): Extension | undefined {
  switch (languageName) {
    case 'enso-table-expression':
      return tableExpression()
  }
}
