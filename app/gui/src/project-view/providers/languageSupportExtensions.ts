import type { ProjectStore } from '$/providers/openedProjects/project'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import type { SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import { createContextStore } from '@/providers'
import { useTableExpressionExtension } from '@/util/codemirror/language/tableExpression'
import type { ToValue } from '@/util/reactivity'
import type { Extension } from '@codemirror/state'
import { record } from 'enso-common/src/utilities/data/object'
import type { Opt } from 'enso-common/src/utilities/data/opt'

export interface LanguageSupportOptions {
  project: ToValue<Opt<ProjectStore>>
  projectNames: ToValue<Opt<ProjectNameStore>>
  suggestionDb: ToValue<Opt<SuggestionDb>>
}

/**
 * A context store for language-support CodeMirror extensions.
 *
 * Some extensions have reactive dependencies on project data; this store allows such extensions to
 * share computation between instances in any number of editors.
 */
export const [provideLanguageSupportExtensions, useLanguageSupportExtensions] = createContextStore(
  'Table expression extension',
  ({
    project,
    projectNames,
    suggestionDb,
  }: LanguageSupportOptions): ((languageName: string) => Extension | undefined) => {
    // For each extension, a function is run to perform any necessary setup; the function returns
    // an extension factory, allowing the extension to make use of both globally shared computation
    // and contextual data.
    const extensions = record<string, () => Extension>({
      'enso-table-expression': useTableExpressionExtension({
        project,
        projectNames,
        suggestionDb,
      }),
    })
    function getLanguageExtension(languageName: string): Extension | undefined {
      const extension = extensions[languageName]
      DEV: if (!extension) console.warn(`Unknown WidgetText syntax: ${languageName}`)
      return extension?.()
    }
    return getLanguageExtension
  },
)
