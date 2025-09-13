import { createContextStore } from '@/providers'
import type { ProjectStore } from '@/stores/project'
import type { ProjectNameStore } from '@/stores/projectNames'
import type { SuggestionDb } from '@/stores/suggestionDatabase'
import { useTableExpressionExtension } from '@/util/codemirror/language/tableExpression'
import type { ToValue } from '@/util/reactivity'
import type { Extension } from '@codemirror/state'
import type { Ref } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'

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
    // a ref that allows the extension itself to be initialized lazily.
    const extensions: Record<string, Readonly<Ref<Extension>>> = Object.assign(
      Object.create(null),
      {
        'enso-table-expression': useTableExpressionExtension({
          project,
          projectNames,
          suggestionDb,
        }),
      },
    )
    function getLanguageExtension(languageName: string): Extension | undefined {
      const extension = extensions[languageName]
      DEV: if (!extension) console.warn(`Unknown WidgetText syntax: ${languageName}`)
      return extension?.value
    }
    return getLanguageExtension
  },
)
