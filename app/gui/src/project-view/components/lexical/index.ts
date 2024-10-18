import { unrefElement, type MaybeElement } from '@vueuse/core'
import type {
  EditorThemeClasses,
  KlassConstructor,
  LexicalEditor,
  LexicalNode,
  LexicalNodeReplacement,
} from 'lexical'
import { createEditor } from 'lexical'
import { markRaw, onMounted, type Ref } from 'vue'
import { assertDefined } from 'ydoc-shared/util/assert'

type NodeDefinition = KlassConstructor<typeof LexicalNode> | LexicalNodeReplacement

export interface LexicalPlugin {
  nodes?: NodeDefinition[]
  register: (editor: LexicalEditor) => void
}

/** TODO: Add docs */
export function lexicalTheme(theme: Record<string, string>): EditorThemeClasses {
  // eslint-disable-next-line @typescript-eslint/no-empty-object-type
  interface EditorThemeShape extends Record<string, EditorThemeShape | string> {}
  const editorClasses: EditorThemeShape = {}
  for (const [classPath, className] of Object.entries(theme)) {
    const path = classPath.split('_')
    const leaf = path.pop()
    // `split` will always return at least one value
    assertDefined(leaf)
    let obj = editorClasses
    for (const section of path) {
      const nextObj = (obj[section] ??= {})
      if (typeof nextObj === 'string') {
        console.warn(
          `Lexical theme contained path '${classPath}', but path component '${section}' is a leaf.`,
        )
        continue
      }
      obj = nextObj
    }
    obj[leaf] = className
  }
  return editorClasses
}

/** TODO: Add docs */
export function useLexical(
  contentElement: Ref<MaybeElement>,
  namespace: string,
  theme: EditorThemeClasses,
  plugins: LexicalPlugin[],
) {
  const nodes = new Set<NodeDefinition>()
  for (const node of plugins.flatMap((plugin) => plugin.nodes)) if (node) nodes.add(node)

  const editor = markRaw(
    createEditor({
      editable: true,
      namespace,
      theme,
      nodes: [...nodes],
      onError: console.error,
    }),
  )

  onMounted(() => {
    const element = unrefElement(contentElement.value)
    if (element instanceof HTMLElement) editor.setRootElement(element)

    for (const plugin of plugins) plugin.register(editor)
  })

  return { editor }
}
