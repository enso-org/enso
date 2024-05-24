import { blockTypes, normalizeHeadingLevel } from '@/components/lexical/formatting'
import { unrefElement, type MaybeElement } from '@vueuse/core'
import {
  createEditor,
  type EditorThemeClasses,
  type KlassConstructor,
  type LexicalEditor,
  type LexicalNode,
  type LexicalNodeReplacement,
} from 'lexical'
import { assertDefined } from 'shared/util/assert'
import { markRaw, onMounted, type Ref } from 'vue'

type NodeDefinition = KlassConstructor<typeof LexicalNode> | LexicalNodeReplacement

export interface LexicalPlugin {
  nodes?: NodeDefinition[]
  register: (editor: LexicalEditor) => void
}

export function lexicalTheme(themeCss: Record<string, string>): EditorThemeClasses {
  const theme = themeFromCss(themeCss)
  if (theme.heading) {
    for (const level of Object.keys(theme.heading)) {
      const levelTag = level as keyof typeof theme.heading
      const normalized = normalizeHeadingLevel(levelTag)
      theme.heading[levelTag] = theme.heading[normalized] ?? 'lexical-unsupported-heading-level'
    }
  }
  return theme
}

function themeFromCss(theme: Record<string, string>): EditorThemeClasses {
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
