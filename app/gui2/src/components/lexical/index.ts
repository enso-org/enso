import { unrefElement, type MaybeElement } from '@vueuse/core'
import {
  createEditor,
  type EditorThemeClasses,
  type KlassConstructor,
  type LexicalEditor,
  type LexicalNode,
  type LexicalNodeReplacement,
} from 'lexical'
import { markRaw, onMounted, type Ref } from 'vue'

type NodeDefinition = KlassConstructor<typeof LexicalNode> | LexicalNodeReplacement

export interface LexicalPlugin {
  nodes?: NodeDefinition[]
  register: (editor: LexicalEditor) => void
}

export function lexicalTheme(theme: Record<string, string>): EditorThemeClasses {
  function getTheme(className: string) {
    if (!(className in theme))
      console.warn(`Referenced class ${className} not found in lexical theme.`, theme)
    return theme[className]!
  }
  return {
    text: {
      strikethrough: getTheme('strikethrough'),
      italic: getTheme('italic'),
      bold: getTheme('bold'),
    },
    quote: getTheme('quote'),
    heading: {
      h1: getTheme('h1'),
      h2: getTheme('h2'),
      h3: getTheme('h3'),
    },
    paragraph: getTheme('paragraph'),
    list: {
      ol: getTheme('ol'),
      ul: getTheme('ul'),
    },
  }
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
