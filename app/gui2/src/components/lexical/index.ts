import { unrefElement, type MaybeElement } from '@vueuse/core'
import {
  createEditor,
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

export function useLexical(
  contentElement: Ref<MaybeElement>,
  namespace: string,
  plugins: LexicalPlugin[],
) {
  const nodes = new Set<NodeDefinition>()
  for (const node of plugins.flatMap((plugin) => plugin.nodes)) if (node) nodes.add(node)

  const editor = markRaw(
    createEditor({
      editable: true,
      namespace,
      theme: {
        text: {
          strikethrough: 'lexical-strikethrough',
        },
      },
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
