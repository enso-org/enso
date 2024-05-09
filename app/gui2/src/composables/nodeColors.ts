import { useGraphStore, type NodeId } from '@/stores/graph'
import { computed } from 'vue'

export function useNodeColors(getCssValue: (variable: string) => string) {
  const graphStore = useGraphStore()

  function getNodeColor(node: NodeId) {
    const color = graphStore.db.getNodeColorStyle(node)
    if (color.startsWith('var')) {
      // Some colors are defined in CSS variables, we need to get the actual color.
      const variableName = color.slice(4, -1)
      const value = getCssValue(variableName)
      if (value === '') return undefined
      return value
    } else {
      return color
    }
  }

  const visibleNodeColors = computed(() => {
    const colors = new Set<string>()
    for (const node of graphStore.db.nodeIds()) {
      const color = getNodeColor(node)
      if (color) colors.add(color)
    }
    return colors
  })

  return { getNodeColor, visibleNodeColors }
}
