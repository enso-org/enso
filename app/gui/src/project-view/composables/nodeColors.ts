import { type GraphStore, type NodeId } from '$/providers/openedProjects/graph'
import type { NodeType } from '$/providers/openedProjects/graph/graphDatabase'
import { type GroupInfo } from '$/providers/openedProjects/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { ProjectPath } from '@/util/projectPath'
import { computed } from 'vue'

/** TODO: Add docs */
export function useNodeColors(graphStore: GraphStore, getCssValue: (variable: string) => string) {
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

  function getNodeColors(filter?: (node: NodeId) => boolean) {
    return computed(() => {
      const colors = new Set<string>()
      for (const node of graphStore.db.nodeIds()) {
        if (filter?.(node) !== false) {
          const color = getNodeColor(node)
          if (color) colors.add(color)
        }
      }
      return colors
    })
  }

  return { getNodeColor, getNodeColors }
}

/** Compute node color based on the node type, group, and type name. */
export function computeNodeColor(
  getType: () => NodeType,
  getGroup: () => GroupInfo | undefined,
  getTypeName: () => ProjectPath | undefined,
) {
  if (getType() === 'output') return 'var(--output-node-color)'
  if (getType() === 'input') return 'var(--output-node-color)'
  const group = getGroup()
  if (group) return groupColorStyle(group)
  const typeName = getTypeName()
  if (typeName) return colorFromString(typeName.key())
  return 'var(--node-color-no-type)'
}

/** TODO: Add docs */
export function groupColorVar(group: GroupInfo | undefined): string {
  const name = group ? `${group.project}-${group.name}`.replace(/[^\w]/g, '-') : 'fallback'
  return `--group-color-${name}`
}

/** TODO: Add docs */
export function groupColorStyle(group: GroupInfo | undefined): string {
  return `var(${groupColorVar(group)})`
}
