import type { GraphSelection } from '@/providers/graphSelection'
import type { GraphNavigator } from '../src/providers/graphNavigator'
import { Rect } from '../src/util/data/rect'
import { Vec2 } from '../src/util/data/vec2'

export const graphNavigator: GraphNavigator = {
  events: {} as any,
  clientToScenePos: () => Vec2.Zero,
  clientToSceneRect: () => Rect.Zero,
  panAndZoomTo: () => {},
  transform: '',
  prescaledTransform: '',
  translate: Vec2.Zero,
  scale: 1,
  sceneMousePos: Vec2.Zero,
  viewBox: '',
  viewport: Rect.Zero,
}

export function graphNavigatorWith(modifications?: Partial<GraphNavigator>): GraphNavigator {
  return Object.assign({}, graphNavigator, modifications)
}

export const graphSelection: GraphSelection = {
  events: {} as any,
  anchor: undefined,
  deselectAll: () => {},
  handleSelectionOf: () => {},
  hoveredNode: undefined,
  hoveredPort: undefined,
  isSelected: () => false,
  mouseHandler: () => false,
  selectAll: () => {},
  selected: new Set(),
}

export function graphSelectionWith(modifications?: Partial<GraphSelection>): GraphSelection {
  return Object.assign({}, graphSelection, modifications)
}

export const all = {
  'graph navigator': graphNavigator,
  'graph selection': graphSelection,
}

export function allWith(
  modifications: Partial<{ [K in keyof typeof all]: Partial<(typeof all)[K]> }>,
): typeof all {
  return {
    'graph navigator': graphNavigatorWith(modifications['graph navigator']),
    'graph selection': graphSelectionWith(modifications['graph selection']),
  }
}
