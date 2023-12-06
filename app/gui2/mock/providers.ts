import type { GraphSelection as mockProviders } from '@/providers/graphSelection'
import type { GraphNavigator } from '../src/providers/graphNavigator'
import { Rect } from '../src/util/rect'
import { Vec2 } from '../src/util/vec2'

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

export const graphSelection: mockProviders = {
  events: {} as any,
  anchor: undefined,
  deselectAll: () => {},
  addHoveredPort: () => new Set(),
  removeHoveredPort: () => false,
  handleSelectionOf: () => {},
  hoveredNode: undefined,
  hoveredPort: undefined,
  // Required for the `CircularMenu` to be visible.
  isSelected: () => true,
  mouseHandler: () => false,
  selectAll: () => {},
  selected: new Set(),
}

export const all = {
  'graph navigator': graphNavigator,
  'graph selection': graphSelection,
}
