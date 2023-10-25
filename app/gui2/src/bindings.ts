import { defineKeybinds } from '@/util/shortcuts'

export const codeEditorBindings = defineKeybinds('code-editor', {
  toggle: ['`'],
})

export const componentBrowserBindings = defineKeybinds('component-browser', {
  applySuggestion: ['Tab'],
  acceptSuggestion: ['Enter'],
  cancelEditing: ['Escape'],
  moveUp: ['ArrowUp'],
  moveDown: ['ArrowDown'],
})

export const graphBindings = defineKeybinds('graph-editor', {
  undo: ['Mod+Z'],
  redo: ['Mod+Y', 'Mod+Shift+Z'],
  dragScene: ['PointerAux', 'Mod+PointerMain'],
  openComponentBrowser: ['Enter'],
  newNode: ['N'],
  toggleVisualization: ['Space'],
  deleteSelected: ['Delete'],
  selectAll: ['Mod+A'],
  deselectAll: ['Escape', 'PointerMain'],
})

export const selectionMouseBindings = defineKeybinds('selection', {
  replace: ['PointerMain'],
  add: ['Mod+Shift+PointerMain'],
  remove: ['Shift+Alt+PointerMain'],
  toggle: ['Shift+PointerMain'],
  invert: ['Mod+Shift+Alt+PointerMain'],
})

export const nodeEditBindings = defineKeybinds('node-edit', {
  selectAll: ['Mod+A'],
})
