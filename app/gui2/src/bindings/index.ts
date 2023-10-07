import { defineKeybinds } from '@/util/shortcuts'

export const codeEditorBindings = defineKeybinds('code-editor', {
  toggle: ['`'],
})

export const graphBindings = defineKeybinds('graph-editor', {
  undo: ['Mod+Z'],
  redo: ['Mod+Y', 'Mod+Shift+Z'],
  dragScene: ['PointerAux', 'Mod+PointerMain'],
  openComponentBrowser: ['Enter'],
  newNode: ['N'],
})

export const nodeBindings = defineKeybinds('node-selection', {
  deleteSelected: ['Delete'],
  selectAll: ['Mod+A'],
  deselectAll: ['Escape', 'PointerMain'],
  replace: ['PointerMain'],
  add: ['Mod+Shift+PointerMain'],
  remove: ['Shift+Alt+PointerMain'],
  toggle: ['Shift+PointerMain'],
  invert: ['Mod+Shift+Alt+PointerMain'],
  toggleVisualization: ['Space'],
})
