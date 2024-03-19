import { defineKeybinds } from '@/util/shortcuts'

export const codeEditorBindings = defineKeybinds('code-editor', {
  toggle: ['Mod+`'],
})

export const interactionBindings = defineKeybinds('current-interaction', {
  cancel: ['Escape'],
})

export const componentBrowserBindings = defineKeybinds('component-browser', {
  applySuggestion: ['Tab'],
  acceptSuggestion: ['Shift+Enter'],
  acceptInput: ['Mod+Enter'],
  acceptAIPrompt: ['Enter'],
  moveUp: ['ArrowUp'],
  moveDown: ['ArrowDown'],
})

export const graphBindings = defineKeybinds('graph-editor', {
  undo: ['Mod+Z'],
  redo: ['Mod+Y', 'Mod+Shift+Z'],
  dragScene: ['PointerAux', 'Mod+PointerMain'],
  openComponentBrowser: ['Enter'],
  toggleVisualization: ['Space'],
  deleteSelected: ['OsDelete'],
  zoomToSelected: ['Mod+Shift+A'],
  selectAll: ['Mod+A'],
  deselectAll: ['Escape'],
  copyNode: ['Mod+C'],
  pasteNode: ['Mod+V'],
  collapse: ['Mod+G'],
  startProfiling: ['Mod+Alt+,'],
  stopProfiling: ['Mod+Alt+.'],
  enterNode: ['Mod+E'],
  exitNode: ['Mod+Shift+E'],
})

export const visualizationBindings = defineKeybinds('visualization', {
  nextType: ['Mod+Space'],
  toggleFullscreen: ['Shift+Space'],
  exitFullscreen: ['Escape'],
})

export const selectionMouseBindings = defineKeybinds('selection', {
  replace: ['PointerMain'],
  add: ['Mod+Shift+PointerMain'],
  remove: ['Shift+Alt+PointerMain'],
  toggle: ['Shift+PointerMain'],
  invert: ['Mod+Shift+Alt+PointerMain'],
})

export const nodeEditBindings = defineKeybinds('node-edit', {
  cancel: ['Escape'],
  edit: ['Mod+PointerMain'],
})
