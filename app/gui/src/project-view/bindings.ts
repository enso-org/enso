import { defineKeybinds } from '@/util/shortcuts'
import { isMacLike } from './composables/events'

export const undoBindings = defineKeybinds('undo', {
  undo: ['Mod+Z'],
  // On Mac, `Mod+Shift+Z` takes priority and will be displayed in the tooltip.
  redo: isMacLike ? ['Mod+Shift+Z', 'Mod+Y'] : ['Mod+Y', 'Mod+Shift+Z'],
})

export const codeEditorBindings = defineKeybinds('code-editor', {
  toggle: ['Mod+`'],
})

export const documentationEditorBindings = defineKeybinds('documentation-editor', {
  toggle: ['Mod+D'],
  paste: ['Mod+V'],
  pasteRaw: ['Mod+Shift+V'],
})

export const textEditorsBindings = defineKeybinds('text-editors', {
  openLink: ['Mod+PointerMain'],
})

export const textEditorsCommonBindings = defineKeybinds('text-editors-common-bindings', {
  moveLeft: ['ArrowLeft'],
  moveRight: ['ArrowRight'],
  deleteBack: ['Backspace'],
  deleteForward: ['Delete'],
})

export const textEditorsMultilineBindings = defineKeybinds('text-editors-multiline-bindings', {
  moveUp: ['ArrowUp'],
  moveDown: ['ArrowDown'],
  newline: ['Alt+Enter'],
})

export const listBindings = defineKeybinds('list', {
  moveUp: ['ArrowUp'],
  moveDown: ['ArrowDown'],
  accept: ['Enter'],
})

export const interactionBindings = defineKeybinds('current-interaction', {
  cancel: ['Escape'],
})

export const componentBrowserBindings = defineKeybinds('component-browser', {
  applySuggestion: ['Shift+Enter'],
  acceptSuggestion: ['Enter'],
  acceptCode: ['Enter'],
  acceptInput: ['Mod+Enter'],
  acceptAIPrompt: ['Enter'],
  switchPanelFocus: ['Tab'],
  switchToCodeEditMode: ['Mod+Tab'],
})

export const graphBindings = defineKeybinds('graph-editor', {
  openComponentBrowser: ['Enter'],
  toggleVisualization: ['Space'],
  deleteSelected: ['Delete', 'Backspace'],
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
  changeColorSelectedNodes: ['Mod+Shift+C'],
  openDocumentation: ['F1'],
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

export const gridBindings = defineKeybinds('grid', {
  cutCells: ['Mod+X'],
  copyCells: ['Mod+C'],
  pasteCells: ['Mod+V'],
})
