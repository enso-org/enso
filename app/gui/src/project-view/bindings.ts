import { isMacLike } from '@/composables/events'
import { defineKeybinds } from '@/util/shortcuts'

export const undoBindings = defineKeybinds('undo', {
  'graph.undo': ['Mod+Z'],
  // On Mac, `Mod+Shift+Z` takes priority and will be displayed in the tooltip.
  'graph.redo': isMacLike ? ['Mod+Shift+Z', 'Mod+Y'] : ['Mod+Y', 'Mod+Shift+Z'],
})

export const panelsBindings = defineKeybinds('panels', {
  'graph.toggleCodeEditor': ['Mod+`'],
  'graph.toggleDocumentationEditor': ['Mod+D'],
})

export const documentationEditorBindings = defineKeybinds('documentation-editor', {
  'documentationEditor.paste': ['Mod+V'],
  'documentationEditor.pasteRaw': ['Mod+Shift+V'],
})

export const documentationEditorFormatBindings = defineKeybinds('documentation-editor-formatting', {
  'documentationEditor.italic': ['Mod+I'],
  'documentationEditor.bold': ['Mod+B'],
  'documentationEditor.link': ['Mod+K'],
  'documentationEditor.paragraph': ['Mod+Alt+0'],
  'documentationEditor.header1': ['Mod+Alt+1'],
  'documentationEditor.header2': ['Mod+Alt+2'],
  'documentationEditor.header3': ['Mod+Alt+3'],
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
  'componentBrowser.editSuggestion': ['Shift+Enter'],
  'componentBrowser.acceptSuggestion': ['Enter'],
  'componentBrowser.acceptInputAsCode': ['Enter'],
  'componentBrowser.switchToCodeEditMode': ['Mod+Tab'],
  acceptInput: ['Mod+Enter'],
  acceptAIPrompt: ['Enter'],
  switchPanelFocus: ['Tab'],
})

export const graphBindings = defineKeybinds('graph-editor', {
  openComponentBrowser: ['Enter'],
  toggleVisualization: ['Space'],
  'components.deleteSelected': ['Delete', 'Backspace'],
  'graph.fitAll': ['Mod+Shift+A'],
  selectAll: ['Mod+A'],
  deselectAll: ['Escape'],
  'components.copy': ['Mod+C'],
  pasteNode: ['Mod+V'],
  'components.collapse': ['Mod+G'],
  startProfiling: ['Mod+Alt+,'],
  stopProfiling: ['Mod+Alt+.'],
  enterNode: ['Mod+E'],
  'graph.navigateUp': ['Mod+Shift+E'],
  'components.pickColorMulti': ['Mod+Shift+C'],
  openDocumentation: ['F1'],
})

export const visualizationBindings = defineKeybinds('visualization', {
  nextType: ['Mod+Space'],
  toggleFullscreen: ['Shift+Space'],
  exitFullscreen: ['Escape'],
})

export const gridBindings = defineKeybinds('grid', {
  cutCells: ['Mod+X'],
  copyCells: ['Mod+C'],
  pasteCells: ['Mod+V'],
})

// === Mouse bindings ===

export const textEditorsBindings = defineKeybinds('text-editors', {
  openLink: ['Mod+PointerMain'],
})

export const selectionMouseBindings = defineKeybinds('selection', {
  replace: ['PointerMain'],
  add: ['Mod+Shift+PointerMain'],
  remove: ['Shift+Alt+PointerMain'],
  toggle: ['Shift+PointerMain'],
  invert: ['Mod+Shift+Alt+PointerMain'],
})

export const nodeEditBindings = defineKeybinds('node-edit', {
  edit: ['Mod+PointerMain'],
})
