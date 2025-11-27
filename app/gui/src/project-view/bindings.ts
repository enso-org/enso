import { isMacLike } from '@/composables/events'
import { defineKeybinds } from '@/util/shortcuts'

// Some debug shortcuts are also defined in electron
// (Look for `registerShortcuts` method).

export const appBindings = defineKeybinds('app', {
  'app.cancel': ['Escape'],
  'app.close': ['Mod+Q'],
})

export const appContainerBindings = defineKeybinds('app-container', {
  'app.closeTab':
    // An alternative shortcut is required because Mod+W cannot be overridden in browsers.
    ['Mod+W', 'Mod+Alt+W', ...(!isMacLike ? ['Mod+F4' as const] : [])],
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
  'textEditor.moveLeft': [{ key: 'ArrowLeft', allowRepeat: true }],
  'textEditor.moveRight': [{ key: 'ArrowRight', allowRepeat: true }],
  'textEditor.deleteBack': [{ key: 'Backspace', allowRepeat: true }],
  'textEditor.deleteForward': [{ key: 'Delete', allowRepeat: true }],
  'textEditor.cut': ['Mod+X'],
  'textEditor.copy': ['Mod+C'],
  'textEditor.paste': ['Mod+V'],
  'textEditor.pasteRaw': ['Mod+Shift+V'],
})

export const textEditorsMultilineBindings = defineKeybinds('text-editors-multiline-bindings', {
  'textEditor.newline': ['Alt+Enter'],
})

export const listBindings = defineKeybinds('list', {
  'list.moveUp': [{ key: 'ArrowUp', allowRepeat: true }],
  'list.moveDown': [{ key: 'ArrowDown', allowRepeat: true }],
  'list.accept': ['Enter'],
})

export const componentBrowserBindings = defineKeybinds('component-browser', {
  'componentBrowser.editSuggestion': ['Shift+Enter'],
  'componentBrowser.acceptSuggestion': ['Enter'],
  'componentBrowser.acceptInputAsCode': ['Enter'],
  'componentBrowser.switchToCodeEditMode': ['Mod+Tab'],
  'componentBrowser.acceptInput': ['Mod+Enter'],
  'componentBrowser.acceptAIPrompt': ['Enter'],
  'componentBrowser.switchPanelFocus': ['Tab'],
})

export const graphBindings = defineKeybinds('graph-editor', {
  'graph.toggleCodeEditor': ['Mod+`'],
  'graph.toggleDocumentationEditor': ['Mod+D'],
  'graph.undo': ['Mod+Z'],
  // On Mac, `Mod+Shift+Z` takes priority and will be displayed in the tooltip.
  'graph.redo': isMacLike ? ['Mod+Shift+Z', 'Mod+Y'] : ['Mod+Y', 'Mod+Shift+Z'],
  'graph.openComponentBrowser': ['Enter'],
  'graph.toggleVisualization': ['Space'],
  'components.deleteSelected': ['Delete', 'Backspace'],
  'graph.fitAll': ['Mod+Shift+A'],
  'graph.selectAll': ['Mod+A'],
  'graph.deselectAll': ['Escape'],
  'components.copy': ['Mod+C'],
  'graph.pasteNode': ['Mod+V'],
  'components.collapse': ['Mod+G'],
  'graph.startProfiling': ['Mod+Alt+,'],
  'graph.stopProfiling': ['Mod+Alt+.'],
  'component.enterNode': ['Mod+E'],
  'graph.navigateUp': ['Mod+Shift+E'],
  'components.pickColorMulti': ['Mod+Shift+C'],
  'graph.openDocumentation': ['F1'],
  'graph.deleteSelectedEdge': ['Delete', 'Backspace'],
})

export const visualizationBindings = defineKeybinds('visualization', {
  'visualization.nextType': ['Mod+Space'],
  'panel.fullscreen': ['Shift+Space'],
  'visualization.exitFullscreen': ['Escape'],
})

export const gridBindings = defineKeybinds('grid', {
  'grid.cutCells': ['Mod+X'],
  'grid.copyCells': ['Mod+C'],
  'grid.pasteCells': ['Mod+V'],
})

export const commandPaletteBindings = defineKeybinds('command-palette', {
  'commandPalette.open': ['Mod+K'],
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
