import { defineKeybinds } from '@/util/shortcuts'

export const nodeBindings = defineKeybinds('node-selection', {
  selectAll: ['Mod+A'],
  deselectAll: ['Escape'],
  replace: ['PointerMain'],
  add: ['Mod+Shift+PointerMain'],
  remove: ['Shift+Alt+PointerMain'],
  toggle: ['Shift+PointerMain'],
  invert: ['Mod+Shift+Alt+PointerMain'],
})
