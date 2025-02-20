import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconMetadata/iconName'
import type { ToValue } from '@/util/reactivity'
import type { Ref } from 'vue'

export interface Button {
  iconStyle?: Record<string, string>
  title?: string
  dataTestid?: string
  icon: Icon | URLString
}

export interface ActionButton extends Button {
  onClick: () => void
  disabled?: ToValue<boolean>
}

export interface ToggleButton extends Button {
  toggle: Ref<boolean>
  disabled?: ToValue<boolean>
}

export interface SelectionMenuOption extends Button {
  label?: string
}

export interface SelectionMenu {
  selected: Ref<string>
  title?: string
  options: Record<string, SelectionMenuOption>
}

export interface TextSelectionMenuOption {
  title?: string
  dataTestid?: string
  label: string
}

export interface TextSelectionMenu {
  type: 'textSelectionMenu'
  selectedTextOption: Ref<string>
  title?: string
  options: Record<string, TextSelectionMenuOption>
  heading: string
}

export type ToolbarItem = ActionButton | ToggleButton | SelectionMenu | TextSelectionMenu

/** {@link ActionButton} discriminant */
export function isActionButton(item: Readonly<ToolbarItem>): item is ActionButton {
  return 'onClick' in item
}

/** {@link ToggleButton} discriminant */
export function isToggleButton(item: Readonly<ToolbarItem>): item is ToggleButton {
  return 'toggle' in item
}

/** {@link SelectionMenu} discriminant */
export function isSelectionMenu(item: Readonly<ToolbarItem>): item is SelectionMenu {
  return 'selected' in item
}

/** {@link SelectionTextMenu} discriminant */
export function isTextSelectionMenu(item: Readonly<ToolbarItem>): item is TextSelectionMenu {
  return 'selectedTextOption' in item
}
