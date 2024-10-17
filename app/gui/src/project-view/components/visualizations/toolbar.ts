import { URLString } from '@/util/data/urlString'
import { Icon } from '@/util/iconName'
import { ToValue } from '@/util/reactivity'
import { Ref } from 'vue'

export interface Button {
  iconStyle?: Record<string, string>
  title?: string
  dataTestid?: string
}

export interface ActionButton extends Button {
  onClick: () => void
  disabled?: ToValue<boolean>
  icon: Icon | URLString
}

export interface ToggleButton extends Button {
  toggle: Ref<boolean>
  disabled?: ToValue<boolean>
  icon: Icon | URLString
}

export interface SelectionMenuOption extends Button {
  label?: string
  icon?: Icon | URLString
}

export interface SelectionMenu {
  selected: Ref<string>
  title?: string
  options: Record<string, SelectionMenuOption>
  isTextDropdown?: boolean
  heading?: string
}

export type ToolbarItem = ActionButton | ToggleButton | SelectionMenu

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
