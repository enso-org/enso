import { URLString } from '@/util/data/urlString'
import { Icon } from '@/util/iconName'
import { ToValue } from '@/util/reactivity'
import { Ref } from 'vue'

export interface Button {
  icon: Icon | URLString
  iconStyle?: Record<string, string>
  title?: string
  dataTestid?: string
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
  selected: Ref<number | string | symbol>
  title?: string
  options: Record<number | string | symbol, SelectionMenuOption>
}

export type ToolbarItem = ActionButton | ToggleButton | SelectionMenu

export function isActionButton(item: Readonly<ToolbarItem>): item is ActionButton {
  return 'onClick' in item
}

export function isToggleButton(item: Readonly<ToolbarItem>): item is ToggleButton {
  return 'toggle' in item
}

export function isSelectionMenu(item: Readonly<ToolbarItem>): item is SelectionMenu {
  return 'selected' in item
}
