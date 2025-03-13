/** @file Constants for `Checkbox`. */
import type { TSchema, UseFormRegisterReturn } from '#/components/AriaComponents/Form'
import { tv } from '#/utilities/tailwindVariants'
import { createContext } from 'react'
import { createStore, type StoreApi } from 'zustand'

export const CHECKBOX_STYLES = tv({
  base: 'group flex gap-2 items-center cursor-pointer select-none',
  variants: {
    isInvalid: {
      true: {
        base: 'text-danger',
        icon: 'border-danger focus-within:border-danger focus-within:outline-danger',
      },
    },
    isReadOnly: {
      true: { icon: 'bg-primary/50 border-primary/50' },
    },
    isDisabled: {
      true: { icon: 'bg-primary/30 border-primary/30 cursor-not-allowed' },
      false: '',
    },
    isSelected: {
      true: { icon: 'bg-primary text-white' },
      false: { icon: 'bg-transparent text-primary' },
    },
    size: { medium: { icon: 'w-4 h-4' } },
  },
  slots: {
    icon: [
      'border-[0.5px] rounded-md transition-[outline-offset,border-width] duration-200',
      'outline -outline-offset-2 outline-transparent group-focus-visible:outline-offset-0 group-focus-visible:outline-primary',
      'border-primary group-selected:border-transparent',
      'group-pressed:border',
      'shrink-0',
    ],
  },
  defaultVariants: {
    size: 'medium',
  },
  compoundVariants: [
    {
      isInvalid: true,
      isSelected: true,
      class: {
        icon: 'bg-danger border-danger focus-within:border-danger focus-within:outline-danger',
      },
    },
  ],
})

/** Checkbox group state when the checkbox is inside a group. */
export interface CheckBoxGroupPropsStateInsideGroup {
  readonly insideGroup: true
  readonly selected: Set<string>
  readonly name: string
  readonly field: UseFormRegisterReturn<TSchema>
}

/** Checkbox group state when the checkbox is not inside a group. */
interface CheckBoxGroupPropsStateOutsideGroup {
  readonly insideGroup: false
}

/**
 * State for a checkbox group.
 * If `insideGroup` is `true`, then the checkbox is inside a group, and `selected` and `setSelected` will be defined.
 * If `insideGroup` is `false`, then the checkbox is not inside a group, and `selected` and `setSelected` will be `undefined`.
 * that means that the checkbox should be controlled manually.
 */
export type CheckGroupPropsState =
  | CheckBoxGroupPropsStateInsideGroup
  | CheckBoxGroupPropsStateOutsideGroup

/** Context for the checkbox. */
export interface CheckboxContextType {
  readonly store: StoreApi<CheckGroupPropsState>
  readonly addSelected: (selected: string) => void
  readonly removeSelected: (selected: string) => void
  readonly toggleSelected: (selected: string) => void
}

export const CheckboxContext = createContext<CheckboxContextType>({
  store: createStore<CheckBoxGroupPropsStateOutsideGroup>(() => ({ insideGroup: false })),
  addSelected: () => {},
  removeSelected: () => {},
  toggleSelected: () => {},
})
