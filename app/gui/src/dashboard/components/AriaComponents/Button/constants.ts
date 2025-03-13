/** @file Constants for `Button`. */
import { createContext, type RefObject } from 'react'
import type { ButtonGroupSharedButtonProps, ButtonProps, PrivateJoinedButtonProps } from './types'

/**
 * Context for a button group.
 * Allows to specify unified styles for a group of buttons
 * Or provide additional information, like if the buttons are joined
 */
export interface ButtonGroupContextType extends ButtonGroupSharedButtonProps {}

export const ButtonGroupContext = createContext<ButtonGroupContextType>({})

/** Button context that allows passing props using the context API. */
export type ButtonContextType<IconType extends string> = ButtonProps<IconType> & {
  readonly ref?: RefObject<HTMLButtonElement>
}

export const ButtonContext = createContext<ButtonContextType<string> | null>(null)

export const JoinedButtonPrivateContext = createContext<PrivateJoinedButtonProps>({
  isJoined: false,
  position: undefined,
})
