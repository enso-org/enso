/** @file Hooks for `Button`. */
import { useContext } from 'react'
import { ButtonContext, ButtonGroupContext, JoinedButtonPrivateContext } from './constants'
import type { ButtonVariants } from './variants'

/** Hook to use the button group context. */
export function useButtonGroupContext() {
  return useContext(ButtonGroupContext)
}

/** Hook to merge button styles with the button group context. */
export function useMergedButtonStyles<Props extends ButtonVariants>(props: Props) {
  const context = useButtonGroupContext()

  return { ...context, ...props }
}

/** Hook to get the joined button private context. */
export function useJoinedButtonPrivateContext() {
  return useContext(JoinedButtonPrivateContext)
}

/** Hook to get the button context. */
export function useButtonContext() {
  return useContext(ButtonContext)
}
