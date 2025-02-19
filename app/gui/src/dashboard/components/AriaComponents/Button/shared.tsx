/** @file Context for a button group. */
import { createContext, useContext, type PropsWithChildren } from 'react'
import type { ButtonGroupSharedButtonProps, PrivateJoinedButtonProps } from './types'
import { type ButtonVariants } from './variants'

/**
 * Context for a button group.
 * Allows to specify unified styles for a group of buttons
 * Or provide additional information, like if the buttons are joined
 */
export interface ButtonGroupContextType extends ButtonGroupSharedButtonProps {}

const ButtonGroupContext = createContext<ButtonGroupContextType>({})

/**
 * Provider for a button group context
 */
export function ButtonGroupProvider(props: ButtonGroupContextType & PropsWithChildren) {
  const { children, ...rest } = props

  return <ButtonGroupContext.Provider value={rest}>{children}</ButtonGroupContext.Provider>
}

const EMPTY_CONTEXT: ButtonGroupContextType = {}

/**
 * A wrapper that resets the button group context
 */
export function ResetButtonGroupContext(props: PropsWithChildren) {
  const { children } = props

  return (
    <ButtonGroupContext.Provider value={EMPTY_CONTEXT}>
      <JoinedButtonPrivateContextProvider isJoined={false} position={undefined}>
        {children}
      </JoinedButtonPrivateContextProvider>
    </ButtonGroupContext.Provider>
  )
}

/**
 * Hook to use the button group context
 */
export function useButtonGroupContext() {
  return useContext(ButtonGroupContext)
}

/**
 * Hook to merge button styles with the button group context
 */
export function useMergedButtonStyles<Props extends ButtonVariants>(props: Props) {
  const context = useButtonGroupContext()

  return { ...context, ...props }
}

const JoinedButtonPrivateContext = createContext<PrivateJoinedButtonProps>({
  isJoined: false,
  position: undefined,
})

/**
 * A provider for the joined button private context
 */
export function JoinedButtonPrivateContextProvider(
  props: PrivateJoinedButtonProps & PropsWithChildren,
) {
  const { children, isJoined, position } = props

  return (
    <JoinedButtonPrivateContext.Provider value={{ isJoined, position }}>
      {children}
    </JoinedButtonPrivateContext.Provider>
  )
}

/**
 * Hook to get the joined button private context
 */
export function useJoinedButtonPrivateContext() {
  return useContext(JoinedButtonPrivateContext)
}
