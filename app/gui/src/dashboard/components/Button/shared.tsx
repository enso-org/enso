/** @file Context for a button group. */
import { createContext, useContext, type PropsWithChildren, type RefObject } from 'react'
import type { ButtonGroupSharedButtonProps, ButtonProps, PrivateJoinedButtonProps } from './types'
import type { ButtonVariants } from './variants'

/**
 * Context for a button group.
 * Allows to specify unified styles for a group of buttons
 * Or provide additional information, like if the buttons are joined
 */
export interface ButtonGroupContextType extends ButtonGroupSharedButtonProps {}

const ButtonGroupContext = createContext<ButtonGroupContextType>({})

/**
 * Button context, allows passing props using the context API
 */
export type ButtonContextType<IconType extends string> = ButtonProps<IconType> & {
  readonly ref?: RefObject<HTMLButtonElement>
}

// eslint-disable-next-line react-refresh/only-export-components
export const ButtonContext = createContext<ButtonContextType<string> | null>(null)

/**
 * Provider for a button group context
 */
export function ButtonGroupProvider(props: ButtonGroupContextType & PropsWithChildren) {
  const {
    children,
    extraClickZone,
    fullWidth,
    iconOnly,
    iconPosition,
    isActive,
    isDisabled,
    isFocused,
    isJoined,
    isLoading,
    isPressed,
    loaderPosition,
    loading,
    position,
    rounded,
    showIconOnHover,
    size,
    variant,
    variants,
  } = props

  const contextValue = {
    extraClickZone,
    fullWidth,
    iconOnly,
    iconPosition,
    isActive,
    isDisabled,
    isFocused,
    isJoined,
    isLoading,
    isPressed,
    loaderPosition,
    loading,
    position,
    rounded,
    showIconOnHover,
    size,
    variant,
    variants,
  } satisfies ButtonGroupContextType

  return <ButtonGroupContext.Provider value={contextValue}>{children}</ButtonGroupContext.Provider>
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

/** Hook to use the button group context */
// eslint-disable-next-line react-refresh/only-export-components
export function useButtonGroupContext() {
  return useContext(ButtonGroupContext)
}

/** Hook to merge button styles with the button group context */
// eslint-disable-next-line react-refresh/only-export-components
export function useMergedButtonStyles<Props extends ButtonVariants>(props: Props) {
  const context = useButtonGroupContext()

  return { ...context, ...props }
}

const JoinedButtonPrivateContext = createContext<PrivateJoinedButtonProps>({
  isJoined: false,
  position: undefined,
})

/** A provider for the joined button private context. */
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

/** Hook to get the joined button private context. */
// eslint-disable-next-line react-refresh/only-export-components
export function useJoinedButtonPrivateContext() {
  return useContext(JoinedButtonPrivateContext)
}

/** Hook to get the button context. */
// eslint-disable-next-line react-refresh/only-export-components
export function useButtonContext() {
  return useContext(ButtonContext)
}

/**
 * A wrapper that resets the button context
 */
export function ResetButtonContext(props: PropsWithChildren) {
  const { children } = props

  return <ButtonContext.Provider value={null}>{children}</ButtonContext.Provider>
}
