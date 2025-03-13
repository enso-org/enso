/** @file Context for a button group. */
import type { PropsWithChildren } from 'react'
import {
  ButtonContext,
  ButtonGroupContext,
  JoinedButtonPrivateContext,
  type ButtonGroupContextType,
} from './constants'
import type { PrivateJoinedButtonProps } from './types'

/** Provider for a button group context. */
export function ButtonGroupProvider(props: ButtonGroupContextType & Readonly<PropsWithChildren>) {
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

/** A wrapper that resets the button group context. */
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

/** A provider for the joined button private context. */
export function JoinedButtonPrivateContextProvider(
  props: PrivateJoinedButtonProps & Readonly<PropsWithChildren>,
) {
  const { children, isJoined, position } = props

  return (
    <JoinedButtonPrivateContext.Provider value={{ isJoined, position }}>
      {children}
    </JoinedButtonPrivateContext.Provider>
  )
}

/** A wrapper that resets the button context. */
export function ResetButtonContext(props: PropsWithChildren) {
  const { children } = props

  return <ButtonContext.Provider value={null}>{children}</ButtonContext.Provider>
}
