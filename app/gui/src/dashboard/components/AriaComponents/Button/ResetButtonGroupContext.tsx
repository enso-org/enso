import { ButtonGroupContext } from '#/components/AriaComponents/Button/constants'
import {
  EMPTY_CONTEXT,
  JoinedButtonPrivateContextProvider,
} from '#/components/AriaComponents/Button/shared'
import type { PropsWithChildren } from 'react'

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
