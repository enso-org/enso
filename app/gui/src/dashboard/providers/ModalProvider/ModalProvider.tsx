/**
 * @file The React provider for modals, along with hooks to use the provider via
 * the shared React context.
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useStore } from '#/hooks/storeHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import {
  startTransition,
  useMemo,
  useState,
  type Dispatch,
  type JSX,
  type PropsWithChildren,
  type RefObject,
  type SetStateAction,
} from 'react'
import { ModalContext, ModalsStore, ModalStaticContext, type Modal } from './constants'

/** Props for a {@link ModalProvider}. */
export type ModalProviderProps = Readonly<PropsWithChildren>

/** A React provider containing the currently active modal. */
export function ModalProvider(props: ModalProviderProps) {
  const { children } = props

  const modalState = useStore(ModalsStore, (state) => state, {
    areEqual: 'never',
    unsafeEnableTransition: true,
  })

  return (
    <ModalContext.Provider value={{ modal: modalState.modal, key: modalState.key }}>
      <ModalStaticProvider>{children}</ModalStaticProvider>
    </ModalContext.Provider>
  )
}

/** Props for a {@link ModalStaticProvider}. */
interface InternalModalStaticProviderProps extends Readonly<React.PropsWithChildren> {}

/** A React provider containing a function to set the currently active modal. */
function ModalStaticProvider(props: InternalModalStaticProviderProps) {
  const { children } = props

  const modalState = useStore(ModalsStore, (state) => ({ setModal: state.setModal }), {
    areEqual: 'always',
    unsafeEnableTransition: true,
  })

  const modalRef = useStore(ModalsStore, (state) => ({ current: state.modal }), {
    areEqual: 'object',
    unsafeEnableTransition: true,
  })

  return (
    <ModalStaticContext.Provider value={{ setModal: modalState.setModal, modalRef }}>
      {children}
    </ModalStaticContext.Provider>
  )
}
