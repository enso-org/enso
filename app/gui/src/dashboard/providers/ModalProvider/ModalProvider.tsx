/**
 * @file The React provider for modals, along with hooks to use the provider via
 * the shared React context.
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
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
import { ModalContext, ModalStaticContext, type Modal } from './constants'

/** Props for a {@link ModalProvider}. */
export type ModalProviderProps = Readonly<PropsWithChildren>

/** A React provider containing the currently active modal. */
export function ModalProvider(props: ModalProviderProps) {
  const { children } = props
  const [modal, setModal] = useState<Modal | null>(null)
  // We use keys to tell react to invalidate the DialogTrigger when we change the modal.
  const [key, setKey] = useState(0)
  const modalRef = useSyncRef(modal)

  const setModalStableCallback = useEventCallback(
    (nextModal: SetStateAction<JSX.Element | null>) => {
      startTransition(() => {
        setModal(nextModal)
        setKey((currentKey) => currentKey + 1)
      })
    },
  )

  // This is NOT for optimization purposes - this is for debugging purposes,
  // so that a change of `modal` does not trigger VDOM changes everywhere in the page.
  const setModalProvider = useMemo(
    () => (
      <ModalStaticProvider setModal={setModalStableCallback} modalRef={modalRef}>
        {children}
      </ModalStaticProvider>
    ),
    [children, modalRef, setModalStableCallback],
  )

  return <ModalContext.Provider value={{ modal, key }}>{setModalProvider}</ModalContext.Provider>
}

/** Props for a {@link ModalStaticProvider}. */
interface InternalModalStaticProviderProps extends Readonly<PropsWithChildren> {
  readonly setModal: Dispatch<SetStateAction<Modal | null>>
  readonly modalRef: RefObject<Modal>
}

/** A React provider containing a function to set the currently active modal. */
function ModalStaticProvider(props: InternalModalStaticProviderProps) {
  const { setModal, modalRef, children } = props

  return (
    <ModalStaticContext.Provider value={{ setModal, modalRef }}>
      {children}
    </ModalStaticContext.Provider>
  )
}
