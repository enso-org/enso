/** @file A page. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'

import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import InfoBar from '#/layouts/InfoBar'

import TheModal from '#/components/dashboard/TheModal'
import Portal from '#/components/Portal'

// ============
// === Page ===
// ============

/** Props for a {@link Page}. */
export interface PageProps extends Readonly<React.PropsWithChildren> {
  readonly hideInfoBar?: true
}

/** A page. */
export default function Page(props: PageProps) {
  const { hideInfoBar = false, children } = props
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)
  const { unsetModal } = modalProvider.useSetModal()
  const session = authProvider.useUserSession()

  const doCloseChat = () => {
    setIsHelpChatOpen(false)
  }

  React.useEffect(() => {
    const onClick = () => {
      if (getSelection()?.type !== 'Range') {
        unsetModal()
      }
    }
    document.addEventListener('click', onClick)
    return () => {
      document.removeEventListener('click', onClick)
    }
  }, [/* should never change */ unsetModal])

  return (
    <>
      {children}
      {!hideInfoBar && (
        <div className="m-top-bar fixed right top z-1 text-xs text-primary transition-all duration-side-panel">
          <InfoBar isHelpChatOpen={isHelpChatOpen} setIsHelpChatOpen={setIsHelpChatOpen} />
        </div>
      )}
      {/* `session.accessToken` MUST be present in order for the `Chat` component to work. */}
      {!hideInfoBar && session?.accessToken != null && process.env.ENSO_CLOUD_CHAT_URL != null ? (
        <Chat
          isOpen={isHelpChatOpen}
          doClose={doCloseChat}
          endpoint={process.env.ENSO_CLOUD_CHAT_URL}
        />
      ) : (
        <ChatPlaceholder hideLoginButtons isOpen={isHelpChatOpen} doClose={doCloseChat} />
      )}
      <Portal>
        <div className="select-none text-xs text-primary">
          <TheModal />
        </div>
      </Portal>
    </>
  )
}
