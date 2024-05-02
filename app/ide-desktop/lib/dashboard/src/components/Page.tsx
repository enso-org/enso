/** @file A page. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'

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
  const session = authProvider.useNonPartialUserSession()

  const doCloseChat = () => {
    setIsHelpChatOpen(false)
  }

  return (
    <>
      {children}
      {!hideInfoBar && (
        <InfoBar isHelpChatOpen={isHelpChatOpen} setIsHelpChatOpen={setIsHelpChatOpen} />
      )}
      {/* `session.accessToken` MUST be present in order for the `Chat` component to work. */}
      {!hideInfoBar && session.accessToken != null && process.env.ENSO_CLOUD_CHAT_URL != null ? (
        <Chat
          isOpen={isHelpChatOpen}
          doClose={doCloseChat}
          endpoint={process.env.ENSO_CLOUD_CHAT_URL}
        />
      ) : (
        <ChatPlaceholder isOpen={isHelpChatOpen} doClose={doCloseChat} />
      )}
      <Portal>
        <div className="select-none text-xs text-primary">
          <TheModal />
        </div>
      </Portal>
    </>
  )
}
