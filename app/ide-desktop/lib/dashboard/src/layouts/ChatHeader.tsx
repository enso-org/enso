/** @file The header bar for a `Chat`. Includes the title, close button,
 * and threads list. */
import * as React from 'react'

import CloseLargeIcon from 'enso-assets/close_large.svg'
import TriangleDownIcon from 'enso-assets/triangle_down.svg'
import * as chat from 'enso-chat/chat'

import * as gtagHooks from '#/hooks/gtagHooks'

import * as object from '#/utilities/object'

// ==================
// === ChatHeader ===
// ==================

/** Props for a {@Link ChatHeader}. */
interface InternalChatHeaderProps {
  readonly threads: chat.ThreadData[]
  readonly setThreads: React.Dispatch<React.SetStateAction<chat.ThreadData[]>>
  readonly threadId: chat.ThreadId | null
  readonly threadTitle: string
  readonly setThreadTitle: (threadTitle: string) => void
  readonly switchThread: (threadId: chat.ThreadId) => void
  readonly sendMessage: (message: chat.ChatClientMessageData) => void
  readonly doClose: () => void
}

/** The header bar for a `Chat`. Includes the title, close button,
 * and threads list. */
export default function ChatHeader(props: InternalChatHeaderProps) {
  const { threads, setThreads, threadId, threadTitle, setThreadTitle } = props
  const { switchThread, sendMessage, doClose } = props
  const gtagEvent = gtagHooks.useGtagEvent()
  const [isThreadListVisible, setIsThreadListVisible] = React.useState(false)
  // These will never be `null` as their values are set immediately.
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const titleInputRef = React.useRef<HTMLInputElement>(null!)

  React.useEffect(() => {
    titleInputRef.current.value = threadTitle
  }, [threadTitle])

  React.useEffect(() => {
    const onClick = () => {
      setIsThreadListVisible(false)
    }
    document.addEventListener('click', onClick)
    gtagEvent('cloud_open_chat')
    return () => {
      document.removeEventListener('click', onClick)
      gtagEvent('cloud_close_chat')
    }
  }, [gtagEvent])

  const toggleThreadListVisibility = React.useCallback((event: React.SyntheticEvent) => {
    event.stopPropagation()
    setIsThreadListVisible(visible => !visible)
  }, [])

  return (
    <>
      <div className="flex text-sm font-semibold mx-4 mt-2">
        <button className="flex grow items-center" onClick={toggleThreadListVisibility}>
          <img
            className={`transition-transform duration-300 shrink-0 ${
              isThreadListVisible ? '-rotate-180' : ''
            }`}
            src={TriangleDownIcon}
          />
          {/* Spacing. */}
          <div className="w-2" />
          <div className="grow">
            <input
              type="text"
              ref={titleInputRef}
              defaultValue={threadTitle}
              className="bg-transparent w-full leading-6"
              onClick={event => {
                event.stopPropagation()
              }}
              onKeyDown={event => {
                switch (event.key) {
                  case 'Escape': {
                    event.currentTarget.value = threadTitle
                    break
                  }
                  case 'Enter': {
                    event.currentTarget.blur()
                    break
                  }
                }
              }}
              onBlur={event => {
                const newTitle = event.currentTarget.value
                setThreadTitle(newTitle)
                if (threadId != null) {
                  setThreads(oldThreads =>
                    oldThreads.map(thread =>
                      thread.id !== threadId ? thread : object.merge(thread, { title: newTitle })
                    )
                  )
                  sendMessage({
                    type: chat.ChatMessageDataType.renameThread,
                    title: newTitle,
                    threadId: threadId,
                  })
                }
              }}
            />
          </div>
        </button>
        <button className="mx-1" onClick={doClose}>
          <img src={CloseLargeIcon} />
        </button>
      </div>
      <div className="relative text-sm font-semibold">
        <div
          className={`grid absolute shadow-soft clip-path-bottom-shadow bg-frame backdrop-blur-3xl overflow-hidden transition-grid-template-rows w-full z-1 ${
            isThreadListVisible ? 'grid-rows-1fr' : 'grid-rows-0fr'
          }`}
        >
          <div className="min-h-0 max-h-70 overflow-y-auto">
            {threads.map(thread => (
              <div
                key={thread.id}
                className={`flex p-1 ${
                  thread.id === threadId
                    ? 'cursor-default bg-frame-selected'
                    : 'cursor-pointer hover:bg-frame'
                }`}
                onClick={event => {
                  event.stopPropagation()
                  if (thread.id !== threadId) {
                    switchThread(thread.id)
                    setIsThreadListVisible(false)
                  }
                }}
              >
                <div className="w-8 text-center">
                  {/* {thread.hasUnreadMessages ? '(!) ' : ''} */}
                </div>
                <div>{thread.title}</div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </>
  )
}
