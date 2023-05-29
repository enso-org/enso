/** @file A WebSocket-based chat directly to official support on the official Discord server. */
import * as react from 'react'
import * as reactDom from 'react-dom'

import * as animations from '../../animations'
import * as authProvider from '../../authentication/providers/auth'
import * as loggerProvider from '../../providers/logger'
import * as newtype from '../../newtype'
import * as svg from '../../components/svg'

import Twemoji from './twemoji'

// =================
// === Constants ===
// =================

// TODO[sb]: Consider associating a project with a thread
// (and providing a button to jump to the relevant project).
// The project shouldn't be jumped to automatically, since it may take a long time
// to switch projects, and undo history may be lost.

const HELP_CHAT_ID = 'enso-chat'
export const ANIMATION_DURATION_MS = 1000
const WIDTH_PX = 300
/** The size (both width and height) of each reaction button. */
const REACTION_BUTTON_SIZE = 20
/** The size (both width and height) of each reaction on a message. */
const REACTION_SIZE = 16
// These must be `as const` for type-safety when using the `Twemoji` component.
/** The list of reaction emojis, in order. */
// eslint-disable-next-line no-restricted-syntax
const REACTION_EMOJIS = ['❤️', '👍', '👎', '😀', '🙁', '👀', '🎉'] as const
/** The initial title of the thread. */
const DEFAULT_THREAD_TITLE = 'New chat thread'

// =====================
// === Message Types ===
// =====================

// FIXME[sb]: Consider deduplicating.

// Intentionally the same as in `database.ts`; this one is intended to be copied to the frontend.
/** A Discord thread ID. */
export type ThreadId = newtype.Newtype<string, 'ThreadId'>
/** A Discord message ID. */
export type MessageId = newtype.Newtype<string, 'MessageId'>

/** Types of chat message (both server and client messages). */
export enum ChatMessageDataType {
    // Messages from the server to the client.
    /** Metadata for all threads associated with a user. */
    serverThreads = 'serverThreads',
    /** Metadata for the currently open thread. */
    serverThread = 'serverThread',
    /** A message from the server to the client. */
    serverMessage = 'serverMessage',
    /** An edited message from the server to the client. */
    serverEditedMessage = 'serverEditedMessage',
    // Messages from the client to the server.
    /** The authentication token. */
    authenticate = 'authenticate',
    /** Create a new thread with an initial message. */
    newThread = 'newThread',
    /** Rename an existing thread. */
    renameThread = 'renameThread',
    /** Change the currently active thread. */
    switchThread = 'switchThread',
    /** A message from the client to the server. */
    message = 'message',
    /** A reaction from the client. */
    reaction = 'reaction',
    /** Mark a message as read. Used to determine whether to show the notification dot
     * next to a thread. */
    markAsRead = 'markAsRead',
}

/** Properties common to all chat messages. */
interface ChatBaseMessageData<Type extends ChatMessageDataType> {
    type: Type
}

// ======================================
// === Messages from server to client ===
// ======================================

/** Basic metadata for a single thread. */
export interface ThreadData {
    title: string
    id: string
    hasUnreadMessages: boolean
}

/** Basic metadata for a all of a user's threads. */
export interface ChatServerThreadsMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverThreads> {
    threads: ThreadData[]
}

/** Thread details and recent messages.
 * This message is sent every time the user switches threads. */
export interface ChatServerThreadMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverThread> {
    title: string
    id: ThreadId
    // FIXME: decide on the format for serialized messages.
    messages: ChatServerMessageData[]
}

/** A regular chat message from the server to the client. */
export interface ChatServerMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverMessage> {
    id: string
    timestamp: number
    content: string
}

/** A regular edited chat message from the server to the client. */
export interface ChatServerEditedMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverEditedMessage> {
    id: string
    timestamp: number
    content: string
}

/** A message from the server to the client. */
export type ChatServerMessageData =
    | ChatServerEditedMessageMessageData
    | ChatServerMessageMessageData
    | ChatServerThreadMessageData
    | ChatServerThreadsMessageData

// ======================================
// === Messages from client to server ===
// ======================================

/** Sent when the user first opens the chat sidebar. */
export interface ChatAuthenticateMessageData
    extends ChatBaseMessageData<ChatMessageDataType.authenticate> {
    accessToken: string
}

/** Sent when the user sends a message in a new thread. */
export interface ChatNewThreadMessageData
    extends ChatBaseMessageData<ChatMessageDataType.newThread> {
    title: string
    /** Content of the first message, to reduce the number of round trips. */
    content: string
}

/** Sent when the user finishes editing the thread name in the chat title bar. */
export interface ChatRenameThreadMessageData
    extends ChatBaseMessageData<ChatMessageDataType.renameThread> {
    title: string
    threadId: ThreadId
}

/** Sent when the user picks a thread from the dropdown. */
export interface ChatSwitchThreadMessageData
    extends ChatBaseMessageData<ChatMessageDataType.switchThread> {
    threadId: ThreadId
}

/** A regular message from the client to the server. */
export interface ChatMessageMessageData extends ChatBaseMessageData<ChatMessageDataType.message> {
    threadId: ThreadId
    content: string
}

/** A reaction to a message sent by staff. */
export interface ChatReactionMessageData extends ChatBaseMessageData<ChatMessageDataType.reaction> {
    threadId: ThreadId
    messageId: MessageId
    reaction: string
}

/** Sent when the user scrolls to the bottom of a chat thread. */
export interface ChatMarkAsReadMessageData
    extends ChatBaseMessageData<ChatMessageDataType.markAsRead> {
    threadId: ThreadId
    messageId: MessageId
}

/** A message from the client to the server. */
export type ChatClientMessageData =
    | ChatAuthenticateMessageData
    | ChatMarkAsReadMessageData
    | ChatMessageMessageData
    | ChatNewThreadMessageData
    | ChatReactionMessageData
    | ChatRenameThreadMessageData
    | ChatSwitchThreadMessageData

// ===================
// === ReactionBar ===
// ===================

/** Props for a {@link ReactionBar}. */
export interface ReactionBarProps {
    threadId: string
    messageId: string
    sendMessage: (message: ChatClientMessageData) => void
}

/** A list of emoji reactions to choose from. */
function ReactionBar(props: ReactionBarProps) {
    const { threadId, messageId, sendMessage } = props

    return (
        <div className="inline-block bg-white rounded-full m-1">
            {REACTION_EMOJIS.map(emoji => (
                <button
                    onClick={() => {
                        sendMessage({
                            type: ChatMessageDataType.reaction,
                            threadId,
                            messageId,
                            reaction: emoji,
                        })
                    }}
                    className="rounded-full hover:bg-gray-200 m-1 p-1"
                >
                    <Twemoji key={emoji} emoji={emoji} size={REACTION_BUTTON_SIZE} />
                </button>
            ))}
        </div>
    )
}

// =================
// === Reactions ===
// =================

/** Props for a {@link Reactions}. */
export interface ReactionsProps {
    reactions: Reaction[]
}

/** A list of emoji reactions that have been on a message. */
function Reactions(props: ReactionsProps) {
    const { reactions } = props

    if (reactions.length === 0) {
        return null
    } else {
        return (
            <div>
                {reactions.map(reaction => (
                    <Twemoji emoji={reaction} size={REACTION_SIZE} />
                ))}
            </div>
        )
    }
}

// ====================
// === ChatMessage ===
// ====================

/** Props for a {@link ChatMessage}. */
export interface ChatMessageProps {
    threadId: string
    message: ChatDisplayMessage
    reactions: Reaction[]
    shouldShowReactionBar: boolean
    sendMessage: (message: ChatClientMessageData) => void
}

/** A chat message, including user info, sent date, and reactions (if any). */
function ChatMessage(props: ChatMessageProps) {
    const { threadId, message, reactions, shouldShowReactionBar, sendMessage } = props
    return (
        <div>
            <div>
                <img src={message.avatar} />
                {message.name}
            </div>
            <div>
                {message.content}
                <Reactions reactions={reactions} />
            </div>
            {shouldShowReactionBar && (
                <ReactionBar threadId={threadId} messageId={message.id} sendMessage={sendMessage} />
            )}
        </div>
    )
}

// ============
// === Chat ===
// ============

/** Props for a {@link Chat}. */
export interface ChatProps {
    /** This should only be false when the panel is closing. */
    isOpen: boolean
    doClose: () => void
}

/** Chat sidebar. */
function Chat(props: ChatProps) {
    const { isOpen, doClose } = props
    const { accessToken } = authProvider.useFullUserSession()
    const logger = loggerProvider.useLogger()

    const [isPaidUser, setIsPaidUser] = react.useState(true)
    const [messages, setMessages] = react.useState<ChatDisplayMessage[]>([])
    const [threadId, setThreadId] = react.useState<string | null>(null)
    const [threadTitle, setThreadTitle] = react.useState(DEFAULT_THREAD_TITLE)
    const [isThreadTitleEditable, setIsThreadTitleEditable] = react.useState(false)
    // TODO: proper URL
    const [websocket] = react.useState(() => new WebSocket('ws://localhost:8082'))
    const [right, setTargetRight] = animations.useInterpolateOverTime(
        animations.interpolationFunctionEaseInOut,
        ANIMATION_DURATION_MS,
        -WIDTH_PX
    )
    // These will never be `null` as their values are set immediately.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const titleInput = react.useRef<HTMLInputElement>(null!)
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const messageInput = react.useRef<HTMLInputElement>(null!)

    react.useEffect(() => {
        setIsPaidUser(false)
        return () => {
            websocket.close()
        }
    }, [])

    react.useEffect(() => {
        const onMessage = (data: MessageEvent) => {
            if (typeof data.data !== 'string') {
                logger.error('Chat cannot handle binary messages.')
            } else {
                // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                const message: ChatServerMessageData = JSON.parse(data.data)
                switch (message.type) {
                    case ChatMessageDataType.serverMessage: {
                        setMessages([
                            ...messages,
                            { id: message.messageId, timestamp: message.timestamp },
                        ])
                    }
                }
            }
        }
        const onOpen = () => {
            sendMessage({
                type: ChatMessageDataType.authenticate,
                accessToken,
            })
        }
        websocket.addEventListener('message', onMessage)
        websocket.addEventListener('open', onOpen)
        return () => {
            websocket.removeEventListener('message', onMessage)
            websocket.removeEventListener('open', onOpen)
        }
    }, [websocket])

    const container = document.getElementById(HELP_CHAT_ID)

    react.useEffect(() => {
        // The types come from a third-party API and cannot be changed.
        // eslint-disable-next-line no-restricted-syntax
        let handle: number | undefined
        if (container != null) {
            if (isOpen) {
                container.style.display = ''
                setTargetRight(0)
            } else {
                setTargetRight(-WIDTH_PX)
                handle = window.setTimeout(() => {
                    container.style.display = 'none'
                }, ANIMATION_DURATION_MS)
            }
        }
        return () => {
            clearTimeout(handle)
        }
    }, [isOpen])

    const showThreadList = () => {
        // TODO:
    }

    const sendMessage = (message: ChatClientMessageData) => {
        websocket.send(JSON.stringify(message))
    }

    const sendCurrentMessage = (event: react.FormEvent) => {
        console.log('a', threadId)
        event.preventDefault()
        if (threadId == null) {
            sendMessage({
                type: ChatMessageDataType.newThread,
                title: threadTitle,
                content: messageInput.current.value,
            })
        }
        messageInput.current.value = ''
    }

    const createNewThread = () => {
        console.log('b')
        sendMessage({
            type: ChatMessageDataType.newThread,
            title: threadTitle,
            content: messageInput.current.value,
        })
        setMessages([])
    }

    const stopEditing = (event: react.KeyboardEvent) => {
        if (!event.ctrlKey && !event.altKey && !event.metaKey && !event.shiftKey) {
            if (event.key === 'Escape') {
                titleInput.current.value = threadTitle
                setIsThreadTitleEditable(false)
            } else if (event.key === 'Enter') {
                if (threadId) {
                    sendMessage({
                        type: ChatMessageDataType.renameThread,
                        threadId: threadId,
                        title: threadTitle,
                    })
                }
                setThreadTitle(titleInput.current.value)
                setIsThreadTitleEditable(false)
            }
        }
    }

    const onThreadTitleClick = (event: react.MouseEvent) => {
        if (event.ctrlKey && !event.altKey && !event.metaKey && !event.shiftKey) {
            setIsThreadTitleEditable(true)
        }
    }

    const upgradeToPro = () => {
        // TODO:
    }

    if (container == null) {
        console.error('Chat container not found.')
        return null
    } else {
        // This should be `findLast`, but that requires ES2023.
        const lastStaffMessage = messages.reverse().find(message => message.isStaffMessage)

        return reactDom.createPortal(
            <div
                style={{ right }}
                className="text-xs text-primary flex flex-col fixed top-0 right-0 h-screen bg-ide-bg border-ide-bg-dark border-l-2 w-80 p-1"
                onKeyDown={stopEditing}
            >
                <div className="flex">
                    <button
                        className="flex grow items-center text-sm font-semibold"
                        onClick={showThreadList}
                    >
                        {svg.DOWN_ARROW_ICON}{' '}
                        {/* TODO: reset to current value when editing canceled */}
                        <input
                            type="text"
                            ref={titleInput}
                            disabled={!isThreadTitleEditable}
                            defaultValue={threadTitle}
                            onClick={onThreadTitleClick}
                        />
                    </button>
                    <button onClick={doClose}>{svg.LARGE_CLOSE_ICON}</button>
                </div>
                <div className="grow">
                    {threadId != null &&
                        messages.map(message => (
                            <ChatMessage
                                key={message.id}
                                threadId={threadId}
                                message={message}
                                reactions={[]}
                                sendMessage={sendMessage}
                                shouldShowReactionBar={message === lastStaffMessage}
                            />
                        ))}
                </div>
                <div className="rounded-xl bg-white p-2 m-1">
                    <form onSubmit={sendCurrentMessage}>
                        <div>
                            <input
                                ref={messageInput}
                                required
                                type="text"
                                placeholder="Type your message..."
                                className="w-full rounded-full"
                            ></input>
                            <div className="flex">
                                <button type="button" className="grow" onClick={createNewThread}>
                                    New question? Click to start a new thread!
                                </button>
                                <button type="submit">Reply!</button>
                            </div>
                        </div>
                    </form>
                </div>
                {!isPaidUser && (
                    <button
                        className="rounded-xl bg-call-to-action text-white p-2 m-1"
                        onClick={upgradeToPro}
                    >
                        Click here to upgrade to Enso Pro and get access to high-priority, live
                        support!
                    </button>
                )}
            </div>,
            container
        )
    }
}

export default Chat
