/** @file A WebSocket-based chat directly to official support on the official Discord server. */
import * as react from 'react'
import * as reactDom from 'react-dom'
import toast from 'react-hot-toast'

import CloseLargeIcon from 'enso-assets/close_large.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'
import TriangleDownIcon from 'enso-assets/triangle_down.svg'

import * as animations from '../../animations'
import * as authProvider from '../../authentication/providers/auth'
import * as dateTime from '../dateTime'
import * as loggerProvider from '../../providers/logger'
import * as newtype from '../../newtype'

import Twemoji from './twemoji'

// =================
// === Constants ===
// =================

// TODO[sb]: Consider associating a project with a thread
// (and providing a button to jump to the relevant project).
// The project shouldn't be jumped to automatically, since it may take a long time
// to switch projects, and undo history may be lost.

const HELP_CHAT_ID = 'enso-chat'
export const ANIMATION_DURATION_MS = 200
const WIDTH_PX = 352
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

// =============
// === Types ===
// =============

/** All possible reaction emojis. */
type Reaction = (typeof REACTION_EMOJIS)[number]

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
    // Messages internal to the server.
    /** Like the authenticate message, but with user details. */
    internalAuthenticate = 'internal-authenticate',
    // Messages from the server to the client.
    /** Metadata for all threads associated with a user. */
    serverThreads = 'server-threads',
    /** Metadata for the currently open thread. */
    serverThread = 'server-thread',
    /** A message from the server to the client. */
    serverMessage = 'server-message',
    /** An edited message from the server to the client. */
    serverEditedMessage = 'server-edited-message',
    /** A message from the client to the server, sent from the server to the client as part of
     * the message history. */
    serverReplayedMessage = 'server-replayed-message',
    // Messages from the client to the server.
    /** The authentication token. */
    authenticate = 'authenticate',
    /** Create a new thread with an initial message. */
    newThread = 'newThread',
    /** Rename an existing thread. */
    renameThread = 'rename-thread',
    /** Change the currently active thread. */
    switchThread = 'switch-thread',
    /** A message from the client to the server. */
    message = 'message',
    /** A reaction from the client. */
    reaction = 'reaction',
    /** Mark a message as read. Used to determine whether to show the notification dot
     * next to a thread. */
    markAsRead = 'mark-as-read',
}

/** Properties common to all WebSocket messages. */
interface ChatBaseMessageData<Type extends ChatMessageDataType> {
    type: Type
}

// ======================================
// === Messages from server to client ===
// ======================================

/** Basic metadata for a single thread. */
export interface ThreadData {
    title: string
    id: ThreadId
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
    title: MessageId
    id: ThreadId
    // FIXME: decide on the format for serialized messages.
    messages: (ChatServerMessageMessageData | ChatServerReplayedMessageMessageData)[]
}

/** A regular chat message from the server to the client. */
export interface ChatServerMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverMessage> {
    id: MessageId
    authorAvatar: string | null
    authorName: string
    timestamp: number
    content: string
}

/** A regular edited chat message from the server to the client. */
export interface ChatServerEditedMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverEditedMessage> {
    id: MessageId
    timestamp: number
    content: string
}

/** A replayed message from the client to the server. Includes the timestamp of the message. */
export interface ChatServerReplayedMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverReplayedMessage> {
    id: MessageId
    timestamp: number
    content: string
}

/** A message from the server to the client. */
export type ChatServerMessageData =
    | ChatServerEditedMessageMessageData
    | ChatServerMessageMessageData
    | ChatServerReplayedMessageMessageData
    | ChatServerThreadMessageData
    | ChatServerThreadsMessageData

// ======================================
// === Messages from client to server ===
// ======================================

/** Sent whenever the user opens the chat sidebar. */
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

// ==========================
// === ChatDisplayMessage ===
// ==========================

/** Information needed to display a chat message. */
interface ChatDisplayMessage {
    id: MessageId
    /** If `true`, this is a message from the staff to the user.
     * If `false`, this is a message from the user to the staff. */
    isStaffMessage: boolean
    avatar: string | null
    /** Name of the author of the message. */
    name: string
    content: string
    /** Given in milliseconds since the unix epoch. */
    timestamp: number
    /** Given in milliseconds since the unix epoch. */
    editedTimestamp: number | null
}

// ===================
// === ReactionBar ===
// ===================

/** Props for a {@link ReactionBar}. */
export interface ReactionBarProps {
    threadId: ThreadId
    messageId: MessageId
    sendMessage: (message: ChatClientMessageData) => void
    selectedReactions: Set<Reaction>
}

/** A list of emoji reactions to choose from. */
function ReactionBar(props: ReactionBarProps) {
    const { threadId, messageId, sendMessage, selectedReactions } = props

    return (
        <div className="inline-block bg-white rounded-full m-1">
            {REACTION_EMOJIS.map(emoji => (
                <button
                    key={emoji}
                    onClick={() => {
                        sendMessage({
                            type: ChatMessageDataType.reaction,
                            threadId,
                            messageId,
                            reaction: emoji,
                        })
                    }}
                    // FIXME: Grayscale has the wrong lightness
                    className={`rounded-full hover:bg-gray-200 m-1 p-1 ${
                        selectedReactions.has(emoji) ? '' : 'opacity-70 grayscale hover:grayscale-0'
                    }`}
                >
                    <Twemoji key={emoji} emoji={emoji} size={REACTION_BUTTON_SIZE} />
                </button>
            ))}
        </div>
    )
}

// =================
// === Reactions ===
// =================lib/dashboard/src/authentication/src/dashboard/events/directoryListEvent.ts

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

// ===================
// === ChatMessage ===
// ===================

/** Props for a {@link ChatMessage}. */
export interface ChatMessageProps {
    threadId: ThreadId
    message: ChatDisplayMessage
    reactions: Reaction[]
    shouldShowReactionBar: boolean
    sendMessage: (message: ChatClientMessageData) => void
}

/** A chat message, including user info, sent date, and reactions (if any). */
function ChatMessage(props: ChatMessageProps) {
    const { threadId, message, reactions, shouldShowReactionBar, sendMessage } = props
    return (
        <div className="m-1">
            <div className="flex">
                {/* FIXME: This should default to the default user image. */}
                <img
                    crossOrigin="anonymous"
                    src={message.avatar ?? DefaultUserIcon}
                    className="rounded-full h-8 w-8 m-1"
                />
                <div className="m-1">
                    <div className="font-semibold">{message.name}</div>
                    <div className="text-opacity-50 text-primary">
                        {dateTime.formatDateTimeChatFriendly(new Date(message.timestamp))}
                    </div>
                </div>
            </div>
            <div className="mx-1">
                {message.content}
                <Reactions reactions={reactions} />
            </div>
            {shouldShowReactionBar && (
                <ReactionBar
                    threadId={threadId}
                    messageId={message.id}
                    sendMessage={sendMessage}
                    // FIXME:
                    selectedReactions={new Set()}
                />
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
    const { accessToken: rawAccessToken } = authProvider.useNonPartialUserSession()
    const logger = loggerProvider.useLogger()

    /** This is SAFE, because this component is only rendered when `accessToken` is present.
     * See `dashboard.tsx` for its sole usage. */
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const accessToken = rawAccessToken!

    const [isPaidUser, setIsPaidUser] = react.useState(true)
    const [isReplyEnabled, setIsReplyEnabled] = react.useState(false)
    const [threads, setThreads] = react.useState<ThreadData[]>([])
    const [messages, setMessages] = react.useState<ChatDisplayMessage[]>([])
    const [threadId, setThreadId] = react.useState<ThreadId | null>(null)
    const [isThreadListVisible, setIsThreadListVisible] = react.useState(false)
    const [threadTitle, setThreadTitle] = react.useState(DEFAULT_THREAD_TITLE)
    const [isThreadTitleEditable, setIsThreadTitleEditable] = react.useState(false)
    const [isThreadTitleEditingCanceled, setIsThreadTitleEditingCanceled] = react.useState(false)
    // TODO: proper URL
    const [websocket] = react.useState(() => new WebSocket('ws://localhost:8082'))
    const [right, setTargetRight] = animations.useInterpolateOverTime(
        animations.interpolationFunctionEaseInOut,
        ANIMATION_DURATION_MS,
        -WIDTH_PX
    )
    // These will never be `null` as their values are set immediately.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const titleInputRef = react.useRef<HTMLInputElement>(null!)
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
                // This is SAFE, as the format of server messages is known.
                // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                const message: ChatServerMessageData = JSON.parse(data.data)
                switch (message.type) {
                    case ChatMessageDataType.serverThreads: {
                        setThreads(message.threads)
                        break
                    }
                    case ChatMessageDataType.serverThread: {
                        setThreadId(message.id)
                        setThreadTitle(message.title)
                        titleInputRef.current.value = message.title
                        setMessages(
                            message.messages.flatMap(innerMessage => {
                                switch (innerMessage.type) {
                                    case ChatMessageDataType.serverMessage: {
                                        const displayMessage: ChatDisplayMessage = {
                                            id: innerMessage.id,
                                            isStaffMessage: true,
                                            content: innerMessage.content,
                                            avatar: innerMessage.authorAvatar,
                                            name: innerMessage.authorName,
                                            timestamp: innerMessage.timestamp,
                                            editedTimestamp: null, // FIXME:
                                        }
                                        return displayMessage
                                    }
                                    case ChatMessageDataType.serverReplayedMessage: {
                                        const displayMessage: ChatDisplayMessage = {
                                            id: innerMessage.id,
                                            isStaffMessage: false,
                                            content: innerMessage.content,
                                            avatar: null,
                                            name: 'Me',
                                            timestamp: innerMessage.timestamp,
                                            editedTimestamp: null,
                                        }
                                        return displayMessage
                                    }
                                }
                            })
                        )
                        break
                    }
                    case ChatMessageDataType.serverMessage: {
                        const newMessage: ChatDisplayMessage = {
                            id: message.id,
                            isStaffMessage: true,
                            avatar: message.authorAvatar,
                            name: message.authorName,
                            content: message.content,
                            timestamp: message.timestamp,
                            editedTimestamp: null,
                        }
                        setMessages(oldMessages => [...oldMessages, newMessage])
                        break
                    }
                    case ChatMessageDataType.serverEditedMessage: {
                        setMessages(
                            messages.map(otherMessage => {
                                if (otherMessage.id !== message.id) {
                                    return otherMessage
                                } else {
                                    return {
                                        ...otherMessage,
                                        content: message.content,
                                        editedTimestamp: message.timestamp,
                                    }
                                }
                            })
                        )
                        break
                    }
                    case ChatMessageDataType.serverReplayedMessage: {
                        // This message is only sent as part of the `serverThread` message and
                        // can safely be ignored.
                        break
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

    const toggleThreadListVisibility = react.useCallback(() => {
        setIsThreadListVisible(visible => !visible)
    }, [])

    const sendMessage = react.useCallback(
        (message: ChatClientMessageData) => {
            websocket.send(JSON.stringify(message))
        },
        [websocket]
    )

    const switchThread = react.useCallback(
        (newThreadId: ThreadId) => {
            const threadData = threads.find(thread => thread.id === newThreadId)
            if (threadData == null) {
                const message = `Unknown thread id '${newThreadId}'.`
                toast.error(message)
                logger.error(message)
            } else {
                sendMessage({
                    type: ChatMessageDataType.switchThread,
                    threadId: newThreadId,
                })
            }
        },
        [threads, /* should never change */ logger]
    )

    const sendCurrentMessage = react.useCallback(
        (event: react.SyntheticEvent, createNewThread?: boolean) => {
            event.preventDefault()
            const content = messageInput.current.value
            if (content !== '') {
                messageInput.current.value = ''
                const newMessage: ChatDisplayMessage = {
                    // This MUST be unique.
                    id: newtype.asNewtype<MessageId>(String(Number(new Date()))),
                    isStaffMessage: false,
                    avatar: null,
                    name: 'Me',
                    content,
                    timestamp: Number(new Date()),
                    editedTimestamp: null,
                }
                if (threadId == null || createNewThread) {
                    sendMessage({
                        type: ChatMessageDataType.newThread,
                        title: threadTitle,
                        content,
                    })
                    setMessages([newMessage])
                } else {
                    sendMessage({
                        type: ChatMessageDataType.message,
                        threadId,
                        content,
                    })
                    setMessages(oldMessages => [...oldMessages, newMessage])
                }
            }
        },
        [sendMessage, threadId]
    )

    const maybeMakeThreadTitleEditable = react.useCallback((event: react.MouseEvent) => {
        if (event.ctrlKey && !event.altKey && !event.metaKey && !event.shiftKey) {
            event.stopPropagation()
            setIsThreadTitleEditable(true)
            setIsThreadTitleEditingCanceled(false)
            setIsThreadListVisible(false)
            setTimeout(() => {
                titleInputRef.current.focus()
            }, 0)
        }
    }, [])

    const upgradeToPro = () => {
        // TODO:
    }

    if (container == null) {
        logger.error('Chat container not found.')
        return null
    } else {
        // This should be `findLast`, but that requires ES2023.
        const lastStaffMessage = [...messages].reverse().find(message => message.isStaffMessage)

        return reactDom.createPortal(
            <div
                style={{ right }}
                className="text-xs text-primary flex flex-col fixed top-0 right-0 h-screen bg-ide-bg border-ide-bg-dark border-l-2 w-88 py-1"
            >
                <div className="flex text-sm font-semibold mx-1">
                    <button className="flex grow items-center" onClick={toggleThreadListVisibility}>
                        <img src={TriangleDownIcon} />{' '}
                        {/* TODO: reset to current value when editing canceled */}
                        <div onClick={maybeMakeThreadTitleEditable}>
                            <input
                                type="text"
                                ref={titleInputRef}
                                disabled={!isThreadTitleEditable}
                                defaultValue={threadTitle}
                                className={`cursor-pointer bg-transparent ${
                                    isThreadTitleEditable ? '' : 'pointer-events-none'
                                }`}
                                onKeyDown={event => {
                                    switch (event.key) {
                                        case 'Escape': {
                                            setIsThreadTitleEditable(false)
                                            setIsThreadTitleEditingCanceled(true)
                                            break
                                        }
                                        case 'Enter': {
                                            event.currentTarget.blur()
                                            break
                                        }
                                    }
                                }}
                                onBlur={event => {
                                    setIsThreadTitleEditable(false)
                                    if (isThreadTitleEditingCanceled) {
                                        event.currentTarget.value = threadTitle
                                    } else {
                                        const newTitle = event.currentTarget.value
                                        setThreadTitle(newTitle)
                                        if (threadId != null) {
                                            setThreads(oldThreads =>
                                                oldThreads.map(thread =>
                                                    thread.id !== threadId
                                                        ? thread
                                                        : { ...thread, title: newTitle }
                                                )
                                            )
                                            sendMessage({
                                                type: ChatMessageDataType.renameThread,
                                                title: newTitle,
                                                threadId: threadId,
                                            })
                                        }
                                    }
                                }}
                            />
                        </div>
                    </button>
                    <button onClick={doClose}>
                        <img src={CloseLargeIcon} />
                    </button>
                </div>
                <div className="relative text-sm font-semibold">
                    <div
                        className={`grid absolute w-full bg-ide-bg shadow-soft clip-path-bottom-shadow overflow-hidden transition-grid-template-rows z-10 ${
                            isThreadListVisible ? 'grid-rows-1fr' : 'grid-rows-0fr'
                        }`}
                    >
                        <div className="min-h-0">
                            {threads.map(thread => (
                                <div
                                    key={thread.id}
                                    className={`flex p-1 ${
                                        thread.id === threadId
                                            ? 'cursor-default bg-gray-300'
                                            : 'cursor-pointer'
                                    }`}
                                    onClick={() => {
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
                <div className="flex flex-col grow mx-1">
                    {threadId != null &&
                        messages.map(message => (
                            <ChatMessage
                                key={message.id}
                                threadId={threadId}
                                message={message}
                                reactions={[]}
                                sendMessage={sendMessage}
                                // FIXME: Consider adding reaction bars to other messages, on hover.
                                shouldShowReactionBar={message === lastStaffMessage}
                            />
                        ))}
                </div>
                <div className="rounded-xl bg-white p-2 mx-2 my-1">
                    <form onSubmit={sendCurrentMessage}>
                        <div>
                            <input
                                ref={messageInput}
                                autoFocus
                                required
                                type="text"
                                placeholder="Type your message..."
                                className="w-full rounded-full p-1"
                                onInput={event => {
                                    const newIsReplyEnabled = event.currentTarget.value !== ''
                                    if (newIsReplyEnabled !== isReplyEnabled) {
                                        setIsReplyEnabled(newIsReplyEnabled)
                                    }
                                }}
                            ></input>
                            <div className="flex">
                                <button
                                    type="button"
                                    disabled={!isReplyEnabled}
                                    className={`text-white rounded-full grow text-left p-1 ${
                                        isReplyEnabled ? 'bg-gray-400' : 'bg-gray-300'
                                    }`}
                                    onClick={event => {
                                        sendCurrentMessage(event, true)
                                    }}
                                >
                                    New question? Click to start a new thread!
                                </button>
                                {/* Spacing. */}
                                <div className="w-0.5" />
                                <button
                                    type="submit"
                                    disabled={!isReplyEnabled}
                                    className={`text-white bg-blue-600 rounded-full p-1 ${
                                        isReplyEnabled ? '' : 'opacity-50'
                                    }`}
                                >
                                    Reply!
                                </button>
                            </div>
                        </div>
                    </form>
                </div>
                {!isPaidUser && (
                    <button
                        className="rounded-xl bg-call-to-action text-white p-2 mx-2 my-1"
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
