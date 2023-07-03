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
/** The list of reaction emojis, in order. */
const REACTION_EMOJIS: ReactionSymbol[] = ['❤️', '👍', '👎', '😀', '🙁', '👀', '🎉']
/** The initial title of the thread. */
const DEFAULT_THREAD_TITLE = 'New chat thread'
/** The maximum number of lines to show in the message input, past which a scrollbar is shown. */
const MAX_MESSAGE_INPUT_LINES = 10
/** The maximum number of messages to fetch when opening a new thread.
 * This SHOULD be the same limit as the chat backend (the maximum number of messages sent in
 * `serverThread` events). */
const MAX_MESSAGE_HISTORY = 25

// =============
// === Types ===
// =============

/** All possible reaction emojis. */
type ReactionSymbol = '❤️' | '🎉' | '👀' | '👍' | '👎' | '😀' | '🙁'

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
    /** Sent when the user is requesting scrollback history. */
    historyBefore = 'history-before',
    /** Create a new thread with an initial message. */
    newThread = 'new-thread',
    /** Rename an existing thread. */
    renameThread = 'rename-thread',
    /** Change the currently active thread. */
    switchThread = 'switch-thread',
    /** A message from the client to the server. */
    message = 'message',
    /** A reaction from the client. */
    reaction = 'reaction',
    /** Removal of a reaction from the client. */
    removeReaction = 'remove-reaction',
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

/** All possible message types that may trigger a {@link ChatServerThreadMessageData} response. */
export type ChatServerThreadRequestType =
    | ChatMessageDataType.authenticate
    | ChatMessageDataType.historyBefore
    | ChatMessageDataType.newThread
    | ChatMessageDataType.switchThread

/** Thread details and recent messages.
 * This message is sent every time the user switches threads. */
export interface ChatServerThreadMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverThread> {
    /** The type of the message that triggered this response. */
    requestType: ChatServerThreadRequestType
    title: MessageId
    id: ThreadId
    /** `true` if there is no more message history before these messages. */
    isAtBeginning: boolean
    messages: (ChatServerMessageMessageData | ChatServerReplayedMessageMessageData)[]
}

/** A regular chat message from the server to the client. */
export interface ChatServerMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverMessage> {
    id: MessageId
    authorAvatar: string | null
    authorName: string
    content: string
    reactions: ReactionSymbol[]
    /** Milliseconds since the Unix epoch. */
    timestamp: number
    /** Milliseconds since the Unix epoch. */
    editedTimestamp: number | null
}

/** A regular edited chat message from the server to the client. */
export interface ChatServerEditedMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverEditedMessage> {
    id: MessageId
    content: string
    /** Milliseconds since the Unix epoch. */
    timestamp: number
}

/** A replayed message from the client to the server. Includes the timestamp of the message. */
export interface ChatServerReplayedMessageMessageData
    extends ChatBaseMessageData<ChatMessageDataType.serverReplayedMessage> {
    id: MessageId
    content: string
    /** Milliseconds since the Unix epoch. */
    timestamp: number
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

/** Sent when the user is requesting scrollback history. */
export interface ChatHistoryBeforeMessageData
    extends ChatBaseMessageData<ChatMessageDataType.historyBefore> {
    messageId: MessageId
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
    messageId: MessageId
    reaction: ReactionSymbol
}

/** Removal of a reaction from the client. */
export interface ChatRemoveReactionMessageData
    extends ChatBaseMessageData<ChatMessageDataType.removeReaction> {
    messageId: MessageId
    reaction: ReactionSymbol
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
    | ChatHistoryBeforeMessageData
    | ChatMarkAsReadMessageData
    | ChatMessageMessageData
    | ChatNewThreadMessageData
    | ChatReactionMessageData
    | ChatRemoveReactionMessageData
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
    reactions: ReactionSymbol[]
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
    selectedReactions: Set<ReactionSymbol>
    doReact: (reaction: ReactionSymbol) => void
    doRemoveReaction: (reaction: ReactionSymbol) => void
}

/** A list of emoji reactions to choose from. */
function ReactionBar(props: ReactionBarProps) {
    const { selectedReactions, doReact, doRemoveReaction } = props

    return (
        <div className="inline-block bg-white rounded-full m-1">
            {REACTION_EMOJIS.map(emoji => (
                <button
                    key={emoji}
                    onClick={() => {
                        if (selectedReactions.has(emoji)) {
                            doRemoveReaction(emoji)
                        } else {
                            doReact(emoji)
                        }
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
    reactions: ReactionSymbol[]
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
    message: ChatDisplayMessage
    reactions: ReactionSymbol[]
    shouldShowReactionBar: boolean
    doReact: (reaction: ReactionSymbol) => void
    doRemoveReaction: (reaction: ReactionSymbol) => void
}

/** A chat message, including user info, sent date, and reactions (if any). */
function ChatMessage(props: ChatMessageProps) {
    const { message, reactions, shouldShowReactionBar, doReact, doRemoveReaction } = props
    return (
        <div className="m-2">
            <div className="flex">
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
            <div className="mx-1 whitespace-pre-wrap">
                {message.content}
                <Reactions reactions={reactions} />
            </div>
            {shouldShowReactionBar && (
                <ReactionBar
                    doReact={doReact}
                    doRemoveReaction={doRemoveReaction}
                    selectedReactions={new Set(message.reactions)}
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
    // `true` if and only if scrollback was triggered for the current thread.
    const [shouldIgnoreMessageLimit, setShouldIgnoreMessageLimit] = react.useState(false)
    const [isAtBeginning, setIsAtBeginning] = react.useState(false)
    const [threads, setThreads] = react.useState<ThreadData[]>([])
    const [messages, setMessages] = react.useState<ChatDisplayMessage[]>([])
    const [threadId, setThreadId] = react.useState<ThreadId | null>(null)
    const [isThreadListVisible, setIsThreadListVisible] = react.useState(false)
    const [threadTitle, setThreadTitle] = react.useState(DEFAULT_THREAD_TITLE)
    const [isThreadTitleEditable, setIsThreadTitleEditable] = react.useState(false)
    const [isAtTop, setIsAtTop] = react.useState(false)
    const [isAtBottom, setIsAtBottom] = react.useState(true)
    const [messagesHeightBeforeMessageHistory, setMessagesHeightBeforeMessageHistory] =
        react.useState<number | null>(null)
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
    const messageInputRef = react.useRef<HTMLTextAreaElement>(null!)
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const messagesRef = react.useRef<HTMLDivElement>(null!)

    react.useEffect(() => {
        setIsPaidUser(false)
        return () => {
            websocket.close()
        }
    }, [])

    react.useEffect(() => {
        titleInputRef.current.value = threadTitle
    }, [threadTitle])

    react.useLayoutEffect(() => {
        const element = messagesRef.current
        if (isAtTop && messagesHeightBeforeMessageHistory != null) {
            element.scrollTop = element.scrollHeight - messagesHeightBeforeMessageHistory
            setMessagesHeightBeforeMessageHistory(null)
        } else if (isAtBottom) {
            element.scrollTop = element.scrollHeight - element.clientHeight
        }
    }, [messages])

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
                        setIsAtBeginning(message.isAtBeginning)
                        const newMessages = message.messages.flatMap(innerMessage => {
                            switch (innerMessage.type) {
                                case ChatMessageDataType.serverMessage: {
                                    const displayMessage: ChatDisplayMessage = {
                                        id: innerMessage.id,
                                        isStaffMessage: true,
                                        content: innerMessage.content,
                                        reactions: innerMessage.reactions,
                                        avatar: innerMessage.authorAvatar,
                                        name: innerMessage.authorName,
                                        timestamp: innerMessage.timestamp,
                                        editedTimestamp: innerMessage.editedTimestamp,
                                    }
                                    return displayMessage
                                }
                                case ChatMessageDataType.serverReplayedMessage: {
                                    const displayMessage: ChatDisplayMessage = {
                                        id: innerMessage.id,
                                        isStaffMessage: false,
                                        content: innerMessage.content,
                                        reactions: [],
                                        avatar: null,
                                        name: 'Me',
                                        timestamp: innerMessage.timestamp,
                                        editedTimestamp: null,
                                    }
                                    return displayMessage
                                }
                            }
                        })
                        switch (message.requestType) {
                            case ChatMessageDataType.historyBefore: {
                                setMessages(oldMessages => [...newMessages, ...oldMessages])
                                break
                            }
                            default: {
                                setMessages(newMessages)
                                break
                            }
                        }
                        break
                    }
                    case ChatMessageDataType.serverMessage: {
                        const newMessage: ChatDisplayMessage = {
                            id: message.id,
                            isStaffMessage: true,
                            avatar: message.authorAvatar,
                            name: message.authorName,
                            content: message.content,
                            reactions: [],
                            timestamp: message.timestamp,
                            editedTimestamp: null,
                        }
                        setMessages(oldMessages => {
                            const newMessages = [...oldMessages, newMessage]
                            return shouldIgnoreMessageLimit
                                ? newMessages
                                : newMessages.slice(-MAX_MESSAGE_HISTORY)
                        })
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
    }, [websocket, shouldIgnoreMessageLimit])

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
            const element = messageInputRef.current
            const content = element.value
            if (content !== '') {
                element.value = ''
                element.style.height = '0px'
                element.style.height = `${element.scrollHeight}px`
                const newMessage: ChatDisplayMessage = {
                    // This MUST be unique.
                    id: newtype.asNewtype<MessageId>(String(Number(new Date()))),
                    isStaffMessage: false,
                    avatar: null,
                    name: 'Me',
                    content,
                    reactions: [],
                    timestamp: Number(new Date()),
                    editedTimestamp: null,
                }
                if (threadId == null || createNewThread) {
                    const newThreadTitle = threadId == null ? threadTitle : DEFAULT_THREAD_TITLE
                    sendMessage({
                        type: ChatMessageDataType.newThread,
                        title: newThreadTitle,
                        content,
                    })
                    setThreadId(null)
                    setThreadTitle(newThreadTitle)
                    setMessages([newMessage])
                } else {
                    sendMessage({
                        type: ChatMessageDataType.message,
                        threadId,
                        content,
                    })
                    setMessages(oldMessages => {
                        const newMessages = [...oldMessages, newMessage]
                        return shouldIgnoreMessageLimit
                            ? newMessages
                            : newMessages.slice(-MAX_MESSAGE_HISTORY)
                    })
                }
            }
        },
        [sendMessage, threadId, shouldIgnoreMessageLimit]
    )

    const maybeMakeThreadTitleEditable = react.useCallback((event: react.MouseEvent) => {
        if (event.ctrlKey && !event.altKey && !event.metaKey && !event.shiftKey) {
            event.stopPropagation()
            setIsThreadTitleEditable(true)
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
                        <div className="grow" onClick={maybeMakeThreadTitleEditable}>
                            <input
                                type="text"
                                ref={titleInputRef}
                                disabled={!isThreadTitleEditable}
                                defaultValue={threadTitle}
                                className={`bg-transparent w-full ${
                                    isThreadTitleEditable
                                        ? ''
                                        : 'cursor-pointer pointer-events-none'
                                }`}
                                onKeyDown={event => {
                                    switch (event.key) {
                                        case 'Escape': {
                                            setIsThreadTitleEditable(false)
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
                                    setIsThreadTitleEditable(false)
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
                        <div className="min-h-0 max-h-70 overflow-y-auto">
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
                <div
                    ref={messagesRef}
                    className="flex-1 overflow-scroll"
                    onScroll={event => {
                        const element = event.currentTarget
                        const isNowAtTop = element.scrollTop === 0
                        const isNowAtBottom =
                            element.scrollTop + element.clientHeight === element.scrollHeight
                        const firstMessage = messages[0]
                        if (isNowAtTop && !isAtBeginning && firstMessage != null) {
                            setShouldIgnoreMessageLimit(true)
                            sendMessage({
                                type: ChatMessageDataType.historyBefore,
                                messageId: firstMessage.id,
                            })
                            setMessagesHeightBeforeMessageHistory(element.scrollHeight)
                        }
                        if (isNowAtTop !== isAtTop) {
                            setIsAtTop(isNowAtTop)
                        }
                        if (isNowAtBottom !== isAtBottom) {
                            setIsAtBottom(isNowAtBottom)
                        }
                    }}
                >
                    {messages.map(message => (
                        <ChatMessage
                            key={message.id}
                            message={message}
                            reactions={[]}
                            doReact={reaction => {
                                sendMessage({
                                    type: ChatMessageDataType.reaction,
                                    messageId: message.id,
                                    reaction,
                                })
                                setMessages(oldMessages =>
                                    oldMessages.map(oldMessage =>
                                        oldMessage.id === message.id
                                            ? {
                                                  ...message,
                                                  reactions: [...oldMessage.reactions, reaction],
                                              }
                                            : oldMessage
                                    )
                                )
                            }}
                            doRemoveReaction={reaction => {
                                sendMessage({
                                    type: ChatMessageDataType.removeReaction,
                                    messageId: message.id,
                                    reaction,
                                })
                                setMessages(oldMessages =>
                                    oldMessages.map(oldMessage =>
                                        oldMessage.id === message.id
                                            ? {
                                                  ...message,
                                                  reactions: oldMessage.reactions.filter(
                                                      oldReaction => oldReaction !== reaction
                                                  ),
                                              }
                                            : oldMessage
                                    )
                                )
                            }}
                            // FIXME: Consider adding reaction bars to other messages, on hover.
                            shouldShowReactionBar={
                                message === lastStaffMessage || message.reactions.length !== 0
                            }
                        />
                    ))}
                </div>
                <div className="rounded-xl bg-white p-2 mx-2 my-1">
                    <form onSubmit={sendCurrentMessage}>
                        <div>
                            <textarea
                                ref={messageInputRef}
                                rows={1}
                                autoFocus
                                required
                                placeholder="Type your message ..."
                                className="w-full rounded-lg resize-none p-1"
                                onKeyDown={event => {
                                    switch (event.key) {
                                        case 'Enter': {
                                            // If the shift key is not pressed, submit the form.
                                            // If the shift key is pressed, keep the default
                                            // behavior of adding a newline.
                                            if (!event.shiftKey) {
                                                event.preventDefault()
                                                event.currentTarget.form?.requestSubmit()
                                            }
                                        }
                                    }
                                }}
                                onInput={event => {
                                    const element = event.currentTarget
                                    element.style.height = '0px'
                                    element.style.height =
                                        `min(${MAX_MESSAGE_INPUT_LINES}lh,` +
                                        `${element.scrollHeight}px)`
                                    const newIsReplyEnabled = element.value !== ''
                                    if (newIsReplyEnabled !== isReplyEnabled) {
                                        setIsReplyEnabled(newIsReplyEnabled)
                                    }
                                }}
                            />
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
