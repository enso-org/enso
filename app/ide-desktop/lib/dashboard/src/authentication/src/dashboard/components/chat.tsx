/** @file A WebSocket-based chat directly to official support on the official Discord server. */
import * as react from 'react'
import * as reactDom from 'react-dom'

import * as animations from '../../animations'
import * as authProvider from '../../authentication/providers/auth'
import * as loggerProvider from '../../providers/logger'
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

// =============
// === Types ===
// =============

type Reaction = (typeof REACTION_EMOJIS)[number]

interface ChatDisplayMessage {
    id: string
    avatar: string
    name: string
    content: string
    timestamp: number
    isStaffMessage: boolean
}

export enum ChatMessageDataType {
    // Server-only messages.
    serverMessage = 'serverMessage',
    // Client-only messages.
    authenticate = 'authenticate',
    newThread = 'newThread',
    renameThread = 'renameThread',
    message = 'message',
    reaction = 'reaction',
}

interface ChatMessageBaseData<Type extends ChatMessageDataType> {
    type: Type
}

export interface ChatMessageServerMessageData
    extends ChatMessageBaseData<ChatMessageDataType.serverMessage> {
    messageId: string
    timestamp: number
    content: string
}

export type ChatServerMessageData = ChatMessageServerMessageData

export interface ChatAuthenticateMessageData
    extends ChatMessageBaseData<ChatMessageDataType.authenticate> {
    accessToken: string
}

export interface ChatNewThreadMessageData
    extends ChatMessageBaseData<ChatMessageDataType.newThread> {
    title: string
    /** Content of the first message, to reduce the number of round trips. */
    content: string
}

export interface ChatRenameThreadMessageData
    extends ChatMessageBaseData<ChatMessageDataType.renameThread> {
    title: string
    threadId: string
}

export interface ChatMessageMessageData extends ChatMessageBaseData<ChatMessageDataType.message> {
    threadId: string
    content: string
}

export interface ChatReactionMessageData extends ChatMessageBaseData<ChatMessageDataType.reaction> {
    threadId: string
    messageId: string
    reaction: string
}

export type ChatClientMessageData =
    | ChatAuthenticateMessageData
    | ChatMessageMessageData
    | ChatNewThreadMessageData
    | ChatReactionMessageData
    | ChatRenameThreadMessageData

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
interface ChatProps {
    /** This should only be false when the panel is closing. */
    isOpen: boolean
    doClose: () => void
}

function Chat(props: ChatProps) {
    const { isOpen, doClose } = props
    const { accessToken } = authProvider.useFullUserSession()
    const logger = loggerProvider.useLogger()

    const [isPaidUser, setIsPaidUser] = react.useState(true)
    const [messages, setMessages] = react.useState<ChatDisplayMessage[]>([])
    const [threadId, setThreadId] = react.useState<string | null>(null)
    const [threadTitle, setThreadTitle] = react.useState('New chat thread')
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
                                <button className="grow" onClick={createNewThread}>
                                    New question? Click to start a new thread!
                                </button>
                                <button>Reply!</button>
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
