/**
 * @file An API definition for the chat WebSocket server.
 * Types copied from the enso-bot server implementation:
 * https://github.com/enso-org/enso-bot/blob/aa903b6e639a31930ee4fff55c5639e4471fa48d/chat.ts
 */

import type * as newtype from '#/utilities/newtype'

// =====================
// === Message Types ===
// =====================

/** Identifier for a chat Thread. */
export type ThreadId = newtype.Newtype<string, 'ThreadId'>
/** Identifier for a chat message. */
export type MessageId = newtype.Newtype<string, 'MessageId'>
/** Identifier for a chat user. */
export type UserId = newtype.Newtype<string, 'UserId'>
/** Chat user's email addresss. */
export type EmailAddress = newtype.Newtype<string, 'EmailAddress'>

/** Enumeration of all message types exchanged with the chat server. */
export enum ChatMessageDataType {
  // Messages internal to the server.
  /** Like the `authenticate` message, but with user details. */
  internalAuthenticate = 'internal-authenticate',
  /** Like the `authenticateAnonymously` message, but with user details. */
  internalAuthenticateAnonymously = 'internal-authenticate-anonymously',
  // Messages from the server to the client.
  /** Metadata for all threads associated with a user. */
  serverThreads = 'server-threads',
  /** Metadata for the currently open thread. */
  serverThread = 'server-thread',
  /** A message from the server to the client. */
  serverMessage = 'server-message',
  /** An edited message from the server to the client. */
  serverEditedMessage = 'server-edited-message',
  /**
   * A message from the client to the server, sent from the server to the client as part of
   * the message history.
   */
  serverReplayedMessage = 'server-replayed-message',
  // Messages from the client to the server.
  /** The authentication token. */
  authenticate = 'authenticate',
  /** Sent by a user that is not logged in. This is currently only used on the website. */
  authenticateAnonymously = 'authenticate-anonymously',
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
  /**
   * Mark a message as read. Used to determine whether to show the notification dot
   * next to a thread.
   */
  markAsRead = 'mark-as-read',
}

/** Properties common to all WebSocket messages. */
interface ChatBaseMessageData<Type extends ChatMessageDataType> {
  readonly type: Type
}

// =========================
// === Internal messages ===
// =========================

/** Sent to the main file with user information. */
export interface ChatInternalAuthenticateMessageData
  extends ChatBaseMessageData<ChatMessageDataType.internalAuthenticate> {
  readonly userId: UserId
  readonly userName: string
}

/** Sent to the main file with user IP. */
export interface ChatInternalAuthenticateAnonymouslyMessageData
  extends ChatBaseMessageData<ChatMessageDataType.internalAuthenticateAnonymously> {
  readonly userId: UserId
  readonly email: EmailAddress
}

// ======================================
// === Messages from server to client ===
// ======================================

/** All possible emojis that can be used as a reaction on a chat message. */
export type ReactionSymbol = '❤️' | '🎉' | '👀' | '👍' | '👎' | '😀' | '🙁'

/** Basic metadata for a single thread. */
export interface ThreadData {
  readonly title: string
  readonly id: ThreadId
  readonly hasUnreadMessages: boolean
}

/** Basic metadata for a all of a user's threads. */
export interface ChatServerThreadsMessageData
  extends ChatBaseMessageData<ChatMessageDataType.serverThreads> {
  readonly threads: ThreadData[]
}

/** All possible message types that may trigger a {@link ChatServerThreadMessageData} response. */
export type ChatServerThreadRequestType =
  | ChatMessageDataType.authenticate
  | ChatMessageDataType.historyBefore
  | ChatMessageDataType.newThread
  | ChatMessageDataType.switchThread

/**
 * Thread details and recent messages.
 * This message is sent every time the user switches threads.
 */
export interface ChatServerThreadMessageData
  extends ChatBaseMessageData<ChatMessageDataType.serverThread> {
  /** The type of the message that triggered this response. */
  readonly requestType: ChatServerThreadRequestType
  readonly title: string
  readonly id: ThreadId
  /** `true` if there is no more message history before these messages. */
  readonly isAtBeginning: boolean
  readonly messages: (ChatServerMessageMessageData | ChatServerReplayedMessageMessageData)[]
}

/** A regular chat message from the server to the client. */
export interface ChatServerMessageMessageData
  extends ChatBaseMessageData<ChatMessageDataType.serverMessage> {
  readonly id: MessageId
  // This should not be `null` for staff, as registration is required.
  // However, it will be `null` for users that have not yet set an avatar.
  readonly authorAvatar: string | null
  readonly authorName: string
  readonly content: string
  readonly reactions: ReactionSymbol[]
  /** Milliseconds since the Unix epoch. */
  readonly timestamp: number
  /**
   * Milliseconds since the Unix epoch.
   * Should only be present when receiving message history, because new messages cannot have been
   * edited.
   */
  readonly editedTimestamp: number | null
}

/** A regular edited chat message from the server to the client. */
export interface ChatServerEditedMessageMessageData
  extends ChatBaseMessageData<ChatMessageDataType.serverEditedMessage> {
  readonly id: MessageId
  readonly content: string
  /** Milliseconds since the Unix epoch. */
  readonly timestamp: number
}

/** A replayed message from the client to the server. Includes the timestamp of the message. */
export interface ChatServerReplayedMessageMessageData
  extends ChatBaseMessageData<ChatMessageDataType.serverReplayedMessage> {
  readonly id: MessageId
  readonly content: string
  /** Milliseconds since the Unix epoch. */
  readonly timestamp: number
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
  readonly accessToken: string
}

/** Sent whenever the user opens the chat sidebar. */
export interface ChatAuthenticateAnonymouslyMessageData
  extends ChatBaseMessageData<ChatMessageDataType.authenticateAnonymously> {
  readonly email: EmailAddress
}

/** Sent when the user is requesting scrollback history. */
export interface ChatHistoryBeforeMessageData
  extends ChatBaseMessageData<ChatMessageDataType.historyBefore> {
  readonly messageId: MessageId
}

/** Sent when the user sends a message in a new thread. */
export interface ChatNewThreadMessageData
  extends ChatBaseMessageData<ChatMessageDataType.newThread> {
  readonly title: string
  /** Content of the first message, to reduce the number of round trips. */
  readonly content: string
}

/** Sent when the user finishes editing the thread name in the chat title bar. */
export interface ChatRenameThreadMessageData
  extends ChatBaseMessageData<ChatMessageDataType.renameThread> {
  readonly title: string
  readonly threadId: ThreadId
}

/** Sent when the user picks a thread from the dropdown. */
export interface ChatSwitchThreadMessageData
  extends ChatBaseMessageData<ChatMessageDataType.switchThread> {
  readonly threadId: ThreadId
}

/** A regular message from the client to the server. */
export interface ChatMessageMessageData extends ChatBaseMessageData<ChatMessageDataType.message> {
  readonly threadId: ThreadId
  readonly content: string
}

/** A reaction to a message sent by staff. */
export interface ChatReactionMessageData extends ChatBaseMessageData<ChatMessageDataType.reaction> {
  readonly messageId: MessageId
  readonly reaction: ReactionSymbol
}

/** Removal of a reaction from the client. */
export interface ChatRemoveReactionMessageData
  extends ChatBaseMessageData<ChatMessageDataType.removeReaction> {
  readonly messageId: MessageId
  readonly reaction: ReactionSymbol
}

/** Sent when the user scrolls to the bottom of a chat thread. */
export interface ChatMarkAsReadMessageData
  extends ChatBaseMessageData<ChatMessageDataType.markAsRead> {
  readonly threadId: ThreadId
  readonly messageId: MessageId
}

/** A message from the client to the server. */
export type ChatClientMessageData =
  | ChatAuthenticateAnonymouslyMessageData
  | ChatAuthenticateMessageData
  | ChatHistoryBeforeMessageData
  | ChatMarkAsReadMessageData
  | ChatMessageMessageData
  | ChatNewThreadMessageData
  | ChatReactionMessageData
  | ChatRemoveReactionMessageData
  | ChatRenameThreadMessageData
  | ChatSwitchThreadMessageData
