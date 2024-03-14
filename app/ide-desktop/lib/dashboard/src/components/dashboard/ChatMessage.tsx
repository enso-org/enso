/** @file A chat message, including user info, sent date, and reactions (if any). */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'
import type * as chat from 'enso-chat/chat'

import ReactionBar from '#/components/dashboard/ReactionBar'
import Reactions from '#/components/dashboard/Reactions'

import * as dateTime from '#/utilities/dateTime'

// ==========================
// === ChatDisplayMessage ===
// ==========================

/** Information needed to display a chat message. */
export interface ChatDisplayMessage {
  readonly id: chat.MessageId
  /** If `true`, this is a message from the staff to the user.
   * If `false`, this is a message from the user to the staff. */
  readonly isStaffMessage: boolean
  readonly avatar: string | null
  /** Name of the author of the message. */
  readonly name: string
  readonly content: string
  readonly reactions: chat.ReactionSymbol[]
  /** Given in milliseconds since the unix epoch. */
  readonly timestamp: number
  /** Given in milliseconds since the unix epoch. */
  readonly editedTimestamp: number | null
}

// ===================
// === ChatMessage ===
// ===================

/** Props for a {@link ChatMessage}. */
export interface ChatMessageProps {
  readonly message: ChatDisplayMessage
  readonly reactions: chat.ReactionSymbol[]
  readonly shouldShowReactionBar: boolean
  readonly doReact: (reaction: chat.ReactionSymbol) => void
  readonly doRemoveReaction: (reaction: chat.ReactionSymbol) => void
}

/** A chat message, including user info, sent date, and reactions (if any). */
export default function ChatMessage(props: ChatMessageProps) {
  const { message, reactions, shouldShowReactionBar, doReact, doRemoveReaction } = props
  const [isHovered, setIsHovered] = React.useState(false)
  return (
    <div
      className="mx-chat-message-x my-chat-message-y"
      onMouseEnter={() => {
        setIsHovered(true)
      }}
      onMouseLeave={() => {
        setIsHovered(false)
      }}
    >
      <div className="flex">
        <img
          crossOrigin="anonymous"
          src={message.avatar ?? DefaultUserIcon}
          className="my-chat-profile-picture-y size-chat-profile-picture rounded-full"
        />
        <div className="mx-chat-message-info-x leading-cozy">
          <div className="font-bold">{message.name}</div>
          <div className="text-primary text-opacity-unimportant">
            {dateTime.formatDateTimeChatFriendly(new Date(message.timestamp))}
          </div>
        </div>
      </div>
      <div className="whitespace-pre-wrap">
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
      {message.isStaffMessage && !shouldShowReactionBar && isHovered && (
        <div className="relative -my-chat-reaction-bar-py h py-chat-reaction-bar-y">
          <ReactionBar
            doReact={doReact}
            doRemoveReaction={doRemoveReaction}
            selectedReactions={new Set(message.reactions)}
            className="absolute shadow-soft"
          />
        </div>
      )}
    </div>
  )
}
