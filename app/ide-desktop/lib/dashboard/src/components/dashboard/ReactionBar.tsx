/** @file A bar containing clickable reactions. */
import * as React from 'react'

import Twemoji from '#/components/Twemoji'
import UnstyledButton from '#/components/UnstyledButton'

// =================
// === Constants ===
// =================

/** The size (both width and height) of each reaction button. */
const REACTION_BUTTON_SIZE = 20
/** The list of reaction emojis, in order. */
const REACTION_EMOJIS = ['❤️', '👍', '👎', '😀', '🙁', '👀', '🎉'] as const

// =============
// === Types ===
// =============

/** All possible reactions. */
export type ReactionSymbol = (typeof REACTION_EMOJIS)[number]

// ===================
// === ReactionBar ===
// ===================

/** Props for a {@link ReactionBar}. */
export interface ReactionBarProps {
  readonly selectedReactions: Set<ReactionSymbol>
  readonly doReact: (reaction: ReactionSymbol) => void
  readonly doRemoveReaction: (reaction: ReactionSymbol) => void
  readonly className?: string
}

/** A list of emoji reactions to choose from. */
export default function ReactionBar(props: ReactionBarProps) {
  const { selectedReactions, doReact, doRemoveReaction, className } = props

  return (
    <div className={`m-chat-reaction-bar inline-block rounded-full bg-frame ${className ?? ''}`}>
      {REACTION_EMOJIS.map(emoji => (
        <UnstyledButton
          key={emoji}
          onPress={() => {
            if (selectedReactions.has(emoji)) {
              doRemoveReaction(emoji)
            } else {
              doReact(emoji)
            }
          }}
          className={`m-chat-reaction rounded-full p-chat-reaction selectable hover:bg-hover-bg hover:grayscale-0 ${
            selectedReactions.has(emoji) ? 'active' : 'grayscale'
          }`}
        >
          <Twemoji key={emoji} emoji={emoji} size={REACTION_BUTTON_SIZE} />
        </UnstyledButton>
      ))}
    </div>
  )
}
