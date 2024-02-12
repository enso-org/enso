/** @file A list of emoji reactions that have been on a message. */
import * as React from 'react'

import type * as reactionBar from '#/components/dashboard/ReactionBar'
import Twemoji from '#/components/Twemoji'

// =================
// === Constants ===
// =================

/** The size (both width and height) of each reaction on a message. */
export const REACTION_SIZE = 16

// =================
// === Reactions ===
// =================

/** Props for a {@link Reactions}. */
export interface ReactionsProps {
  readonly reactions: reactionBar.ReactionSymbol[]
}

/** A list of emoji reactions that have been on a message. */
export default function Reactions(props: ReactionsProps) {
  const { reactions } = props

  if (reactions.length === 0) {
    return null
  } else {
    return (
      <div>
        {reactions.map(reaction => (
          <Twemoji key={reaction} emoji={reaction} size={REACTION_SIZE} />
        ))}
      </div>
    )
  }
}
