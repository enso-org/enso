/** @file A loading screen, displayed while the user is logging in. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'

import * as errorModule from '#/utilities/error'

// ===================
// === ErrorScreen ===
// ===================

/** Props for an {@link ErrorScreen}. */
export interface ErrorScreenProps {
  readonly error: unknown
}

/** A loading screen. */
export default function ErrorScreen(props: ErrorScreenProps) {
  const { error } = props
  const { getText } = textProvider.useText()
  return (
    <div className="grid h-screen w-screen place-items-center text-primary">
      <div className="flex flex-col items-center gap-status-page text-center text-base">
        <aria.Text>{getText('appErroredMessage')}</aria.Text>
        <aria.Text>{getText('appErroredPrompt')}</aria.Text>
        <aria.Text className="text-delete">{errorModule.getMessageOrToString(error)}</aria.Text>
      </div>
    </div>
  )
}
