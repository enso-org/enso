/** @file A loading screen, displayed while the user is logging in. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

// =================
// === Constants ===
// =================

/** The diameter of the spinner. */
const SPINNER_SIZE_PX = 64

// =====================
// === LoadingScreen ===
// =====================

/** Props for a {@link LoadingScreen}. */
export interface LoadingScreenProps {
  readonly text?: string
}

/** A loading screen. */
export default function LoadingScreen(props: LoadingScreenProps) {
  const { text } = props
  const { getText } = textProvider.useText()

  return (
    <div className="grid h-screen w-screen place-items-center text-primary">
      <div className="flex flex-col items-center gap-status-page text-center text-base">
        <StatelessSpinner
          state={statelessSpinner.SpinnerState.loadingMedium}
          size={SPINNER_SIZE_PX}
        />
        {text ?? getText('loadingAppMessage')}
      </div>
    </div>
  )
}
