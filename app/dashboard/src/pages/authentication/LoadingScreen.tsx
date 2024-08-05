/** @file A loading screen, displayed while the user is logging in. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

// =================
// === Constants ===
// =================

/** The diameter of the spinner. */
const SPINNER_SIZE_PX = 64

// =====================
// === LoadingScreen ===
// =====================

/** A loading screen. */
export default function LoadingScreen() {
  const { getText } = textProvider.useText()

  return (
    <div className="grid h-screen w-screen place-items-center text-primary">
      <div className="flex flex-col items-center gap-8 text-center">
        <StatelessSpinner
          state={statelessSpinner.SpinnerState.loadingFast}
          size={SPINNER_SIZE_PX}
        />

        <ariaComponents.Text.Heading variant="h1" color="inherit">
          {getText('loadingAppMessage')}
        </ariaComponents.Text.Heading>
      </div>
    </div>
  )
}
