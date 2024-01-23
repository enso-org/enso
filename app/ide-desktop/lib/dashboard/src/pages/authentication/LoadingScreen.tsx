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

/** A loading screen. */
export default function LoadingScreen() {
  const { getText } = textProvider.useText()
  return (
    <div className="grid place-items-center text-primary h-screen w-screen">
      <div className="flex flex-col gap-4 items-center">
        <StatelessSpinner
          state={statelessSpinner.SpinnerState.loadingMedium}
          size={SPINNER_SIZE_PX}
        />
        <span>{getText('loadingAppMessage')}</span>
      </div>
    </div>
  )
}
