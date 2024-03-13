/** @file A loading screen, displayed while the user is logging in. */
import * as React from 'react'

import * as common from 'enso-common'

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
  return (
    <div className="grid h-screen w-screen place-items-center text-primary">
      <div className="flex flex-col items-center gap-status-page text-center text-base">
        <StatelessSpinner
          state={statelessSpinner.SpinnerState.loadingMedium}
          size={SPINNER_SIZE_PX}
        />
        <span>Logging in to {common.PRODUCT_NAME}...</span>
      </div>
    </div>
  )
}
