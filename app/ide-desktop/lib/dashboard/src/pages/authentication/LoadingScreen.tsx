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
        <div className="grid place-items-center text-primary h-screen w-screen">
            <div className="flex flex-col gap-4 items-center">
                <StatelessSpinner
                    state={statelessSpinner.SpinnerState.loadingMedium}
                    size={SPINNER_SIZE_PX}
                />
                <span>Logging in to {common.PRODUCT_NAME}...</span>
            </div>
        </div>
    )
}
