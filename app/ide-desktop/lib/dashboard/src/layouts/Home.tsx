/** @file Home screen. */
import * as React from 'react'

import Samples from '#/layouts/Samples'
import WhatsNew from '#/layouts/WhatsNew'

import type * as spinner from '#/components/Spinner'

// ============
// === Home ===
// ============

/** Props for a {@link Home}. */
export interface HomeProps {
  readonly hidden: boolean
  readonly createProject: (
    templateId?: string | null,
    templateName?: string | null,
    onSpinnerStateChange?: ((state: spinner.SpinnerState | null) => void) | null
  ) => void
}

/** Home screen. */
export default function Home(props: HomeProps) {
  const { hidden, createProject } = props
  return (
    <div
      className={`flex flex-col flex-1 overflow-auto scroll-hidden gap-home ${
        hidden ? 'hidden' : ''
      }`}
    >
      {/* For spacing */}
      <div />
      {/* Header */}
      <div className="flex flex-col gap-banner px-banner-x py-banner-y">
        <h1 className="self-center text-center leading-snug text-4xl py-banner-item">
          Welcome to Enso Community
        </h1>
        <h2 className="self-center text-center font-normal leading-snug text-xl py-banner-item">
          Explore templates, plugins, and data sources to kickstart your next big idea.
        </h2>
      </div>
      <WhatsNew />
      <Samples createProject={createProject} />
    </div>
  )
}
