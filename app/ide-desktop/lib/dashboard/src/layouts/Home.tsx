/** @file Home screen. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

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
  const { createProject } = props
  const { getText } = textProvider.useText()
  return (
    <div className="flex flex-1 flex-col gap-home overflow-auto scroll-hidden">
      {/* For spacing */}
      <div />
      {/* Header */}
      <div className="flex flex-col gap-banner px-banner-x py-banner-y">
        <h1 className="self-center py-banner-item text-center text-4xl leading-snug">
          {getText('welcomeMessage')}
        </h1>
        <h2 className="self-center py-banner-item text-center text-xl font-normal leading-snug">
          {getText('welcomeSubtitle')}
        </h2>
      </div>
      <WhatsNew />
      <Samples createProject={createProject} />
    </div>
  )
}
