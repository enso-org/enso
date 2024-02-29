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
  const { hidden, createProject } = props
  const { getText } = textProvider.useText()
  return (
    <div
      className={`flex flex-col flex-1 overflow-auto scroll-hidden gap-12 ${
        hidden ? 'hidden' : ''
      }`}
    >
      {/* For spacing */}
      <div />
      {/* Header */}
      <div className="flex flex-col gap-2 px-9.75 py-2.25">
        <h1 className="self-center text-center leading-144.5 text-4xl py-0.5">
          {getText('welcomeMessage')}
        </h1>
        <h2 className="self-center text-center font-normal leading-144.5 text-xl py-0.5">
          {getText('welcomeSubtitle')}
        </h2>
      </div>
      <WhatsNew />
      <Samples createProject={createProject} />
    </div>
  )
}
