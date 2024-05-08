/** @file Home screen. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import Samples from '#/layouts/Samples'
import WhatsNew from '#/layouts/WhatsNew'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import type * as spinner from '#/components/Spinner'

// ============
// === Home ===
// ============

/** Props for a {@link Home}. */
export interface HomeProps {
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
    <ariaComponents.Dialog
      type="fullscreen"
      closeButton="floating"
    >
      <div className="relative mb-4 flex flex-1 flex-col gap-home text-xs">
        <aria.Heading
          level={2}
          className="mx-10 mt-16 self-center py-banner-item text-center text-3xl font-light leading-snug"
        >
          <aria.Text className="inline-block max-w-[45rem]">{getText('welcomeSubtitle')}</aria.Text>
        </aria.Heading>
        <WhatsNew />
        <Samples createProject={createProject} />
      </div>
    </ariaComponents.Dialog>
  )
}
