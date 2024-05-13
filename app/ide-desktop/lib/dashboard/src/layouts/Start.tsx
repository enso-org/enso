/** @file Home screen. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import Samples from '#/layouts/Samples'
import WhatsNew from '#/layouts/WhatsNew'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

// ============
// === Home ===
// ============

/** Props for a {@link Home}. */
export interface HomeProps {
  readonly createProject: (templateId?: string | null, templateName?: string | null) => void
}

/** Home screen. */
export default function Home(props: HomeProps) {
  const { createProject: createProjectRaw } = props
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.Dialog type="fullscreen" closeButton="floating">
      {opts => (
        <div className="relative mb-4 flex flex-1 flex-col gap-home text-xs">
          <aria.Heading
            level={2}
            className="py-banner-item mx-10 mt-16 self-center text-center text-3xl font-light leading-snug"
          >
            <aria.Text className="inline-block max-w-[45rem]">
              {getText('welcomeSubtitle')}
            </aria.Text>
          </aria.Heading>
          <WhatsNew />
          <Samples
            createProject={(templateId, templateName) => {
              createProjectRaw(templateId, templateName)
              opts.close()
            }}
          />
        </div>
      )}
    </ariaComponents.Dialog>
  )
}
