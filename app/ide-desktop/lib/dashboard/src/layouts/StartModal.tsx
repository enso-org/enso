/** @file A modal containing project templates and news. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import Samples from '#/layouts/Samples'
import WhatsNew from '#/layouts/WhatsNew'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

// ==================
// === StartModal ===
// ==================

/** Props for a {@link StartModal}. */
export interface StartModalProps {
  readonly createProject: (templateId?: string | null, templateName?: string | null) => void
}

/** A modal containing project templates and news. */
export default function StartModal(props: StartModalProps) {
  const { createProject: createProjectRaw } = props
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.Dialog
      aria-label={getText('startModalLabel')}
      type="fullscreen"
      closeButton="floating"
    >
      {opts => (
        <div className="relative mb-4 flex flex-1 flex-col gap-home text-xs text-primary">
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
