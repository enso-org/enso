/** @file A modal containing project templates and news. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import Samples from '#/layouts/Samples'
import WhatsNew from '#/layouts/WhatsNew'

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
  const { createProject } = props
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.Dialog type="fullscreen" title={getText('selectTemplate')}>
      {(opts) => (
        <div className="mb-4 flex flex-1 flex-col gap-home text-xs text-primary">
          <ariaComponents.Text
            elementType="p"
            variant="custom"
            balance
            className="mx-10 my-12 max-w-[45rem] self-center text-center text-3xl font-light leading-snug"
          >
            {getText('welcomeSubtitle')}
          </ariaComponents.Text>

          <WhatsNew />

          <Samples
            createProject={(templateId, templateName) => {
              createProject(templateId, templateName)
              opts.close()
            }}
          />
        </div>
      )}
    </ariaComponents.Dialog>
  )
}
