/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import EnsoLogoIcon from 'enso-assets/enso_logo.svg'

import * as textProvider from '#/providers/TextProvider'

import Modal from '#/components/Modal'
import SvgMask from '#/components/SvgMask'

// ==================
// === AboutModal ===
// ==================

/** Props for a {@link AboutModal}. */
export interface AboutModalProps {
  readonly supportsLocalBackend: boolean
}

/** A modal for confirming the deletion of an asset. */
export default function AboutModal(props: AboutModalProps) {
  const { supportsLocalBackend } = props
  const { getText } = textProvider.useText()

  return (
    <Modal centered className="bg-dim">
      <div
        data-testid="about-modal"
        className="pointer-events-auto relative flex w-[28rem] select-text flex-col gap-modal rounded-default p-modal-wide before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={event => {
          event.stopPropagation()
        }}
      >
        <div className="relative flex gap-4">
          <SvgMask src={EnsoLogoIcon} className="size-16" />
          <div className="flex flex-col gap-1">
            <div className="text-base font-semibold">
              {supportsLocalBackend
                ? getText('appNameDesktopEdition')
                : getText('appNameCloudEdition')}
            </div>
            <div>
              {getText('versionX', process.env.ENSO_CLOUD_DASHBOARD_VERSION)}
              {/* TODO: version injected at build time as environment variable */}
            </div>
            {process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH && (
              <div>
                {getText('commitX', process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH)}
                {/* TODO: version injected at build time as environment variable */}
              </div>
            )}
            {window.electronVersions != null && (
              <div>{getText('electronVersionX', window.electronVersions.electron)}</div>
            )}
          </div>
        </div>
      </div>
    </Modal>
  )
}
