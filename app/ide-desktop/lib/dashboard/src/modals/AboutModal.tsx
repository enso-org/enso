/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import LogoIcon from 'enso-assets/enso_logo.svg'

import type * as text from '#/text'

import * as supportsLocalBackendProvider from '#/providers/SupportsLocalBackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ButtonRow from '#/components/styled/ButtonRow'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

// =================
// === Constants ===
// =================

/** The duration of time for which the "copy" button indicates that the text has
 * successfully been copied. */
const CLEAR_COPIED_STATE_TIMEOUT_MS = 2_500

// ==================
// === AboutModal ===
// ==================

/** A modal for confirming the deletion of an asset. */
export default function AboutModal() {
  const supportsLocalBackend = supportsLocalBackendProvider.useSupportsLocalBackend()
  const { getText } = textProvider.useText()
  const [isCopied, setIsCopied] = React.useState(false)
  const textContainerRef = React.useRef<HTMLTableSectionElement | null>(null)

  const versionsEntries = [
    ...(window.versionInfo != null
      ? ([
          ['version', window.versionInfo.version],
          ['build', window.versionInfo.build],
          ['electronVersion', window.versionInfo.electron],
          ['chromeVersion', window.versionInfo.chrome],
        ] as const)
      : [
          ...(process.env.ENSO_CLOUD_DASHBOARD_VERSION == null
            ? []
            : ([['version', process.env.ENSO_CLOUD_DASHBOARD_VERSION]] as const)),
          ...(process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH == null
            ? []
            : ([['build', process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH]] as const)),
        ]),
    ['userAgent', navigator.userAgent],
  ] satisfies readonly (readonly [text.TextId, string])[]

  const doCopy = () => {
    const lines = versionsEntries.map(entry => {
      const [textId, version] = entry
      return `${getText(textId)} ${version}`
    })
    void navigator.clipboard.writeText(lines.join('\n'))
    if (!isCopied) {
      setIsCopied(true)
      setTimeout(() => {
        setIsCopied(false)
      }, CLEAR_COPIED_STATE_TIMEOUT_MS)
    }
  }

  return (
    <ariaComponents.DialogTrigger defaultOpen>
      {/* This button is not visible - it is only to provide the button for this `DialogTrigger`. */}
      <aria.Button className="h-0 w-0" />
      <ariaComponents.Dialog className="w-[30rem]">
        <div className="relative flex items-center gap-4">
          <SvgMask src={LogoIcon} className="size-16 shrink-0" />
          <div className="flex flex-col gap-3">
            <div className="text-base font-semibold">
              {supportsLocalBackend
                ? getText('appNameDesktopEdition')
                : getText('appNameCloudEdition')}
            </div>
            <table>
              <tbody ref={textContainerRef}>
                {versionsEntries.map(entry => {
                  const [textId, version] = entry
                  return (
                    <tr key={textId}>
                      <td className="whitespace-nowrap pr-cell-x align-text-top">
                        {getText(textId)}
                      </td>
                      <td>{version}</td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
            <ButtonRow>
              <UnstyledButton
                className="button relative bg-invite text-inversed active"
                onPress={doCopy}
              >
                <aria.Text className="text">
                  {isCopied ? getText('copied') : getText('copy')}
                </aria.Text>
              </UnstyledButton>
            </ButtonRow>
          </div>
        </div>
      </ariaComponents.Dialog>
    </ariaComponents.DialogTrigger>
  )
}
