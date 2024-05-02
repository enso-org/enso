/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import LogoIcon from 'enso-assets/enso_logo.svg'

import type * as text from '#/text'

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

/** Props for a {@link AboutModal}. */
export interface AboutModalProps {
  readonly supportsLocalBackend: boolean
}

/** A modal for confirming the deletion of an asset. */
export default function AboutModal(props: AboutModalProps) {
  const { supportsLocalBackend } = props
  const { getText } = textProvider.useText()
  const [isCopied, setIsCopied] = React.useState(false)
  const textContainerRef = React.useRef<HTMLTableSectionElement | null>(null)

  const versionsEntries = (
    window.versionInfo != null
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
        ]
  ) satisfies readonly (readonly [text.TextId, string])[]

  const doCopy = () => {
    const textContainer = textContainerRef.current
    if (textContainer == null) {
      return
    } else {
      const firstChild = textContainer.children[0]
      const lastChild = textContainer.children[textContainer.children.length - 1]
      if (firstChild != null && lastChild != null) {
        getSelection()?.setBaseAndExtent(firstChild, 0, lastChild, 2)
      }
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
      return
    }
  }

  return (
    <aria.DialogTrigger defaultOpen>
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
                      <td className="whitespace-nowrap pr-cell-x">{getText(textId)}</td>
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
    </aria.DialogTrigger>
  )
}
