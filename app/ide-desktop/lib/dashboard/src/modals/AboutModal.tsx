/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import LogoIcon from 'enso-assets/enso_logo.svg'

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
      const texts = Array.from(
        textContainer.children,
        element => `${element.children[0]?.textContent} ${element.children[1]?.textContent}`
      )
      void navigator.clipboard.writeText(texts.join('\n'))
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
              {window.versionInfo != null ? (
                <>
                  <tr>
                    <td className="whitespace-nowrap pr-cell-x">{getText('version')}</td>
                    <td>{window.versionInfo.version}</td>
                  </tr>
                  <tr>
                    <td className="whitespace-nowrap pr-cell-x">{getText('build')}</td>
                    <td>{window.versionInfo.build}</td>
                  </tr>
                  <tr>
                    <td className="whitespace-nowrap pr-cell-x">{getText('electronVersion')}</td>
                    <td>{window.versionInfo.electron}</td>
                  </tr>
                  <tr>
                    <td className="whitespace-nowrap pr-cell-x">{getText('chromeVersion')}</td>
                    <td>{window.versionInfo.chrome}</td>
                  </tr>
                </>
              ) : (
                <>
                  {process.env.ENSO_CLOUD_DASHBOARD_VERSION != null && (
                    <tr>
                      <td className="whitespace-nowrap pr-cell-x">{getText('version')}</td>
                      <td>{process.env.ENSO_CLOUD_DASHBOARD_VERSION}</td>
                    </tr>
                  )}
                  {process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH != null && (
                    <tr>
                      <td className="whitespace-nowrap pr-cell-x">{getText('build')}</td>
                      <td>{process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH}</td>
                    </tr>
                  )}
                </>
              )}
              <tr>
                <td className="whitespace-nowrap pr-cell-x">{getText('userAgent')}</td>
                <td>{navigator.userAgent}</td>
              </tr>
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
  )
}
