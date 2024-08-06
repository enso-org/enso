/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import type * as text from 'enso-common/src/text'

import LogoIcon from '#/assets/enso_logo.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'

// ==================
// === AboutModal ===
// ==================

/** A modal for confirming the deletion of an asset. */
export default function AboutModal() {
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()

  const versionsEntries = [
    ...(window.versionInfo != null ?
      ([
        ['version', window.versionInfo.version],
        ['build', window.versionInfo.build],
        ['electronVersion', window.versionInfo.electron],
        ['chromeVersion', window.versionInfo.chrome],
      ] as const)
    : [
        ...(process.env.ENSO_CLOUD_DASHBOARD_VERSION == null ?
          []
        : ([['version', process.env.ENSO_CLOUD_DASHBOARD_VERSION]] as const)),
        ...(process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH == null ?
          []
        : ([['build', process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH]] as const)),
      ]),
    ['userAgent', navigator.userAgent],
  ] satisfies readonly (readonly [text.TextId, string])[]

  const copyText = React.useMemo(
    () => versionsEntries.map(([textId, version]) => `${getText(textId)} ${version}`).join('\n'),
    [getText, versionsEntries],
  )

  return (
    <ariaComponents.Dialog
      title={getText('aboutThisAppShortcut')}
      modalProps={{ defaultOpen: true }}
    >
      <div className="relative flex items-center gap-4">
        <SvgMask src={LogoIcon} className="size-16 shrink-0 self-start" />

        <div className="flex flex-col">
          <ariaComponents.Text variant="subtitle">
            {localBackend != null ?
              getText('appNameDesktopEdition')
            : getText('appNameCloudEdition')}
          </ariaComponents.Text>

          <table>
            <tbody>
              {versionsEntries.map((entry) => {
                const [textId, version] = entry

                return (
                  <tr key={textId}>
                    <td className="pr-cell-x align-text-top">
                      <ariaComponents.Text nowrap>{getText(textId)}</ariaComponents.Text>
                    </td>
                    <td>
                      <ariaComponents.Text>{version}</ariaComponents.Text>
                    </td>
                  </tr>
                )
              })}
            </tbody>
          </table>

          <ariaComponents.ButtonGroup className="mt-4">
            <ariaComponents.CopyButton copyText={copyText} size="medium" variant="submit">
              {getText('copy')}
            </ariaComponents.CopyButton>
          </ariaComponents.ButtonGroup>
        </div>
      </div>
    </ariaComponents.Dialog>
  )
}
