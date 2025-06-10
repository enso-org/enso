/** @file Modal for confirming delete of any type of asset. */
import { Button, CopyButton } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import { Text } from '#/components/Text'
import { useBackends, useText } from '$/providers/react'
import type * as text from 'enso-common/src/text'
import * as React from 'react'

/** A modal for confirming the deletion of an asset. */
export default function AboutModal() {
  const { localBackend } = useBackends()
  const { getText } = useText()

  const versionsEntries = [
    ...(window.versionInfo != null ?
      ([
        ['version', window.versionInfo.version],
        ['build', window.versionInfo.build],
        ['electronVersion', window.versionInfo.electron],
        ['chromeVersion', window.versionInfo.chrome],
      ] as const)
    : [
        ...($config.VERSION == null ? [] : ([['version', $config.VERSION]] as const)),
        ...($config.COMMIT_HASH == null ? [] : ([['build', $config.COMMIT_HASH]] as const)),
      ]),
    ['userAgent', navigator.userAgent],
  ] satisfies readonly (readonly [text.TextId, string])[]

  const copyText = React.useMemo(
    () => versionsEntries.map(([textId, version]) => `${getText(textId)} ${version}`).join('\n'),
    [getText, versionsEntries],
  )

  return (
    <Dialog title={getText('aboutThisAppShortcut')} modalProps={{ defaultOpen: true }}>
      <div className="relative flex items-center gap-4">
        <Icon icon="enso_logo" className="size-16 shrink-0 self-start" />

        <div className="flex flex-col">
          <Text variant="subtitle">
            {localBackend != null ?
              getText('appNameDesktopEdition')
            : getText('appNameCloudEdition')}
          </Text>

          <table>
            <tbody>
              {versionsEntries.map((entry) => {
                const [textId, version] = entry

                return (
                  <tr key={textId}>
                    <td className="pr-cell-x align-text-top">
                      <Text nowrap>{getText(textId)}</Text>
                    </td>
                    <td>
                      <Text>{version}</Text>
                    </td>
                  </tr>
                )
              })}
            </tbody>
          </table>

          <Button.Group className="mt-4">
            <CopyButton copyText={copyText} size="medium" variant="submit">
              {getText('copy')}
            </CopyButton>
          </Button.Group>
        </div>
      </div>
    </Dialog>
  )
}
