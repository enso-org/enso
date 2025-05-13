/** @file Modal for confirming delete of any type of asset. */
import { ButtonGroup, CopyButton, Dialog, Text } from '#/components/AriaComponents'
import { Icon } from '#/components/Icon'
import { useLocalBackend } from '#/providers/BackendProvider/hooks'
import { useText } from '#/providers/TextProvider'
import type { TextId } from 'enso-common/src/text'
import { useMemo } from 'react'

/** A modal for confirming the deletion of an asset. */
export default function AboutModal() {
  const localBackend = useLocalBackend()
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
  ] satisfies readonly (readonly [TextId, string])[]

  const copyText = useMemo(
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

          <ButtonGroup className="mt-4">
            <CopyButton copyText={copyText} size="medium" variant="submit">
              {getText('copy')}
            </CopyButton>
          </ButtonGroup>
        </div>
      </div>
    </Dialog>
  )
}
