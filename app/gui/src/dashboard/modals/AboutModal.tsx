/** @file Modal for confirming delete of any type of asset. */
import { CopyButton } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import { Text } from '#/components/Text'
import { mergeRefs } from '#/utilities/mergeRefs'
import { useModalState, type ModalApi } from '#/utilities/modal'
import { useBackends, useText } from '$/providers/react'
import type { TextId } from 'enso-common/src/text'
import { forwardRef, useMemo, type ForwardedRef } from 'react'

/** A modal for confirming the deletion of an asset. */
export const AboutModal = Object.assign(
  forwardRef(function AboutModal(_props: object, ref: ForwardedRef<ModalApi>) {
    const { localBackend } = useBackends()
    const { getText } = useText()
    const { isOpen, setIsOpen } = useModalState(
      mergeRefs(ref, (api) => {
        if (api) {
          ABOUT_MODAL_OUTER.open = api.open
          ABOUT_MODAL_OUTER.close = api.close
        }
      }),
    )

    const versionsEntries = [
      ...(window.api != null ?
        ([
          ['version', window.api.versionInfo.version],
          ['build', window.api.versionInfo.build],
          ['electronVersion', window.api.versionInfo.electron],
          ['chromeVersion', window.api.versionInfo.chrome],
        ] as const)
      : ([
          ...($config.VERSION == null ? [] : ([['version', $config.VERSION]] as const)),
          ...($config.COMMIT_HASH == null ? [] : ([['build', $config.COMMIT_HASH]] as const)),
        ] as const)),
      ['userAgent', navigator.userAgent],
    ] satisfies readonly (readonly [TextId, string])[]

    const copyText = useMemo(
      () => versionsEntries.map(([textId, version]) => `${getText(textId)} ${version}`).join('\n'),
      [getText, versionsEntries],
    )

    return (
      <Dialog
        title={getText('aboutThisAppShortcut')}
        size="large"
        modalProps={{ isOpen }}
        onOpenChange={setIsOpen}
      >
        <div className="relative flex flex-col items-center gap-4">
          <Icon icon="enso_logo" className="size-16 shrink-0" />

          <div className="flex flex-col items-center gap-2">
            <Text.Heading>
              {localBackend != null ?
                getText('appNameDesktopEdition')
              : getText('appNameCloudEdition')}
            </Text.Heading>

            <table className="self-stretch">
              <tbody>
                {versionsEntries.map((entry) => {
                  const [textId, version] = entry

                  return (
                    <tr key={textId}>
                      <td className="pr-cell-x align-text-top">
                        <Text nowrap>{getText(textId)}</Text>
                      </td>
                      <td>
                        <Text className="break-words [word-break:break-word]">{version}</Text>
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>

            <CopyButton copyText={copyText} size="medium" variant="submit">
              {getText('copy')}
            </CopyButton>
          </div>
        </div>
      </Dialog>
    )
  }),
  { open: () => {}, close: () => {} },
)
const ABOUT_MODAL_OUTER = AboutModal
