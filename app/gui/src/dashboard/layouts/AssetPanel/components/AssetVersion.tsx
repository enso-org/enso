/** @file Displays information describing a specific version of an asset. */

import type Backend from '#/services/Backend'
import * as backendService from '#/services/Backend'

import { Button, CopyButton, Dialog, Menu, Popover, Text } from '#/components/AriaComponents'
import { Badge } from '#/components/Badge'
import { Icon } from '#/components/Icon'
import { TEXT_WITH_ICON } from '#/components/patterns'
import { ProfilePicture } from '#/components/ProfilePicture'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { setModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { AssetDiffView } from '../../AssetDiffView'

/**
 * A version of an asset.
 */
export interface Version extends backendService.S3ObjectVersion {
  readonly number: number
  readonly title: string
}

/**
 * Options for duplicating an asset.
 */
export interface DuplicateOptions {
  readonly start?: boolean
}

/** Props for a {@link AssetVersion}. */
export interface AssetVersionProps {
  readonly otherVersions: Version[]
  readonly item: backendService.AnyAsset
  readonly version: Version
  readonly previousVersion: Version | undefined
  readonly backend: Backend
  readonly doRestore: (version: Version) => Promise<void> | void
  readonly doDuplicate: (options?: DuplicateOptions) => Promise<void> | void
}

/** Displays information describing a specific version of an asset. */
export function AssetVersion(props: AssetVersionProps) {
  const {
    version,
    item,
    backend,
    doRestore: doRestoreRaw,
    otherVersions,
    previousVersion,
    doDuplicate,
  } = props

  const { getText, locale } = useText()

  const isProject = item.type === backendService.AssetType.project
  const comparableVersions = otherVersions
    .map((v, index) => ({ ...v, number: otherVersions.length - index }))
    .filter((v) => v.versionId !== version.versionId)

  const canRestore = !version.isLatest

  const doRestore = useEventCallback(async () => {
    await doRestoreRaw(version)
  })

  return (
    <div className="grid w-full select-none grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
      <div className="flex flex-1 flex-col">
        <div className="flex items-center gap-2">
          <Text variant="body" truncate>
            {version.title}
          </Text>

          {version.isLatest && <Badge variant="outline">{getText('latestIndicator')}</Badge>}
        </div>

        <div className="flex items-center gap-2">
          <div className={TEXT_WITH_ICON().base({ gap: 'medium', className: 'flex-none' })}>
            <Icon size="small" icon="calendar" className={TEXT_WITH_ICON().icon()} />
            <Text elementType="time" variant="body-sm" className={TEXT_WITH_ICON().text()}>
              {new Date(version.lastModified).toLocaleString(locale, {
                year: 'numeric',
                month: 'short',
                day: 'numeric',
                hour: 'numeric',
                minute: 'numeric',
              })}
            </Text>
          </div>

          {version.user && (
            <Popover.Trigger>
              <Button
                variant="ghost"
                size="xxsmall"
                icon={
                  <ProfilePicture
                    picture={version.user.profilePicture}
                    name={version.user.name}
                    size="xxsmall"
                    className="-mt-0.5"
                  />
                }
                className="min-w-0"
              >
                <Text variant="body-sm" truncate="1" nowrap>
                  {version.user.name}
                </Text>
              </Button>

              <Popover>
                <div className={TEXT_WITH_ICON().base({ verticalAlign: 'top' })}>
                  <ProfilePicture
                    picture={version.user.profilePicture}
                    name={version.user.name}
                    className={TEXT_WITH_ICON().icon()}
                  />

                  <div
                    className={TEXT_WITH_ICON().text({ className: 'flex flex-col items-start' })}
                  >
                    <Text.Group>
                      <Text variant="body" className="leading-[1.2]" truncate="3">
                        {version.user.name}
                      </Text>

                      <Button.Group verticalAlign="center">
                        <Button
                          variant="link"
                          size="small"
                          icon="email"
                          className="min-w-0"
                          tooltip={getText('sendEmail')}
                          href={`mailto:${version.user.email}`}
                        >
                          {version.user.email}
                        </Button>

                        <CopyButton
                          size="xsmall"
                          className="min-w-0"
                          copyText={version.user.email}
                        />
                      </Button.Group>
                    </Text.Group>
                  </div>
                </div>
              </Popover>
            </Popover.Trigger>
          )}
        </div>
      </div>

      <Button.GroupJoin
        className="shrink-0 grow-0"
        buttonVariants={{ size: 'small', variant: 'outline' }}
      >
        {isProject && (
          <Dialog.Trigger>
            <Button icon="compare">{getText('seeChanges')}</Button>

            <VersionDialog
              version={version}
              compareVersion={previousVersion}
              backend={backend}
              item={item}
              doRestore={canRestore ? doRestore : undefined}
              doDuplicate={doDuplicate}
            />
          </Dialog.Trigger>
        )}

        <Menu.Trigger>
          <Button icon="folder_opened" iconPosition="end" variant="outline">
            {!isProject && getText('actions')}
          </Button>

          <Menu>
            {canRestore && (
              <Menu.Item onAction={doRestore} icon="restore">
                {getText('restoreThisVersion')}
              </Menu.Item>
            )}

            <Menu.Item onAction={doDuplicate} icon="duplicate">
              {getText('duplicateThisVersion')}
            </Menu.Item>

            {isProject && (
              <Menu.Item onAction={() => doDuplicate({ start: true })} icon="copy">
                {getText('duplicateAndOpen')}
              </Menu.Item>
            )}

            {isProject && comparableVersions.length > 0 && (
              <Menu.SubmenuTrigger>
                <Menu.Item icon="compare">{getText('compareVersionSubmenuLabel')}</Menu.Item>
                <Menu items={comparableVersions}>
                  {(comparableVersion) => (
                    <Menu.Item
                      id={comparableVersion.versionId}
                      onAction={() => {
                        setModal(
                          <VersionDialog
                            version={version}
                            compareVersion={comparableVersion}
                            backend={backend}
                            item={item}
                            doRestore={!comparableVersion.isLatest ? doRestore : undefined}
                            doDuplicate={doDuplicate}
                          />,
                        )
                      }}
                    >
                      {comparableVersion.isLatest ? 'Latest' : comparableVersion.title}
                    </Menu.Item>
                  )}
                </Menu>
              </Menu.SubmenuTrigger>
            )}
          </Menu>
        </Menu.Trigger>
      </Button.GroupJoin>
    </div>
  )
}

/** Props for a {@link VersionDialog}. */
interface VersionDialogProps {
  readonly version: Version
  readonly compareVersion: Version | undefined
  readonly backend: Backend
  readonly item: backendService.ProjectAsset
  readonly doRestore?: (() => Promise<void> | void) | undefined
  readonly doDuplicate?: (() => Promise<void> | void) | undefined
}

/** Displays a dialog that allows the user to compare two versions of an asset. */
function VersionDialog(props: VersionDialogProps) {
  const { version, compareVersion, backend, item, doRestore, doDuplicate } = props
  const { getText } = useText()

  return (
    <Dialog
      type="fullscreen"
      title={
        compareVersion?.title != null ?
          getText('compareVersionXWithY', version.title, compareVersion.title)
        : getText('changes')
      }
      padding="none"
    >
      <div className="flex h-full flex-col">
        <Button.Group className="px-4 py-4" gap="large">
          {doRestore && (
            <Dialog.Close
              size="medium"
              variant="icon"
              loaderPosition="icon"
              icon="restore"
              onPress={async () => {
                await doRestore()
              }}
            >
              {getText('restoreThisVersion')}
            </Dialog.Close>
          )}

          {doDuplicate && (
            <Dialog.Close
              size="medium"
              variant="icon"
              loaderPosition="icon"
              icon="duplicate"
              onPress={async () => {
                await doDuplicate()
              }}
            >
              {getText('duplicateThisVersion')}
            </Dialog.Close>
          )}
        </Button.Group>

        <AssetDiffView
          currentVersionId={version.versionId}
          previousVersionId={compareVersion?.versionId}
          project={item}
          backend={backend}
        />
      </div>
    </Dialog>
  )
}
