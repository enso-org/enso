/** @file Displays information describing a specific version of an asset. */
import { Badge } from '#/components/Badge'
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import { Menu } from '#/components/Menu'
import { TEXT_WITH_ICON } from '#/components/patterns'
import { Text } from '#/components/Text'
import { UserWithPopover } from '#/components/UserWithPopover'
import { VisualTooltip } from '#/components/VisualTooltip'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMeasure } from '#/hooks/measureHooks'
import { setModal } from '#/providers/ModalProvider'
import { useText } from '$/providers/react'
import type { Backend } from 'enso-common/src/services/Backend'
import * as backendService from 'enso-common/src/services/Backend'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import * as React from 'react'
import { AssetDiffView } from './AssetDiffView'

const HEADER_GAP_PX = 8
const TAG_GAP_PX = 4
const MIN_TAG_WIDTH_CH = 8
const MAX_TAG_WIDTH_CH = 32

/** A version of an asset. */
export interface Version extends backendService.S3ObjectVersion {
  readonly number: number
  readonly title: string
  readonly tags: string[]
}

/** Options for duplicating an asset. */
export interface DuplicateOptions {
  readonly start?: boolean
  readonly versionId?: backendService.S3ObjectVersionId
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

  const { getText } = useText()

  const isProject = item.type === backendService.AssetType.project
  const comparableVersions = otherVersions
    .map((v, index) => ({ ...v, number: otherVersions.length - index }))
    .filter((v) => v.versionId !== version.versionId)

  const canRestore = !version.isLatest
  const doRestore = useEventCallback(async () => {
    await doRestoreRaw(version)
  })

  // Conditional collapsing based on available headerBounds with respect to the approximate width of all tags.
  // The width of tags is based on the first tag, so it can be innacurate, but should be reasonable in practice.
  const [headerRef, headerBounds] = useMeasure()
  const [fullTitleRef, fullTitleBounds] = useMeasure()
  const [minTagRef, minTagBounds] = useMeasure()

  const shouldCollapseTags = React.useMemo(() => {
    if (version.tags.length === 0) {
      return false
    }
    if (headerBounds == null || fullTitleBounds == null || minTagBounds == null) {
      return true
    }

    const minimumTagsWidth =
      version.tags.length * minTagBounds.width + Math.max(version.tags.length - 1, 0) * TAG_GAP_PX
    const requiredWidth = fullTitleBounds.width + HEADER_GAP_PX + minimumTagsWidth

    return requiredWidth > headerBounds.width
  }, [fullTitleBounds, headerBounds, minTagBounds, version.tags.length])

  const CollapsedTagsPlaceholder = () => {
    const collapsedTagsTooltip = (
      <div className="flex flex-col items-start gap-1 pl-2">
        {version.tags.map((tag, index) => (
          <Text key={`${version.versionId}-tooltip-${index}`} color="inherit">
            {tag}
          </Text>
        ))}
      </div>
    )
    return (
      <VisualTooltip tooltip={collapsedTagsTooltip} className="shrink-0">
        <Badge variant="outline">{getText('xTags', version.tags.length)}</Badge>
      </VisualTooltip>
    )
  }

  return (
    <div className="grid w-full select-none grid-cols-[minmax(0,1fr)_auto] items-center gap-4">
      <div className="relative flex flex-1 flex-col">
        <div ref={headerRef} className="flex min-w-0 items-center gap-2">
          <Text
            variant="body"
            truncate={shouldCollapseTags}
            nowrap={!shouldCollapseTags}
            className="min-width-0 shrink-0"
          >
            {version.title}
          </Text>
          {version.tags.length > 0 &&
            (shouldCollapseTags ?
              <CollapsedTagsPlaceholder />
            : <div className="flex min-w-0 items-center gap-1 overflow-hidden">
                {version.tags.map((tag, index) => (
                  <VisualTooltip
                    key={`${version.versionId}-${tag}-${index}`}
                    tooltip={tag}
                    className={`min-w-[${MIN_TAG_WIDTH_CH}ch] max-w-[${MAX_TAG_WIDTH_CH}ch] shrink overflow-hidden`}
                  >
                    <Badge variant="outline" className="w-full">
                      {tag}
                    </Badge>
                  </VisualTooltip>
                ))}
              </div>)}
        </div>

        {/* Tags list copies to measure sizes for conditional collapse behavior. */}
        <div className="pointer-events-none absolute h-0 overflow-hidden opacity-0">
          <Text ref={fullTitleRef} variant="body" nowrap>
            {version.title}
          </Text>
          <span ref={minTagRef} className="inline-block" style={{ width: `${MIN_TAG_WIDTH_CH}ch` }}>
            <Badge variant="outline" className="w-full">
              {version.tags[0] ?? getText('latestIndicator')}
            </Badge>
          </span>
        </div>

        <div className="flex items-center gap-2">
          <div className={TEXT_WITH_ICON().base({ gap: 'medium', className: 'flex-none' })}>
            <Icon size="small" icon="calendar" className={TEXT_WITH_ICON().icon()} />
            <Text elementType="time" variant="body-sm" className={TEXT_WITH_ICON().text()}>
              {toReadableIsoString(new Date(version.lastModified))}
            </Text>
          </div>

          {version.user && <UserWithPopover user={version.user} />}
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

            <Menu.Item
              onAction={() => doDuplicate({ versionId: version.versionId })}
              icon="duplicate"
            >
              {getText('duplicateThisVersion')}
            </Menu.Item>

            {isProject && (
              <Menu.Item
                onAction={() => doDuplicate({ start: true, versionId: version.versionId })}
                icon="copy"
              >
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
  readonly doDuplicate?: ((options?: DuplicateOptions) => Promise<void> | void) | undefined
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
                await doDuplicate({ versionId: version.versionId })
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
