/** @file Displays information describing a specific version of an asset. */
import { Badge } from '#/components/Badge'
import { Button } from '#/components/Button'
import { Dialog, Popover } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import { BasicInput } from '#/components/Inputs/Input'
import { Menu } from '#/components/Menu'
import { TEXT_WITH_ICON } from '#/components/patterns'
import { Text, TEXT_STYLE } from '#/components/Text'
import { UserWithPopover } from '#/components/UserWithPopover'
import { VisualTooltip } from '#/components/VisualTooltip'
import {
  backendQueryOptions,
  useAddAssetVersionTag,
  useRemoveAssetVersionTag,
} from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMeasure } from '#/hooks/measureHooks'
import { setModal } from '#/providers/ModalProvider'
import { tv } from '#/utilities/tailwindVariants'
import { useText } from '$/providers/react'
import { useQuery } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import * as backendService from 'enso-common/src/services/Backend'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import * as React from 'react'
import { useFilter } from 'react-aria-components'
import { AssetDiffView } from './AssetDiffView'

const HEADER_GAP_PX = 8
const TAG_GAP_PX = 4
const MIN_TAG_WIDTH_CH = 8
const MAX_TAG_WIDTH_CH = 32

/** Options for add tag compontent. */
interface AddTagProps {
  readonly availableTags: readonly string[]
  readonly backend: Backend
  readonly item: backendService.AnyAsset
  readonly version: Version
}

const ADD_TAG_STYLES = tv({
  slots: {
    form: 'flex w-80 max-w-[min(20rem,calc(100vw-2rem))] flex-col gap-2',
    inputRow: 'flex items-center gap-2',
    input:
      'w-full rounded-full border-0.5 border-primary/20 bg-transparent px-3 py-1.5 text-xs text-primary outline-none transition-colors placeholder:text-primary/40 focus:border-primary',
    suggestions: 'flex max-h-56 flex-col overflow-y-auto overflow-x-hidden',
    suggestionButton:
      'w-full justify-start rounded-full px-3 py-1.5 text-left text-xs font-medium text-primary hover:bg-primary/5',
  },
})

/** Add tag popover content. */
function AddTag(props: AddTagProps) {
  const { availableTags, item, version, backend } = props
  const { getText } = useText()
  const styles = ADD_TAG_STYLES()
  const filter = useFilter({ sensitivity: 'base' })
  const [value, setValue] = React.useState('')
  const deferredValue = React.useDeferredValue(value)

  const addAssetVersionTag = useAddAssetVersionTag(backend)
  const normalizedValue = value.trim()
  const filteredTags = React.useMemo(() => {
    const existingTags = new Set(version.tags)
    return availableTags.filter(
      (tag) =>
        tag.trim() !== '' &&
        !existingTags.has(tag) &&
        (deferredValue.trim() === '' || filter.contains(tag, deferredValue)),
    )
  }, [availableTags, deferredValue, filter, version.tags])

  const submit = useEventCallback(async (tag: string, close: () => void) => {
    const normalizedTag = tag.trim()
    if (normalizedTag === '' || version.tags.includes(normalizedTag)) {
      return
    }
    await addAssetVersionTag(item.id, version.versionId, normalizedTag)
    setValue('')
    close()
  })

  return (
    <Popover.Trigger>
      <Button
        variant="icon"
        size="xxsmall"
        icon="add"
        tooltip={getText('assetVersions.addTag')}
        className="shrink-0 opacity-40 hover:opacity-100"
      />
      {({ close }: { close: () => void }) => (
        <Popover size="auto" placement="bottom start">
          <form
            className={styles.form()}
            onSubmit={(event) => {
              event.preventDefault()
              void submit(normalizedValue, close)
            }}
          >
            <div className={styles.inputRow()}>
              <BasicInput
                autoFocus
                value={value}
                onChange={(event) => {
                  setValue(event.currentTarget.value)
                }}
                placeholder={getText('assetVersions.addTag')}
                aria-label={getText('assetVersions.addTag')}
                className={styles.input()}
                onKeyDown={(event) => {
                  if (event.key === 'Enter') {
                    event.preventDefault()
                    void submit(normalizedValue, close)
                  } else if (event.key === 'Escape') {
                    event.preventDefault()
                    close()
                  }
                }}
              />
            </div>
            {filteredTags.length > 0 && (
              <div className={styles.suggestions()}>
                {filteredTags.map((tag) => (
                  <Button
                    key={tag}
                    variant="custom"
                    className={styles.suggestionButton()}
                    onPress={() => submit(tag, close)}
                  >
                    {tag}
                  </Button>
                ))}
              </div>
            )}
          </form>
        </Popover>
      )}
    </Popover.Trigger>
  )
}

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
  const { data: availableTags } = useQuery(backendQueryOptions(backend, 'listAssetVersionTags', []))

  const canRestore = !version.isLatest
  const doRestore = useEventCallback(async () => {
    await doRestoreRaw(version)
  })

  const removeAssetVersionTag = useRemoveAssetVersionTag(backend)
  const onDelete = (versionId: backendService.S3ObjectVersionId, tag: string) =>
    removeAssetVersionTag(item.id, versionId, tag)

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
            : <div className="flex min-w-0 items-center gap-1">
                {version.tags.map((tag, index) => (
                  <div
                    key={`${version.versionId}-${tag}-${index}`}
                    className={`min-w-0 min-w-[${MIN_TAG_WIDTH_CH}ch] max-w-[${MAX_TAG_WIDTH_CH}ch] shrink`}
                  >
                    <Tag
                      tooltip={tag}
                      onDelete={
                        tag !== getText('latestIndicator') ?
                          () => onDelete(version.versionId, tag)
                        : undefined
                      }
                    >
                      {tag}
                    </Tag>
                  </div>
                ))}
              </div>)}
          <AddTag
            availableTags={availableTags ?? []}
            item={item}
            version={version}
            backend={backend}
          />
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

/** Tag props. */
interface TagProps {
  readonly children: React.ReactNode
  readonly className?: string
  readonly tooltip: string
  readonly onDelete?: (() => void) | undefined
}

const TAG_STYLES = tv({
  base: 'flex items-center min-w-0 w-full rounded-full border-[0.5px] text-primary overflow-visible',
  variants: {
    variant: {
      deleteButton: 'pl-2 pr-1',
      noDeleteButton: 'px-2',
    },
  },
  slots: {
    deleteButton: 'ml-1 flex-none opacity-40 hover:opacity-100',
    textWrapper: 'min-w-0 flex-1',
    text: TEXT_STYLE({ variant: 'body-sm', color: 'current', truncate: true }),
  },
})

/** Version tag component. */
function Tag(props: TagProps) {
  const { children, onDelete, tooltip, className } = props
  const styles = TAG_STYLES({
    className: className,
    variant: onDelete ? 'deleteButton' : 'noDeleteButton',
  })
  const { getText } = useText()
  return (
    <div className={styles.base()}>
      <div className={styles.textWrapper()}>
        <VisualTooltip tooltip={tooltip} className="block min-w-0">
          <span className={styles.text()}>{children}</span>
        </VisualTooltip>
      </div>
      {onDelete && (
        <Button
          icon="close"
          tooltip={getText('assetVersions.removeTag')}
          variant="icon"
          size="xxsmall"
          onPress={onDelete}
          className={styles.deleteButton()}
        />
      )}
    </div>
  )
}
