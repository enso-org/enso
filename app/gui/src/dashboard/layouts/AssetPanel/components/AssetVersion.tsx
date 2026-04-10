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
const COMMENT_ACTION_BUTTON_WIDTH_BUDGET_PX = 24
const COMMENT_ACTION_BUTTON_CLASS_NAME =
  'opacity-30 transition-opacity hover:opacity-100 focus-visible:opacity-100'
const COMMENT_TEXT_AREA_CLASS_NAME =
  'min-h-7 w-full resize-none rounded-md border border-primary/20 bg-transparent px-2 py-1 text-[10.5px] leading-4 text-primary focus:border-primary disabled:cursor-default disabled:opacity-50'

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
  readonly doUpdateComment: (version: Version, comment: string | null) => Promise<void> | void
  readonly isUpdatingComment?: boolean
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
    doUpdateComment,
    isUpdatingComment = false,
  } = props

  const { getText } = useText()
  const versionComment = normalizeVersionComment(version.comment)

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
    const addCommentButtonWidth = versionComment == null ? COMMENT_ACTION_BUTTON_WIDTH_BUDGET_PX : 0
    const requiredWidth =
      fullTitleBounds.width + HEADER_GAP_PX + addCommentButtonWidth + minimumTagsWidth

    return requiredWidth > headerBounds.width
  }, [fullTitleBounds, headerBounds, minTagBounds, version.tags.length, versionComment])

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
        <VersionComment
          comment={versionComment}
          isUpdating={isUpdatingComment}
          onUpdateComment={(comment) => doUpdateComment(version, comment)}
        >
          {({ isEditing, startEditing }) => (
            <>
              <div ref={headerRef} className="flex min-w-0 items-center gap-2">
                <Text
                  variant="body"
                  truncate={shouldCollapseTags}
                  nowrap={!shouldCollapseTags}
                  className="min-width-0 shrink-0"
                >
                  {version.title}
                </Text>
                {!isEditing && versionComment == null && (
                  <CommentActionButton
                    icon="comment"
                    label={getText('assetVersions.addComment')}
                    isUpdating={isUpdatingComment}
                    onPress={startEditing}
                  />
                )}
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
                <span
                  ref={minTagRef}
                  className="inline-block"
                  style={{ width: `${MIN_TAG_WIDTH_CH}ch` }}
                >
                  <Badge variant="outline" className="w-full">
                    {version.tags[0] ?? getText('latestIndicator')}
                  </Badge>
                </span>
              </div>
            </>
          )}
        </VersionComment>

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
        className="mt-1 shrink-0 grow-0 self-start"
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

/** Props for a {@link CommentActionButton}. */
interface CommentActionButtonProps {
  readonly icon: 'comment' | 'edit'
  readonly label: string
  readonly isUpdating?: boolean
  readonly onPress: () => void
}

/** Small faded action button used for version comment controls. */
function CommentActionButton(props: CommentActionButtonProps) {
  const { icon, label, isUpdating = false, onPress } = props

  return (
    <Button
      size="xsmall"
      variant="icon"
      icon={icon}
      className={COMMENT_ACTION_BUTTON_CLASS_NAME}
      aria-label={label}
      isLoading={isUpdating}
      isDisabled={isUpdating}
      onPress={onPress}
    >
      {null}
    </Button>
  )
}

/** Props for a {@link VersionComment}. */
interface VersionCommentProps {
  readonly comment?: string | null | undefined
  readonly isUpdating?: boolean
  readonly onUpdateComment: (comment: string | null) => Promise<void> | void
  readonly children: (props: VersionCommentRenderProps) => React.ReactNode
}

/** Props exposed by {@link VersionComment} to its render prop. */
interface VersionCommentRenderProps {
  readonly isEditing: boolean
  readonly startEditing: () => void
}

/** Inline version comment control that owns add/edit state and the comment editor. */
function VersionComment(props: VersionCommentProps) {
  const { comment, isUpdating = false, onUpdateComment, children } = props
  const { getText } = useText()
  const [isEditing, setIsEditing] = React.useState(false)
  const [draft, setDraft] = React.useState(comment ?? '')
  const commentInputRef = React.useRef<HTMLTextAreaElement | null>(null)
  const shouldIgnoreBlurRef = React.useRef(false)

  const startEditing = useEventCallback(() => {
    setDraft(comment ?? '')
    setIsEditing(true)
  })
  const cancelEditing = useEventCallback(() => {
    shouldIgnoreBlurRef.current = true
    setDraft(comment ?? '')
    setIsEditing(false)
  })
  const submitComment = useEventCallback(() => {
    const nextComment = normalizeVersionComment(draft.trim())
    setIsEditing(false)
    if (nextComment === comment) {
      return
    }
    void Promise.resolve().then(() => onUpdateComment(nextComment))
  })

  React.useEffect(() => {
    if (!isEditing) {
      return
    }

    const commentInput = commentInputRef.current
    if (commentInput == null) {
      return
    }

    commentInput.focus({ preventScroll: true })
    const selectionEnd = commentInput.value.length
    commentInput.setSelectionRange(0, selectionEnd)
  }, [isEditing])

  return (
    <>
      {children({ isEditing, startEditing })}

      {(isEditing || comment) && (
        <div className="flex min-w-0 items-center gap-1.5">
          {isEditing ?
            <textarea
              ref={commentInputRef}
              value={draft}
              maxLength={256}
              rows={1}
              disabled={isUpdating}
              aria-label={getText('assetVersions.editComment')}
              className={COMMENT_TEXT_AREA_CLASS_NAME}
              onBlur={() => {
                if (shouldIgnoreBlurRef.current) {
                  shouldIgnoreBlurRef.current = false
                  return
                }
                submitComment()
              }}
              onChange={(event) => {
                setDraft(event.currentTarget.value.replace(/[\r\n]+/g, ' '))
              }}
              onKeyDown={(event) => {
                if (event.key === 'Enter') {
                  event.preventDefault()
                  submitComment()
                }

                if (event.key === 'Escape') {
                  event.preventDefault()
                  cancelEditing()
                }
              }}
            />
          : comment && (
              <>
                <CommentActionButton
                  icon="edit"
                  label={getText('assetVersions.editComment')}
                  isUpdating={isUpdating}
                  onPress={startEditing}
                />
                <Text variant="body-sm" color="primary" nowrap="normal" className="min-w-0">
                  {comment}
                </Text>
              </>
            )
          }
        </div>
      )}
    </>
  )
}

/** Normalize empty version comments to an absent value. */
function normalizeVersionComment(comment: string | null | undefined): string | null {
  return !comment ? null : comment
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
