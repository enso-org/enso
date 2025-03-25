/** @file A modal opened when uploaded assets. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import AssetSummary from '#/components/dashboard/AssetSummary'
import Modal from '#/components/Modal'

import * as backendModule from '#/services/Backend'

import {
  Button,
  Dialog,
  Form,
  Input,
  Menu,
  Popover,
  Separator,
  Text,
} from '#/components/AriaComponents'
import { Icon } from '#/components/Icon'
import { listDirectoryQueryOptions, unsafe_assetFromCacheQueryOptions } from '#/hooks/backendHooks'
import { useMount } from '#/hooks/mountHooks'
import { useCategory } from '#/layouts/Drive/Categories/categoriesHooks'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import * as fileInfo from '#/utilities/fileInfo'
import * as object from '#/utilities/object'
import {
  useMutation,
  useQueryClient,
  useSuspenseQueries,
  useSuspenseQuery,
} from '@tanstack/react-query'
import { Fragment } from 'react'
import invariant from 'tiny-invariant'

/**
 * An object containing the current asset, and the asset that is about to be uploaded,
 * that will conflict with the existing asset.
 */
export interface ConflictingAsset<
  Asset extends backendModule.FileAsset | backendModule.ProjectAsset =
    | backendModule.FileAsset
    | backendModule.ProjectAsset,
> {
  readonly current: backendModule.AnyAsset
  readonly new: Asset
  readonly file: File
}

/** Props for a {@link DuplicateAssetsModal}. */
export interface DuplicateAssetsModalProps {
  readonly parentKey: backendModule.DirectoryId
  readonly parentId: backendModule.DirectoryId
  readonly conflictingFiles: readonly ConflictingAsset<backendModule.FileAsset>[]
  readonly conflictingProjects: readonly ConflictingAsset<backendModule.ProjectAsset>[]
  readonly siblingFileNames: Iterable<string>
  readonly siblingProjectNames: Iterable<string>
  readonly nonConflictingFileCount: number
  readonly nonConflictingProjectCount: number
  readonly doUploadNonConflicting: () => Promise<void> | void
  readonly doUpdateConflicting: (toUpdate: ConflictingAsset[]) => Promise<void> | void
}

/**
 * A modal for creating a new label.
 * @deprecated Use {@link resolveDuplications} instead.
 */
export default function DuplicateAssetsModal(props: DuplicateAssetsModalProps) {
  const { conflictingFiles: conflictingFilesRaw } = props
  const { conflictingProjects: conflictingProjectsRaw, doUpdateConflicting } = props
  const { siblingFileNames: siblingFileNamesRaw } = props
  const { siblingProjectNames: siblingProjectNamesRaw } = props
  const { nonConflictingFileCount, nonConflictingProjectCount, doUploadNonConflicting } = props
  const { getText } = textProvider.useText()
  const [conflictingFiles, setConflictingFiles] = React.useState(conflictingFilesRaw)
  const [conflictingProjects, setConflictingProjects] = React.useState(conflictingProjectsRaw)
  const [didUploadNonConflicting, setDidUploadNonConflicting] = React.useState(false)
  const [siblingFileNames] = React.useState(new Set<string>())
  const [siblingProjectNames] = React.useState(new Set<string>())
  const count = conflictingFiles.length + conflictingProjects.length
  const firstConflict = conflictingFiles[0] ?? conflictingProjects[0]
  const otherFilesCount = Math.max(0, conflictingFiles.length - 1)
  const otherProjectsCount = conflictingProjects.length - (conflictingFiles.length > 0 ? 0 : 1)
  const updateConflictingMutation = useMutation({
    mutationKey: ['updateConflicting'],
    mutationFn: async (...args: Parameters<typeof doUpdateConflicting>) => {
      await doUpdateConflicting(...args)
    },
  })
  const uploadNonConflictingMutation = useMutation({
    mutationKey: ['uploadNonConflicting'],
    mutationFn: async (...args: Parameters<typeof doUploadNonConflicting>) => {
      await doUploadNonConflicting(...args)
    },
  })
  const isLoading = uploadNonConflictingMutation.isPending || updateConflictingMutation.isPending

  React.useEffect(() => {
    for (const name of siblingFileNamesRaw) {
      siblingFileNames.add(name)
    }
    for (const name of siblingProjectNamesRaw) {
      siblingProjectNames.add(name)
    }
    // Note that because the props are `Iterable`s, they may be different each time
    // even if their contents are identical. However, as this component should never
    // be re-rendered with different props, the dependency list should not matter anyway.
  }, [siblingFileNames, siblingFileNamesRaw, siblingProjectNames, siblingProjectNamesRaw])

  const findNewName = (conflict: ConflictingAsset, commit = true) => {
    let title = conflict.file.name
    switch (conflict.new.type) {
      case backendModule.AssetType.file: {
        const { basename, extension } = fileInfo.basenameAndExtension(title)
        let i = 1
        while (true) {
          i += 1
          const candidateTitle = `${basename} ${i}.${extension}`
          if (!siblingFileNames.has(candidateTitle)) {
            if (commit) {
              siblingFileNames.add(candidateTitle)
            }
            title = candidateTitle
            break
          }
        }
        break
      }
      case backendModule.AssetType.project: {
        const { basename, extension } = backendModule.extractProjectExtension(title)
        title = basename
        let i = 1
        while (true) {
          i += 1
          const candidateTitle = `${title} ${i}`
          if (!siblingProjectNames.has(candidateTitle)) {
            if (commit) {
              siblingProjectNames.add(candidateTitle)
            }
            title = `${candidateTitle}.${extension}`
            break
          }
        }
        break
      }
    }
    return title
  }

  const doRename = (toRename: ConflictingAsset[]) => {
    const clonedConflicts = structuredClone(toRename)

    for (const conflict of clonedConflicts) {
      // This is SAFE, as it is a shallow mutation of a freshly cloned object.
      object.unsafeMutable(conflict.new).title = findNewName(conflict)
    }

    return clonedConflicts
  }

  return (
    <Modal centered className="absolute bg-dim">
      <form
        data-testid="new-label-modal"
        tabIndex={-1}
        className="pointer-events-auto relative flex w-duplicate-assets-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={(event) => {
          event.preventDefault()
        }}
      >
        <aria.Heading level={2} className="relative text-sm font-semibold">
          {conflictingFiles.length > 0 ?
            conflictingProjects.length > 0 ?
              getText('duplicateFilesAndProjectsFound')
            : getText('duplicateFilesFound')
          : getText('duplicateProjectsFound')}
        </aria.Heading>
        {nonConflictingFileCount > 0 ||
          (nonConflictingProjectCount > 0 && (
            <div className="relative flex flex-col">
              {nonConflictingFileCount > 0 && (
                <aria.Text className="text">
                  {nonConflictingFileCount === 1 ?
                    getText('fileWithoutConflicts')
                  : getText('filesWithoutConflicts', nonConflictingFileCount)}
                </aria.Text>
              )}
              {nonConflictingProjectCount > 0 && (
                <aria.Text className="text">
                  {nonConflictingProjectCount === 1 ?
                    getText('projectWithoutConflicts')
                  : getText('projectsWithoutConflicts', nonConflictingFileCount)}
                </aria.Text>
              )}
              <ariaComponents.Button
                variant="outline"
                isDisabled={didUploadNonConflicting}
                onPress={async () => {
                  await doUploadNonConflicting()
                  setDidUploadNonConflicting(true)
                }}
              >
                {didUploadNonConflicting ? getText('uploaded') : getText('upload')}
              </ariaComponents.Button>
            </div>
          ))}
        {firstConflict && (
          <>
            <div className="flex flex-col">
              <aria.Text className="relative">{getText('currentColon')}</aria.Text>
              <AssetSummary asset={firstConflict.current} className="relative" />
            </div>
            <div className="flex flex-col">
              <aria.Text className="relative">{getText('newColon')}</aria.Text>
              <AssetSummary
                new
                newName={backendModule.stripProjectExtension(findNewName(firstConflict, false))}
                asset={firstConflict.new}
                className="relative"
              />
            </div>
            {count > 1 && (
              <ariaComponents.ButtonGroup>
                <ariaComponents.Button
                  variant="outline"
                  onPress={async () => {
                    switch (firstConflict.new.type) {
                      case backendModule.AssetType.file: {
                        setConflictingFiles((oldConflicts) => oldConflicts.slice(1))
                        break
                      }
                      case backendModule.AssetType.project: {
                        setConflictingProjects((oldConflicts) => oldConflicts.slice(1))
                        break
                      }
                    }
                    await doUpdateConflicting([firstConflict])
                  }}
                >
                  {getText('update')}
                </ariaComponents.Button>

                <ariaComponents.Button
                  variant="outline"
                  onPress={() => {
                    doRename([firstConflict])
                    switch (firstConflict.new.type) {
                      case backendModule.AssetType.file: {
                        setConflictingFiles((oldConflicts) => oldConflicts.slice(1))
                        break
                      }
                      case backendModule.AssetType.project: {
                        setConflictingProjects((oldConflicts) => oldConflicts.slice(1))
                        break
                      }
                    }
                  }}
                >
                  {firstConflict.new.type === backendModule.AssetType.file ?
                    getText('renameNewFile')
                  : getText('renameNewProject')}
                </ariaComponents.Button>
              </ariaComponents.ButtonGroup>
            )}
          </>
        )}
        {otherFilesCount > 0 && (
          <aria.Text className="relative">
            {otherFilesCount === 1 ?
              getText('andOtherFile')
            : getText('andOtherFiles', otherFilesCount)}
          </aria.Text>
        )}
        {otherProjectsCount > 0 && (
          <aria.Text className="relative">
            {otherProjectsCount === 1 ?
              getText('andOtherProject')
            : getText('andOtherProjects', otherProjectsCount)}
          </aria.Text>
        )}

        <ariaComponents.ButtonGroup className="relative">
          <ariaComponents.Button
            variant="submit"
            loading={isLoading}
            onPress={async () => {
              await Promise.allSettled([
                uploadNonConflictingMutation.mutateAsync(),
                updateConflictingMutation.mutateAsync([
                  ...conflictingFiles,
                  ...conflictingProjects,
                ]),
              ])
              unsetModal()
            }}
          >
            {count === 1 ? getText('update') : getText('updateAll')}
          </ariaComponents.Button>

          <ariaComponents.Button
            variant="accent"
            loading={isLoading}
            onPress={async () => {
              const resolved = doRename([...conflictingFiles, ...conflictingProjects])
              await Promise.allSettled([
                uploadNonConflictingMutation.mutateAsync(),
                updateConflictingMutation.mutateAsync(resolved),
              ])
              unsetModal()
            }}
          >
            {count === 1 ?
              firstConflict?.new.type === backendModule.AssetType.file ?
                getText('renameNewFile')
              : getText('renameNewProject')
            : firstConflict?.new.type === backendModule.AssetType.file ?
              getText('renameNewFiles')
            : getText('renameNewProjects')}
          </ariaComponents.Button>
          <ariaComponents.Button variant="outline" loading={isLoading} onPress={unsetModal}>
            {getText('cancel')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </form>
    </Modal>
  )
}

/**
 * The conclusion of a resolved duplication.
 */
export type Conclusion = 'rename' | 'replace' | 'skip'

/**
 * A resolved duplication.
 */
export type ResolvedDuplication = RenameDuplication | ReplaceDuplication | SkipDuplication

/**
 * A resolved duplication that was skipped.
 */
export interface SkipDuplication {
  readonly assetId: backendModule.AssetId
  readonly conclusion: 'skip'
}

/**
 * A resolved duplication that was renamed.
 */
export interface RenameDuplication {
  readonly assetId: backendModule.AssetId
  readonly conclusion: 'rename'
  readonly newName: string
}

/**
 * A resolved duplication that was replaced.
 */
export interface ReplaceDuplication {
  readonly assetId: backendModule.AssetId
  /**
   * Requires backend to support that.
   */
  readonly conclusion: 'replace'
}

/**
 * Props for a {@link ResolveDuplicationsModal}.
 */
export interface ResolveDuplicationsProps {
  readonly targetId: backendModule.DirectoryId
  readonly conflictingIds: readonly backendModule.AssetId[]
  readonly onSubmit: (assets: readonly ResolvedDuplication[]) => Promise<void> | void
  readonly onCancel: () => void
}

/**
 * A modal for resolving duplicates.
 */
export function ResolveDuplicationsModal(props: ResolveDuplicationsProps) {
  const { conflictingIds } = props
  const { getText } = textProvider.useText()

  return (
    <Dialog
      size="xxlarge"
      onDismiss={props.onCancel}
      title={
        conflictingIds.length === 1 ?
          getText('resolveDuplicatesTitleOne')
        : getText('resolveDuplicatesTitleMany', conflictingIds.length)
      }
    >
      <ResolveDuplicationsModalInner {...props} />
    </Dialog>
  )
}

const NEW_TITLE_SUFFIX = ' (copy)'

/**
 * The inner component of a {@link ResolveDuplicationsModal}.
 */
function ResolveDuplicationsModalInner(props: ResolveDuplicationsProps) {
  const { targetId, conflictingIds } = props

  const { category, associatedBackend } = useCategory()

  const { getText } = textProvider.useText()

  const queryClient = useQueryClient()

  const { data: siblingFiles } = useSuspenseQuery({
    ...listDirectoryQueryOptions({
      category,
      backend: associatedBackend,
      parentId: targetId,
      refetchInterval: null,
    }),
    select: (data) => {
      // We use titles as keys, because they are always unique, and we want to find duplicates by title.
      const map = new Map(data.map((asset) => [asset.title, asset]))
      return { map, siblings: data }
    },
  })

  const conflictingAssets = useSuspenseQueries({
    queries: conflictingIds.map((id) =>
      unsafe_assetFromCacheQueryOptions({ backend: associatedBackend, assetId: id, queryClient }),
    ),
    combine: (queries) => queries.map((query) => query.data).filter((asset) => asset != null),
  })

  useMount(() => {
    const onlyExistingConflicts = conflictingAssets.filter(
      (asset) => siblingFiles.map.get(asset.title) != null,
    )

    // If there are no conflicts, we can just skip the modal and return nothing.
    if (onlyExistingConflicts.length === 0) {
      void props.onSubmit([])
    }
  })

  return (
    <Form
      defaultValues={Object.fromEntries(
        conflictingAssets.map((asset) => [asset.id, { assetId: asset.id, type: asset.type }]),
      )}
      method="dialog"
      className="pb-20"
      schema={(schema) =>
        schema.object(
          Object.fromEntries(
            conflictingAssets.map((asset) => [
              asset.id,
              schema
                .object({
                  assetId: schema.custom<backendModule.AssetId>(),
                  type: schema.nativeEnum(backendModule.AssetType),
                  newName: schema.string().trim(),
                  conclusion: schema.literal('rename', { message: getText('invalidConclusion') }),
                })
                .or(
                  schema.object({
                    assetId: schema.custom<backendModule.AssetId>(),
                    type: schema.nativeEnum(backendModule.AssetType),
                    conclusion: schema.literal('skip', { message: getText('invalidConclusion') }),
                  }),
                )
                .or(
                  schema.object({
                    assetId: schema.custom<backendModule.AssetId>(),
                    type: schema.nativeEnum(backendModule.ReplaceableAssetType, {
                      message: getText('invalidConclusion'),
                    }),
                    conclusion: schema.literal('replace', {
                      message: getText('invalidConclusion'),
                    }),
                  }),
                ),
            ]),
          ),
        )
      }
      onSubmit={(data) => props.onSubmit(Object.values(data))}
    >
      {({ form }) => (
        <>
          <Text elementType="p">
            {conflictingIds.length === 1 ?
              getText('resolveDuplicatesDescriptionOne')
            : getText('resolveDuplicatesDescriptionMany', conflictingIds.length)}
          </Text>

          {conflictingAssets.map((asset, index, array) => {
            const isLast = index === array.length - 1
            const sibling = siblingFiles.map.get(asset.title)

            invariant(sibling != null, 'Sibling was not found, this should never happen.')

            return (
              <Fragment key={asset.id}>
                <div className="grid w-full grid-cols-[minmax(0,1fr)_auto_minmax(0,1fr)] grid-rows-[auto_auto_auto] gap-2">
                  <AssetSummary asset={asset} new />

                  <Icon icon="arrow_right" size="medium" className="self-center" />

                  <AssetSummary asset={sibling} />

                  <Button.Group className="col-span-full row-span-2 mt-1">
                    <Form.Controller
                      control={form.control}
                      name={`${asset.id}.conclusion`}
                      render={({ field, fieldState }) => {
                        if (fieldState.isDirty) {
                          return (
                            <div className="flex items-center gap-2">
                              {field.value === 'skip' && (
                                <Text>{getText('assetWillBeSkipped')}</Text>
                              )}

                              {field.value === 'rename' && (
                                <Form.FieldValue name={`${asset.id}.newName`}>
                                  {(value: string) => (
                                    <Text>{getText('assetWillBeRenamed', value)}</Text>
                                  )}
                                </Form.FieldValue>
                              )}

                              {field.value === 'replace' && (
                                <Text>{getText('assetWillBeReplaced')}</Text>
                              )}

                              <Button
                                variant="link"
                                onPress={() => {
                                  form.resetField(`${asset.id}.conclusion`)
                                }}
                              >
                                {getText('change')}
                              </Button>
                            </div>
                          )
                        }

                        return (
                          <Button.Group buttonVariants={{ size: 'xsmall' }}>
                            <Button
                              variant="outline"
                              className="min-w-16"
                              onPress={() => {
                                field.onChange('skip')
                              }}
                            >
                              {getText('skip')}
                            </Button>

                            <Popover.Trigger>
                              <Button variant="primary" className="min-w-16">
                                {getText('rename')}
                              </Button>

                              <Popover placement="bottom start">
                                <Form
                                  method="dialog"
                                  defaultValues={{ newName: asset.title + NEW_TITLE_SUFFIX }}
                                  schema={(schema) =>
                                    schema.object({
                                      newName: backendModule.titleSchema({
                                        asset,
                                        siblings: siblingFiles.siblings,
                                      }),
                                    })
                                  }
                                  onSubmit={(value) => {
                                    field.onChange('rename')
                                    form.setValue(`${asset.id}.newName`, value.newName)
                                  }}
                                >
                                  <Text>{getText('newNameDescription')}</Text>

                                  <Input
                                    label={getText('newName')}
                                    name="newName"
                                    autoFocus="select"
                                  />

                                  <Form.Submit>{getText('apply')}</Form.Submit>

                                  <Form.FormError />
                                </Form>
                              </Popover>
                            </Popover.Trigger>
                          </Button.Group>
                        )
                      }}
                    />
                  </Button.Group>

                  <Form.FieldError
                    form={form}
                    className="col-span-full row-span-3"
                    name={`${asset.id}.conclusion`}
                  />
                </div>

                {!isLast && <Separator className="my-2" />}
              </Fragment>
            )
          })}

          <Button.Group
            className={
              'fixed bottom-0 left-0 right-0 border-t-0.5 border-primary/20 bg-background/90 px-3 py-4 backdrop-blur-md'
            }
          >
            <Dialog.Close variant="ghost" onPress={props.onCancel} className="mr-auto">
              {getText('cancel')}
            </Dialog.Close>

            <Button.GroupJoin className="grow-0">
              <Button
                variant="outline"
                className="min-w-20"
                onPress={() => {
                  for (const asset of conflictingAssets) {
                    form.setValue(`${asset.id}.conclusion`, 'skip', { shouldDirty: true })
                  }
                }}
              >
                {getText('skipAll')}
              </Button>

              <Menu.Trigger>
                <Button variant="outline" icon="folder_opened" />

                <Menu>
                  <Menu.Item
                    onAction={() => {
                      for (const asset of conflictingAssets) {
                        const conclusion = form.getValues(`${asset.id}.conclusion`)

                        // The value COULD be `null` or `undefined`, might be unset by the moment
                        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
                        if (conclusion == null) {
                          form.setValue(`${asset.id}.conclusion`, 'skip', { shouldDirty: true })
                        }
                      }
                    }}
                  >
                    {getText('skipRest')}
                  </Menu.Item>
                </Menu>
              </Menu.Trigger>
            </Button.GroupJoin>

            <Form.Submit className="min-w-20">{getText('apply')}</Form.Submit>
          </Button.Group>

          <Form.FormError />
        </>
      )}
    </Form>
  )
}

/**
 * Options for resolving duplicates.
 */
export interface ResolveDuplicationsOptions
  extends Pick<ResolveDuplicationsProps, 'conflictingIds' | 'targetId'> {}

/**
 * Function for resolving duplicates.
 */
export async function resolveDuplications(props: ResolveDuplicationsOptions) {
  const { targetId, conflictingIds } = props

  return new Promise<readonly ResolvedDuplication[]>((resolve, reject) => {
    setModal(
      <ResolveDuplicationsModal
        targetId={targetId}
        conflictingIds={conflictingIds}
        onSubmit={resolve}
        onCancel={reject}
      />,
    )
  }).finally(unsetModal)
}
