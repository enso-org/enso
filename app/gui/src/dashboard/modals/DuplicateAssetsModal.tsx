/** @file A modal opened when uploaded assets. */
import { Button } from '#/components/Button'
import { Dialog, Popover } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Icon } from '#/components/Icon'
import { Input } from '#/components/Inputs/Input'
import { Menu } from '#/components/Menu'
import { Separator } from '#/components/Separator'
import { Text } from '#/components/Text'
import { listDirectoryQueryOptions, unsafe_assetFromCacheQueryOptions } from '#/hooks/backendHooks'
import { useMount } from '#/hooks/mountHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useCategory } from '#/layouts/Drive/Categories'
import AssetSummary from '#/pages/dashboard/components/AssetSummary'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { regexEscape } from '#/utilities/string'
import { useText } from '$/providers/react'
import { useQueryClient, useSuspenseQueries } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import * as backendModule from 'enso-common/src/services/Backend'
import { FilterBy } from 'enso-common/src/services/Backend'
import { Fragment } from 'react'
import invariant from 'tiny-invariant'

/** Get a unique name based on sibling names. */
function getUniqueName(title: string, siblingTitles: readonly string[]) {
  title = title.match(/^.*(?= \((?:copy)? ?\d*\)$)/)?.[0] ?? title
  const regex = new RegExp(`^${regexEscape(title)}(?: \\((?:copy)? ?(\\d+)?\\))?$`)
  let maximum: number | null = null
  for (const siblingTitle of siblingTitles) {
    const [match, number] = siblingTitle.match(regex) ?? []
    let newMaximum: number
    if (match == null) {
      continue
    } else if (number == null) {
      newMaximum = 1
    } else {
      newMaximum = parseInt(number, 10)
    }
    maximum = Math.max(maximum ?? 0, newMaximum)
  }
  if (maximum == null) {
    return title
  }
  return `${title} (${maximum + 1})`
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
  readonly category?: Category
  readonly backend?: Backend
  /** Whether to show the 'replace'/'update' option. */
  readonly canReplace?: boolean
  readonly onSubmit: (assets: readonly ResolvedDuplication[]) => Promise<void> | void
  readonly onCancel: () => void
}

/**
 * A modal for resolving duplicates.
 */
export function ResolveDuplicationsModal(props: ResolveDuplicationsProps) {
  const { conflictingIds } = props
  const { getText } = useText()

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

/**
 * The inner component of a {@link ResolveDuplicationsModal}.
 */
function ResolveDuplicationsModalInner(props: ResolveDuplicationsProps) {
  const categoryInfo = useCategory()
  const {
    targetId,
    conflictingIds,
    category = categoryInfo.category,
    backend = categoryInfo.associatedBackend,
    canReplace = false,
  } = props

  const { getText } = useText()

  const queryClient = useQueryClient()

  const siblingFiles = useSuspenseQueries({
    queries: [
      listDirectoryQueryOptions({
        category,
        backend,
        parentId: targetId,
        labels: null,
        sortExpression: null,
        sortDirection: null,
        refetchInterval: null,
      }),
      listDirectoryQueryOptions({
        category,
        backend,
        parentId: targetId,
        filterBy: FilterBy.trashed,
        labels: null,
        sortExpression: null,
        sortDirection: null,
        refetchInterval: null,
      }),
    ],
    combine: (queries) => {
      const map = new Map<string, backendModule.AnyAsset>()
      const siblings = []
      for (const query of queries) {
        for (const asset of query.data.assets) {
          map.set(asset.title, asset)
          siblings.push(asset)
        }
      }
      return { map, siblings }
    },
  })
  const siblingTitles = siblingFiles.siblings.map((sibling) => sibling.title)

  const conflictingAssets = useSuspenseQueries({
    queries: conflictingIds.map((id) =>
      unsafe_assetFromCacheQueryOptions({ backend: backend, assetId: id, queryClient }),
    ),
    combine: (queries) => queries.map((query) => query.data).filter((asset) => asset != null),
  })

  const onlyExistingConflicts = conflictingAssets.filter(
    (asset) => siblingFiles.map.get(asset.title) != null,
  )

  // If there are no conflicts, we can just skip the modal and return nothing.
  useMount(() => {
    if (onlyExistingConflicts.length === 0) {
      void props.onSubmit([])
    }
  })

  if (onlyExistingConflicts.length === 0) {
    return null
  }

  return (
    <Form
      defaultValues={Object.fromEntries(
        conflictingAssets.map((asset) => [
          asset.id,
          {
            assetId: asset.id,
            type: asset.type,
            conclusion: 'default' as const,
            newName: getUniqueName(asset.title, siblingTitles),
          },
        ]),
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
                  conclusion: schema.enum(['default', 'rename'], {
                    message: getText('invalidConclusion'),
                  }),
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
      onSubmit={(data) =>
        props.onSubmit(
          Object.values(data).map((entry): ResolvedDuplication => {
            switch (entry.conclusion) {
              case 'default':
              case 'rename':
                return { ...entry, conclusion: 'rename' }
              case 'replace':
              case 'skip':
                return entry
            }
          }),
        )
      }
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
                      name={asset.id}
                      render={({ field }) => {
                        if (field.value.conclusion !== 'default') {
                          return (
                            <div className="flex items-center gap-2">
                              {field.value.conclusion === 'skip' && (
                                <Text>{getText('assetWillBeSkipped')}</Text>
                              )}

                              {field.value.conclusion === 'rename' && (
                                <Form.FieldValue name={`${asset.id}.newName`}>
                                  {(value: string) => (
                                    <Text>{getText('assetWillBeRenamed', value)}</Text>
                                  )}
                                </Form.FieldValue>
                              )}

                              {field.value.conclusion === 'replace' && (
                                <Text>{getText('assetWillBeReplaced')}</Text>
                              )}

                              <Button
                                variant="link"
                                onPress={() => {
                                  form.resetField(asset.id, { defaultValue: field.value })
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
                                field.onChange({ ...field.value, conclusion: 'skip' })
                              }}
                            >
                              {getText('skip')}
                            </Button>

                            {canReplace && (
                              <Button
                                variant="outline"
                                className="min-w-16"
                                onPress={() => {
                                  field.onChange({ ...field.value, conclusion: 'replace' })
                                }}
                              >
                                {getText('replace')}
                              </Button>
                            )}

                            <Popover.Trigger>
                              <Button variant="primary" className="min-w-16">
                                {getText('rename')}
                              </Button>

                              <Popover placement="bottom start">
                                <Form
                                  method="dialog"
                                  defaultValues={{
                                    newName: form.getValues(`${asset.id}.newName`),
                                  }}
                                  schema={(schema) =>
                                    schema.object({
                                      newName: backendModule.titleSchema({
                                        id: asset.id,
                                        siblings: siblingFiles.siblings,
                                      }),
                                    })
                                  }
                                  onSubmit={(value) => {
                                    field.onChange({
                                      ...field.value,
                                      conclusion: 'rename',
                                      newName: value.newName,
                                    })
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

          <Button.Group className="fixed bottom-0 left-0 right-0 border-t-0.5 border-primary/20 bg-background/90 px-3 py-4 backdrop-blur-md">
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

/** Options for resolving duplicates. */
export interface ResolveDuplicationsOptions
  extends Omit<ResolveDuplicationsProps, 'onCancel' | 'onSubmit'> {}

/** Function for resolving duplicates. */
// eslint-disable-next-line react-refresh/only-export-components
export async function resolveDuplications(options: ResolveDuplicationsOptions) {
  return new Promise<readonly ResolvedDuplication[]>((resolve, reject) => {
    setModal(<ResolveDuplicationsModal {...options} onSubmit={resolve} onCancel={reject} />)
  }).finally(unsetModal)
}
