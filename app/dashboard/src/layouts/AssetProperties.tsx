/** @file Display and modify the properties of an asset. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import PenIcon from '#/assets/pen.svg'
import { Heading } from '#/components/aria'
import { Button, ButtonGroup, CopyButton, Text } from '#/components/AriaComponents'
import SharedWithColumn from '#/components/dashboard/column/SharedWithColumn'
import DatalinkInput from '#/components/dashboard/DatalinkInput'
import Label from '#/components/dashboard/Label'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import { validateDatalink } from '#/data/datalinkValidator'
import {
  backendMutationOptions,
  useAssetPassiveListenerStrict,
  useBackendQuery,
} from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSpotlight } from '#/hooks/spotlightHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useDriveStore, useSetAssetPanelProps } from '#/providers/DriveProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { AssetType, BackendType } from '#/services/Backend'
import { extractTypeAndId } from '#/services/LocalBackend'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import { normalizePath } from '#/utilities/fileInfo'
import { mapNonNullish } from '#/utilities/nullable'
import * as permissions from '#/utilities/permissions'

// =======================
// === AssetProperties ===
// =======================

/** Possible elements in this screen to spotlight on. */
export type AssetPropertiesSpotlight = 'datalink' | 'description' | 'secret'

/** Props for an {@link AssetPropertiesProps}. */
export interface AssetPropertiesProps {
  readonly backend: Backend
  readonly item: AnyAssetTreeNode
  readonly category: Category
  readonly isReadonly?: boolean
  readonly spotlightOn: AssetPropertiesSpotlight | undefined
}

/** Display and modify the properties of an asset. */
export default function AssetProperties(props: AssetPropertiesProps) {
  const { backend, item, category, spotlightOn } = props
  const { isReadonly = false } = props

  const asset = useAssetPassiveListenerStrict(backend.type, item.item.id, item.item.parentId)
  const setAssetPanelProps = useSetAssetPanelProps()
  const closeSpotlight = useEventCallback(() => {
    const assetPanelProps = driveStore.getState().assetPanelProps
    if (assetPanelProps != null) {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const { spotlightOn: unusedSpotlightOn, ...rest } = assetPanelProps
      setAssetPanelProps(rest)
    }
  })
  const { user } = useFullUserSession()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const localBackend = useLocalBackend()
  const [isEditingDescriptionRaw, setIsEditingDescriptionRaw] = React.useState(false)
  const isEditingDescription = isEditingDescriptionRaw || spotlightOn === 'description'
  const setIsEditingDescription = React.useCallback(
    (valueOrUpdater: React.SetStateAction<boolean>) => {
      setIsEditingDescriptionRaw((currentValue) => {
        if (typeof valueOrUpdater === 'function') {
          valueOrUpdater = valueOrUpdater(currentValue)
        }
        if (!valueOrUpdater) {
          closeSpotlight()
        }
        return valueOrUpdater
      })
    },
    [closeSpotlight],
  )
  const [queuedDescription, setQueuedDescripion] = React.useState<string | null>(null)
  const [description, setDescription] = React.useState('')
  const [datalinkValue, setDatalinkValue] = React.useState<NonNullable<unknown> | null>(null)
  const [editedDatalinkValue, setEditedDatalinkValue] = React.useState<NonNullable<unknown> | null>(
    datalinkValue,
  )
  const [isDatalinkFetched, setIsDatalinkFetched] = React.useState(false)
  const isDatalinkSubmittable = React.useMemo(
    () => validateDatalink(datalinkValue),
    [datalinkValue],
  )
  const driveStore = useDriveStore()
  const descriptionRef = React.useRef<HTMLDivElement>(null)
  const descriptionSpotlight = useSpotlight({
    ref: descriptionRef,
    enabled: spotlightOn === 'description',
    close: closeSpotlight,
  })
  const secretRef = React.useRef<HTMLDivElement>(null)
  const secretSpotlight = useSpotlight({
    ref: secretRef,
    enabled: spotlightOn === 'secret',
    close: closeSpotlight,
  })
  const datalinkRef = React.useRef<HTMLDivElement>(null)
  const datalinkSpotlight = useSpotlight({
    ref: datalinkRef,
    enabled: spotlightOn === 'datalink',
    close: closeSpotlight,
  })

  const labels = useBackendQuery(backend, 'listTags', []).data ?? []
  const self = permissions.tryFindSelfPermission(user, asset.permissions)
  const ownsThisAsset = self?.permission === permissions.PermissionAction.own
  const canEditThisAsset =
    ownsThisAsset ||
    self?.permission === permissions.PermissionAction.admin ||
    self?.permission === permissions.PermissionAction.edit
  const isSecret = asset.type === AssetType.secret
  const isDatalink = asset.type === AssetType.datalink
  const isDatalinkDisabled = datalinkValue === editedDatalinkValue || !isDatalinkSubmittable
  const isCloud = backend.type === BackendType.remote
  const pathRaw =
    category.type === 'recent' || category.type === 'trash' ? null
    : isCloud ? `${item.path}${item.type === AssetType.datalink ? '.datalink' : ''}`
    : asset.type === AssetType.project ?
      mapNonNullish(localBackend?.getProjectPath(asset.id) ?? null, normalizePath)
    : normalizePath(extractTypeAndId(asset.id).id)
  const path =
    pathRaw == null ? null
    : isCloud ? encodeURI(pathRaw)
    : pathRaw
  const createDatalinkMutation = useMutation(backendMutationOptions(backend, 'createDatalink'))
  const getDatalinkMutation = useMutation(backendMutationOptions(backend, 'getDatalink'))
  const updateAssetMutation = useMutation(backendMutationOptions(backend, 'updateAsset'))
  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))
  const getDatalink = getDatalinkMutation.mutateAsync

  React.useEffect(() => {
    setDescription(asset.description ?? '')
  }, [asset.description])

  React.useEffect(() => {
    void (async () => {
      if (asset.type === AssetType.datalink) {
        const value = await getDatalink([asset.id, asset.title])
        setDatalinkValue(value)
        setIsDatalinkFetched(true)
      }
    })()
  }, [backend, asset, getDatalink])

  const editDescription = async () => {
    setIsEditingDescription(false)
    if (description !== asset.description) {
      await updateAssetMutation.mutateAsync([
        asset.id,
        { parentDirectoryId: null, description },
        asset.title,
      ])
    }
  }

  return (
    <>
      {descriptionSpotlight.spotlightElement}
      {secretSpotlight.spotlightElement}
      {datalinkSpotlight.spotlightElement}
      <div
        ref={descriptionRef}
        className="pointer-events-auto flex flex-col items-start gap-side-panel rounded-default"
        {...descriptionSpotlight.props}
      >
        <Heading
          level={2}
          className="flex h-side-panel-heading items-center gap-side-panel-section py-side-panel-heading-y text-lg leading-snug"
        >
          {getText('description')}
          {!isReadonly && ownsThisAsset && !isEditingDescription && (
            <Button
              size="medium"
              variant="icon"
              icon={PenIcon}
              onPress={() => {
                setIsEditingDescription(true)
                setQueuedDescripion(asset.description)
              }}
            />
          )}
        </Heading>
        <div
          data-testid="asset-panel-description"
          className="self-stretch py-side-panel-description-y"
        >
          {!isEditingDescription ?
            <Text>{asset.description}</Text>
          : <form className="flex flex-col gap-modal pr-4" onSubmit={editDescription}>
              <textarea
                ref={(element) => {
                  if (element != null && queuedDescription != null) {
                    element.value = queuedDescription
                    setQueuedDescripion(null)
                  }
                }}
                autoFocus
                value={description}
                className="w-full resize-none rounded-default border-0.5 border-primary/20 p-2 outline-2 outline-offset-2 transition-[border-color,outline] focus-within:outline focus-within:outline-offset-0"
                onChange={(event) => {
                  setDescription(event.currentTarget.value)
                }}
                onKeyDown={(event) => {
                  event.stopPropagation()
                  switch (event.key) {
                    case 'Escape': {
                      setIsEditingDescription(false)
                      break
                    }
                    case 'Enter': {
                      if (event.ctrlKey) {
                        void editDescription()
                        break
                      }
                    }
                  }
                }}
              />
              <ButtonGroup>
                <Button size="medium" variant="outline" onPress={editDescription}>
                  {getText('update')}
                </Button>
              </ButtonGroup>
            </form>
          }
        </div>
      </div>
      {isCloud && (
        <div className="pointer-events-auto flex flex-col items-start gap-side-panel-section">
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('settings')}
          </Heading>
          <table>
            <tbody>
              {path != null && (
                <tr data-testid="asset-panel-permissions" className="h-row">
                  <td className="text my-auto min-w-side-panel-label p-0">
                    <Text>{getText('path')}</Text>
                  </td>
                  <td className="w-full p-0">
                    <div className="flex items-center gap-2">
                      <Text className="w-0 grow" truncate="1">
                        {decodeURI(path)}
                      </Text>
                      <CopyButton copyText={path} />
                    </div>
                  </td>
                </tr>
              )}
              <tr data-testid="asset-panel-permissions" className="h-row">
                <td className="text my-auto min-w-side-panel-label p-0">
                  <Text className="text inline-block">{getText('sharedWith')}</Text>
                </td>
                <td className="flex w-full gap-1 p-0">
                  <SharedWithColumn
                    isReadonly={isReadonly}
                    item={item}
                    state={{ backend, category, setQuery: () => {} }}
                  />
                </td>
              </tr>
              <tr data-testid="asset-panel-labels" className="h-row">
                <td className="text my-auto min-w-side-panel-label p-0">
                  <Text className="text inline-block">{getText('labels')}</Text>
                </td>
                <td className="flex w-full gap-1 p-0">
                  {asset.labels?.map((value) => {
                    const label = labels.find((otherLabel) => otherLabel.value === value)
                    return label == null ? null : (
                        <Label key={value} active isDisabled color={label.color} onPress={() => {}}>
                          {value}
                        </Label>
                      )
                  })}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      )}

      {isSecret && (
        <div
          ref={secretRef}
          className="pointer-events-auto flex flex-col items-start gap-side-panel-section rounded-default"
          {...secretSpotlight.props}
        >
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('secret')}
          </Heading>
          <UpsertSecretModal
            noDialog
            canReset
            canCancel={false}
            id={asset.id}
            name={asset.title}
            doCreate={async (name, value) => {
              await updateSecretMutation.mutateAsync([asset.id, { value }, name])
            }}
          />
        </div>
      )}

      {isDatalink && (
        <div
          ref={datalinkRef}
          className="pointer-events-auto flex flex-col items-start gap-side-panel-section rounded-default"
          {...datalinkSpotlight.props}
        >
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('datalink')}
          </Heading>
          {!isDatalinkFetched ?
            <div className="grid place-items-center self-stretch">
              <StatelessSpinner size={48} state={statelessSpinner.SpinnerState.loadingMedium} />
            </div>
          : <>
              <DatalinkInput
                readOnly={!canEditThisAsset}
                dropdownTitle="Type"
                value={editedDatalinkValue}
                onChange={setEditedDatalinkValue}
              />
              {canEditThisAsset && (
                <ButtonGroup>
                  <Button
                    variant="submit"
                    isDisabled={isDatalinkDisabled}
                    {...(isDatalinkDisabled ?
                      { title: 'Edit the Datalink before updating it.' }
                    : {})}
                    onPress={async () => {
                      const oldDatalinkValue = datalinkValue
                      try {
                        setDatalinkValue(editedDatalinkValue)
                        await createDatalinkMutation.mutateAsync([
                          {
                            datalinkId: asset.id,
                            name: asset.title,
                            parentDirectoryId: null,
                            value: editedDatalinkValue,
                          },
                        ])
                      } catch (error) {
                        toastAndLog(null, error)
                        setDatalinkValue(oldDatalinkValue)
                        setEditedDatalinkValue(oldDatalinkValue)
                      }
                    }}
                  >
                    {getText('update')}
                  </Button>
                  <Button
                    variant="outline"
                    isDisabled={isDatalinkDisabled}
                    onPress={() => {
                      setEditedDatalinkValue(datalinkValue)
                    }}
                  >
                    {getText('cancel')}
                  </Button>
                </ButtonGroup>
              )}
            </>
          }
        </div>
      )}
    </>
  )
}
