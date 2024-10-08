/** @file Display and modify the properties of an asset. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import PenIcon from '#/assets/pen.svg'

import * as datalinkValidator from '#/data/datalinkValidator'

import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import SharedWithColumn from '#/components/dashboard/column/SharedWithColumn'
import { DatalinkFormInput } from '#/components/dashboard/DatalinkInput'
import Label from '#/components/dashboard/Label'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSpotlight } from '#/hooks/spotlightHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useDriveStore, useSetAssetPanelProps } from '#/providers/DriveProvider'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import { normalizePath } from '#/utilities/fileInfo'
import { mapNonNullish } from '#/utilities/nullable'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'

// =======================
// === AssetProperties ===
// =======================

/** Possible elements in this screen to spotlight on. */
export type AssetPropertiesSpotlight = 'datalink' | 'description' | 'secret'

/** Props for an {@link AssetPropertiesProps}. */
export interface AssetPropertiesProps {
  readonly backend: Backend
  readonly item: assetTreeNode.AnyAssetTreeNode
  readonly setItem: React.Dispatch<React.SetStateAction<assetTreeNode.AnyAssetTreeNode>>
  readonly category: Category
  readonly isReadonly?: boolean
  readonly spotlightOn: AssetPropertiesSpotlight | undefined
}

/** Display and modify the properties of an asset. */
export default function AssetProperties(props: AssetPropertiesProps) {
  const { backend, item, setItem, category, spotlightOn } = props
  const { isReadonly = false } = props

  const setAssetPanelProps = useSetAssetPanelProps()
  const closeSpotlight = useEventCallback(() => {
    const assetPanelProps = driveStore.getState().assetPanelProps
    if (assetPanelProps != null) {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const { spotlightOn: unusedSpotlightOn, ...rest } = assetPanelProps
      setAssetPanelProps(rest)
    }
  })
  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const localBackend = backendProvider.useLocalBackend()
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
  const [isDatalinkFetched, setIsDatalinkFetched] = React.useState(false)
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
  const self = permissions.tryFindSelfPermission(user, item.item.permissions)
  const ownsThisAsset = self?.permission === permissions.PermissionAction.own
  const canEditThisAsset =
    ownsThisAsset ||
    self?.permission === permissions.PermissionAction.admin ||
    self?.permission === permissions.PermissionAction.edit
  const isSecret = item.type === backendModule.AssetType.secret
  const isDatalink = item.type === backendModule.AssetType.datalink
  const isCloud = backend.type === backendModule.BackendType.remote
  const pathRaw =
    category.type === 'recent' || category.type === 'trash' ? null
    : isCloud ? `${item.path}${item.type === backendModule.AssetType.datalink ? '.datalink' : ''}`
    : item.item.type === backendModule.AssetType.project ?
      mapNonNullish(localBackend?.getProjectPath(item.item.id) ?? null, normalizePath)
    : normalizePath(localBackendModule.extractTypeAndId(item.item.id).id)
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
    setDescription(item.item.description ?? '')
  }, [item.item.description])

  React.useEffect(() => {
    void (async () => {
      if (item.item.type === backendModule.AssetType.datalink) {
        const value = await getDatalink([item.item.id, item.item.title])
        setDatalinkValue(value)
        setIsDatalinkFetched(true)
      }
    })()
  }, [backend, item.item, getDatalink])

  const doEditDescription = async () => {
    setIsEditingDescription(false)
    if (description !== item.item.description) {
      const oldDescription = item.item.description
      setItem((oldItem) => oldItem.with({ item: object.merge(oldItem.item, { description }) }))
      try {
        await updateAssetMutation.mutateAsync([
          item.item.id,
          { parentDirectoryId: null, description },
          item.item.title,
        ])
      } catch (error) {
        toastAndLog('editDescriptionError')
        setItem((oldItem) =>
          oldItem.with({ item: object.merge(oldItem.item, { description: oldDescription }) }),
        )
      }
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
        <aria.Heading
          level={2}
          className="flex h-side-panel-heading items-center gap-side-panel-section py-side-panel-heading-y text-lg leading-snug"
        >
          {getText('description')}
          {!isReadonly && ownsThisAsset && !isEditingDescription && (
            <ariaComponents.Button
              size="medium"
              variant="icon"
              icon={PenIcon}
              onPress={() => {
                setIsEditingDescription(true)
                setQueuedDescripion(item.item.description)
              }}
            />
          )}
        </aria.Heading>
        <div
          data-testid="asset-panel-description"
          className="self-stretch py-side-panel-description-y"
        >
          {!isEditingDescription ?
            <aria.Text className="text">{item.item.description}</aria.Text>
          : <form className="flex flex-col gap-modal pr-4" onSubmit={doEditDescription}>
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
                        void doEditDescription()
                        break
                      }
                    }
                  }
                }}
              />
              <ariaComponents.ButtonGroup>
                <ariaComponents.Button size="medium" variant="outline" onPress={doEditDescription}>
                  {getText('update')}
                </ariaComponents.Button>
              </ariaComponents.ButtonGroup>
            </form>
          }
        </div>
      </div>
      {isCloud && (
        <div className="pointer-events-auto flex flex-col items-start gap-side-panel-section">
          <aria.Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('settings')}
          </aria.Heading>
          <table>
            <tbody>
              {path != null && (
                <tr data-testid="asset-panel-permissions" className="h-row">
                  <td className="text my-auto min-w-side-panel-label p-0">
                    <aria.Label className="text inline-block">{getText('path')}</aria.Label>
                  </td>
                  <td className="w-full p-0">
                    <div className="flex items-center gap-2">
                      <ariaComponents.Text className="w-0 grow" truncate="1">
                        {decodeURI(path)}
                      </ariaComponents.Text>
                      <ariaComponents.CopyButton copyText={path} />
                    </div>
                  </td>
                </tr>
              )}
              <tr data-testid="asset-panel-permissions" className="h-row">
                <td className="text my-auto min-w-side-panel-label p-0">
                  <aria.Label className="text inline-block">{getText('sharedWith')}</aria.Label>
                </td>
                <td className="flex w-full gap-1 p-0">
                  <SharedWithColumn
                    isReadonly={isReadonly}
                    item={item}
                    setItem={setItem}
                    state={{ category, setQuery: () => {} }}
                  />
                </td>
              </tr>
              <tr data-testid="asset-panel-labels" className="h-row">
                <td className="text my-auto min-w-side-panel-label p-0">
                  <aria.Label className="text inline-block">{getText('labels')}</aria.Label>
                </td>
                <td className="flex w-full gap-1 p-0">
                  {item.item.labels?.map((value) => {
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
          <aria.Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('secret')}
          </aria.Heading>
          <UpsertSecretModal
            noDialog
            canReset
            canCancel={false}
            id={item.item.id}
            name={item.item.title}
            doCreate={async (name, value) => {
              await updateSecretMutation.mutateAsync([item.item.id, { value }, name])
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
          <aria.Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('datalink')}
          </aria.Heading>
          {!isDatalinkFetched ?
            <div className="grid place-items-center self-stretch">
              <StatelessSpinner size={48} state={statelessSpinner.SpinnerState.loadingMedium} />
            </div>
          : <>
              <ariaComponents.Form
                schema={(z) =>
                  z.object({
                    value: z.unknown().refine(datalinkValidator.validateDatalink),
                  })
                }
                defaultValues={{ value: datalinkValue }}
                className="w-full"
                onSubmit={async ({ value }) => {
                  await createDatalinkMutation.mutateAsync([
                    {
                      datalinkId: item.item.id,
                      name: item.item.title,
                      parentDirectoryId: null,
                      value: value,
                    },
                  ])
                }}
              >
                {({ form }) => (
                  <>
                    <DatalinkFormInput
                      form={form}
                      name="value"
                      readOnly={!canEditThisAsset}
                      dropdownTitle={getText('type')}
                    />
                    {canEditThisAsset && (
                      <ariaComponents.ButtonGroup>
                        <ariaComponents.Form.Submit action="update" />
                        <ariaComponents.Form.Reset action="cancel" />
                      </ariaComponents.ButtonGroup>
                    )}
                  </>
                )}
              </ariaComponents.Form>
            </>
          }
        </div>
      )}
    </>
  )
}
