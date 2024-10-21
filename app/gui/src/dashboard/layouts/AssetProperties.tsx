/** @file Display and modify the properties of an asset. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import PenIcon from '#/assets/pen.svg'
import { Heading } from '#/components/aria'
import {
  Button,
  ButtonGroup,
  CopyButton,
  Form,
  ResizableContentEditableInput,
  Text,
} from '#/components/AriaComponents'
import SharedWithColumn from '#/components/dashboard/column/SharedWithColumn'
import { DatalinkFormInput } from '#/components/dashboard/DatalinkInput'
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
import { useSyncRef } from '#/hooks/syncRefHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useDriveStore, useSetAssetPanelProps } from '#/providers/DriveProvider'
import { useFeatureFlags } from '#/providers/FeatureFlagsProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { AssetType, BackendType, Plan, type DatalinkId } from '#/services/Backend'
import { extractTypeAndId } from '#/services/LocalBackend'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import { normalizePath } from '#/utilities/fileInfo'
import { mapNonNullish } from '#/utilities/nullable'
import * as permissions from '#/utilities/permissions'
import { tv } from '#/utilities/tailwindVariants'

// =======================
// === AssetProperties ===
// =======================

const ASSET_PROPERTIES_VARIANTS = tv({
  base: '',
  slots: {
    section: 'pointer-events-auto flex flex-col items-start gap-side-panel-section rounded-default',
  },
})

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
  const { backend, item, category, spotlightOn, isReadonly = false } = props
  const styles = ASSET_PROPERTIES_VARIANTS({})

  const asset = useAssetPassiveListenerStrict(
    backend.type,
    item.item.id,
    item.item.parentId,
    category,
  )
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
  const isEnterprise = user.plan === Plan.enterprise
  const { getText } = useText()
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
  const featureFlags = useFeatureFlags()
  const datalinkQuery = useBackendQuery(
    backend,
    'getDatalink',
    // eslint-disable-next-line no-restricted-syntax
    [asset.id as DatalinkId, asset.title],
    {
      enabled: asset.type === AssetType.datalink,
      ...(featureFlags.enableAssetsTableBackgroundRefresh ?
        { refetchInterval: featureFlags.assetsTableBackgroundRefreshInterval }
      : {}),
    },
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
  const editDescriptionMutation = useMutation(
    // Provide an extra `mutationKey` so that it has its own loading state.
    backendMutationOptions(backend, 'updateAsset', { mutationKey: ['editDescription'] }),
  )
  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))
  const displayedDescription =
    editDescriptionMutation.variables?.[1].description ?? asset.description

  const editDescriptionForm = Form.useForm({
    schema: (z) => z.object({ description: z.string() }),
    defaultValues: { description: asset.description ?? '' },
    onSubmit: async ({ description }) => {
      if (description !== asset.description) {
        await editDescriptionMutation.mutateAsync([
          asset.id,
          { parentDirectoryId: null, description },
          asset.title,
        ])
      }
      setIsEditingDescription(false)
    },
  })

  const editDatalinkForm = Form.useForm({
    schema: (z) => z.object({ datalink: z.custom((x) => validateDatalink(x)) }),
    defaultValues: { datalink: datalinkQuery.data },
    onSubmit: async ({ datalink }) => {
      await createDatalinkMutation.mutateAsync([
        {
          // The UI to submit this form is only visible if the asset is a datalink.
          // eslint-disable-next-line no-restricted-syntax
          datalinkId: asset.id as DatalinkId,
          name: asset.title,
          parentDirectoryId: null,
          value: datalink,
        },
      ])
    },
  })

  const editDatalinkFormRef = useSyncRef(editDatalinkForm)
  React.useEffect(() => {
    editDatalinkFormRef.current.setValue('datalink', datalinkQuery.data)
  }, [datalinkQuery.data, editDatalinkFormRef])

  return (
    <>
      {descriptionSpotlight.spotlightElement}
      {secretSpotlight.spotlightElement}
      {datalinkSpotlight.spotlightElement}
      <div ref={descriptionRef} className={styles.section()} {...descriptionSpotlight.props}>
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
              loading={editDescriptionMutation.isPending}
              onPress={() => {
                setIsEditingDescription(true)
              }}
            />
          )}
        </Heading>
        <div
          data-testid="asset-panel-description"
          className="self-stretch py-side-panel-description-y"
        >
          {!isEditingDescription ?
            <Text>{displayedDescription}</Text>
          : <Form form={editDescriptionForm} className="flex flex-col gap-modal pr-4">
              <ResizableContentEditableInput
                autoFocus
                form={editDescriptionForm}
                name="description"
                mode="onBlur"
              />
              <ButtonGroup>
                <Form.Submit>{getText('update')}</Form.Submit>
              </ButtonGroup>
            </Form>
          }
        </div>
      </div>
      {isCloud && (
        <div className={styles.section()}>
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
              {isEnterprise && (
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
              )}
              <tr data-testid="asset-panel-labels" className="h-row">
                <td className="text my-auto min-w-side-panel-label p-0">
                  <Text className="text inline-block">{getText('labels')}</Text>
                </td>
                <td className="flex w-full gap-1 p-0">
                  {asset.labels?.map((value) => {
                    const label = labels.find((otherLabel) => otherLabel.value === value)
                    return (
                      label != null && (
                        <Label key={value} active isDisabled color={label.color} onPress={() => {}}>
                          {value}
                        </Label>
                      )
                    )
                  })}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      )}

      {isSecret && (
        <div ref={secretRef} className={styles.section()} {...secretSpotlight.props}>
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
        <div ref={datalinkRef} className={styles.section()} {...datalinkSpotlight.props}>
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('datalink')}
          </Heading>
          {datalinkQuery.isLoading ?
            <div className="grid place-items-center self-stretch">
              <StatelessSpinner size={48} state={statelessSpinner.SpinnerState.loadingMedium} />
            </div>
          : <Form form={editDatalinkForm} className="w-full">
              <DatalinkFormInput
                form={editDatalinkForm}
                name="datalink"
                readOnly={!canEditThisAsset}
                dropdownTitle={getText('type')}
              />
              {canEditThisAsset && (
                <ButtonGroup>
                  <Form.Submit>{getText('update')}</Form.Submit>
                  <Form.Reset
                    onPress={() => {
                      editDatalinkForm.reset({ datalink: datalinkQuery.data })
                    }}
                  />
                </ButtonGroup>
              )}
            </Form>
          }
        </div>
      )}
    </>
  )
}
