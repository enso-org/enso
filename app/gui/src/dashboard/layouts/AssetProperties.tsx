/** @file Display and modify the properties of an asset. */
import * as React from 'react'

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
import { Result } from '#/components/Result'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import { validateDatalink } from '#/data/datalinkValidator'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSpotlight } from '#/hooks/spotlightHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { assetPanelStore, useSetAssetPanelProps } from '#/layouts/AssetPanel/'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useFeatureFlags } from '#/providers/FeatureFlagsProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { AssetType, BackendType, Plan, type AnyAsset, type DatalinkId } from '#/services/Backend'
import { extractTypeAndId } from '#/services/LocalBackend'
import { normalizePath } from '#/utilities/fileInfo'
import { mapNonNullish } from '#/utilities/nullable'
import * as permissions from '#/utilities/permissions'
import { tv } from '#/utilities/tailwindVariants'
import { useMutation } from '@tanstack/react-query'
import { useStore } from '../utilities/zustand'

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
  readonly category: Category
  readonly isReadonly?: boolean
}

/**
 * Display and modify the properties of an asset.
 */
export default function AssetProperties(props: AssetPropertiesProps) {
  const { isReadonly = false, backend, category } = props

  const { item, spotlightOn, path } = useStore(
    assetPanelStore,
    (state) => ({
      item: state.assetPanelProps.item,
      spotlightOn: state.assetPanelProps.spotlightOn ?? null,
      path: state.assetPanelProps.path,
    }),
    { unsafeEnableTransition: true },
  )

  const { getText } = useText()

  if (backend.type === BackendType.local) {
    return <Result status="info" centered title={getText('assetProperties.localBackend')} />
  }

  if (item == null || path == null) {
    return <Result status="info" title={getText('assetProperties.notSelected')} centered />
  }

  return (
    <AssetPropertiesInternal
      backend={backend}
      item={item}
      isReadonly={isReadonly}
      category={category}
      spotlightOn={spotlightOn}
      path={path}
    />
  )
}

/**
 * Props for {@link AssetPropertiesInternal}.
 */
export interface AssetPropertiesInternalProps extends AssetPropertiesProps {
  readonly item: AnyAsset
  readonly path: string | null
  readonly spotlightOn: AssetPropertiesSpotlight | null
}

/**
 * Internal implementation of {@link AssetProperties}.
 */
function AssetPropertiesInternal(props: AssetPropertiesInternalProps) {
  const { backend, item, category, spotlightOn, isReadonly = false, path: pathRaw } = props
  const styles = ASSET_PROPERTIES_VARIANTS({})

  const setAssetPanelProps = useSetAssetPanelProps()

  const closeSpotlight = useEventCallback(() => {
    const assetPanelProps = assetPanelStore.getState().assetPanelProps
    setAssetPanelProps({ ...assetPanelProps, spotlightOn: null })
  })
  const { user } = useFullUserSession()
  const isEnterprise = user.plan === Plan.enterprise
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const [isEditingDescriptionRaw, setIsEditingDescriptionRaw] = React.useState(false)
  const isEditingDescription = isEditingDescriptionRaw || spotlightOn === 'description'
  const setIsEditingDescription = useEventCallback(
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
  )
  const featureFlags = useFeatureFlags()
  const datalinkQuery = useBackendQuery(
    backend,
    'getDatalink',
    // eslint-disable-next-line no-restricted-syntax
    [item.id as DatalinkId, item.title],
    {
      enabled: item.type === AssetType.datalink,
      ...(featureFlags.enableAssetsTableBackgroundRefresh ?
        { refetchInterval: featureFlags.assetsTableBackgroundRefreshInterval }
      : {}),
    },
  )
  const descriptionSpotlight = useSpotlight({
    enabled: spotlightOn === 'description',
    close: closeSpotlight,
  })
  const secretSpotlight = useSpotlight({
    enabled: spotlightOn === 'secret',
    close: closeSpotlight,
  })
  const datalinkSpotlight = useSpotlight({
    enabled: spotlightOn === 'datalink',
    close: closeSpotlight,
  })

  const labels = useBackendQuery(backend, 'listTags', []).data ?? []
  const self = permissions.tryFindSelfPermission(user, item.permissions)
  const ownsThisAsset = self?.permission === permissions.PermissionAction.own
  const canEditThisAsset =
    ownsThisAsset ||
    self?.permission === permissions.PermissionAction.admin ||
    self?.permission === permissions.PermissionAction.edit
  const isSecret = item.type === AssetType.secret
  const isDatalink = item.type === AssetType.datalink
  const isCloud = backend.type === BackendType.remote
  const pathComputed =
    category.type === 'recent' || category.type === 'trash' ? null
    : isCloud ? `${pathRaw}${item.type === AssetType.datalink ? '.datalink' : ''}`
    : item.type === AssetType.project ?
      mapNonNullish(localBackend?.getProjectPath(item.id) ?? null, normalizePath)
    : normalizePath(extractTypeAndId(item.id).id)
  const path =
    pathComputed == null ? null
    : isCloud ? encodeURI(pathComputed)
    : pathComputed
  const createDatalinkMutation = useMutation(backendMutationOptions(backend, 'createDatalink'))
  // Provide an extra `mutationKey` so that it has its own loading state.
  const editDescriptionMutation = useMutation(
    backendMutationOptions(backend, 'updateAsset', { mutationKey: ['editDescription'] }),
  )
  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))
  const displayedDescription =
    editDescriptionMutation.variables?.[0] === item.id ?
      (editDescriptionMutation.variables[1].description ?? item.description)
    : item.description

  const editDescriptionForm = Form.useForm({
    schema: (z) => z.object({ description: z.string() }),
    defaultValues: { description: item.description ?? '' },
    onSubmit: async ({ description }) => {
      if (description !== item.description) {
        await editDescriptionMutation.mutateAsync([
          item.id,
          { parentDirectoryId: null, description },
          item.title,
        ])
      }
      setIsEditingDescription(false)
    },
  })
  const resetEditDescriptionForm = editDescriptionForm.reset

  React.useEffect(() => {
    setIsEditingDescription(false)
  }, [item.id, setIsEditingDescription])

  React.useEffect(() => {
    resetEditDescriptionForm({ description: item.description ?? '' })
  }, [item.description, resetEditDescriptionForm])

  const editDatalinkForm = Form.useForm({
    schema: (z) => z.object({ datalink: z.custom((x) => validateDatalink(x)) }),
    defaultValues: { datalink: datalinkQuery.data },
    onSubmit: async ({ datalink }) => {
      await createDatalinkMutation.mutateAsync([
        {
          // The UI to submit this form is only visible if the asset is a datalink.
          // eslint-disable-next-line no-restricted-syntax
          datalinkId: item.id as DatalinkId,
          name: item.title,
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
    <div className="flex w-full flex-col gap-8">
      {descriptionSpotlight.spotlightElement}
      {secretSpotlight.spotlightElement}
      {datalinkSpotlight.spotlightElement}
      <div className={styles.section()} {...descriptionSpotlight.props}>
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
                  {item.labels?.map((value) => {
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
        <div className={styles.section()} {...secretSpotlight.props}>
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
            id={item.id}
            name={item.title}
            doCreate={async (title, value) => {
              await updateSecretMutation.mutateAsync([item.id, { title, value }, title])
            }}
          />
        </div>
      )}

      {isDatalink && (
        <div className={styles.section()} {...datalinkSpotlight.props}>
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('datalink')}
          </Heading>
          {datalinkQuery.isLoading ?
            <div className="grid place-items-center self-stretch">
              <StatelessSpinner size={48} state="loading-medium" />
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
    </div>
  )
}
