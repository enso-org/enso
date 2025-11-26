/** @file Display and modify the properties of an asset. */
import { Heading } from '#/components/aria'
import { Button, CopyButton } from '#/components/Button'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Form } from '#/components/Form'
import { Result } from '#/components/Result'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import { Text } from '#/components/Text'
import { validateDatalink } from '#/data/datalinkValidator'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSpotlight } from '#/hooks/spotlightHooks'
import type { Category } from '#/layouts/Drive/Categories'
import { UpsertSecretForm } from '#/modals/UpsertSecretModal'
import { SharedWithColumn } from '#/pages/dashboard/components/column'
import { DatalinkFormInput } from '#/pages/dashboard/components/DatalinkInput'
import Label from '#/pages/dashboard/components/Label'
import { tv } from '#/utilities/tailwindVariants'
import { useBackends, useFullUserSession, useText } from '$/providers/react'
import { useVueValue } from '$/providers/react/common'
import {
  useRightPanelContextCategory,
  useRightPanelData,
  useRightPanelFocusedAsset,
} from '$/providers/react/container'
import { useFeatureFlags } from '$/providers/react/featureFlags'
import { useMutation, useQuery } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import {
  AssetType,
  BackendType,
  getAssetPermissionId,
  getAssetPermissionName,
  isAssetCredential,
  Plan,
  type AnyAsset,
  type DatalinkId,
} from 'enso-common/src/services/Backend'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import * as permissions from 'enso-common/src/utilities/permissions'
import * as React from 'react'

const ASSET_PROPERTIES_VARIANTS = tv({
  base: '',
  slots: {
    section: 'pointer-events-auto flex flex-col items-start gap-side-panel-section rounded-default',
  },
})

/** Display and modify the properties of an asset. */
export function AssetProperties() {
  const { remoteBackend } = useBackends()
  const focusedAsset = useRightPanelFocusedAsset()
  const category = useRightPanelContextCategory()
  const { getText } = useText()

  if (category?.backend !== BackendType.remote) {
    return <Result status="info" centered title={getText('assetProperties.localBackend')} />
  }

  if (focusedAsset == null) {
    return <Result status="info" title={getText('assetProperties.notSelected')} centered />
  }

  return (
    <ErrorBoundary>
      <AssetPropertiesInternal
        key={focusedAsset.id}
        backend={remoteBackend}
        item={focusedAsset}
        category={category}
      />
    </ErrorBoundary>
  )
}

/** Props for an {@link AssetPropertiesInternal}. */
export interface AssetPropertiesInternalProps {
  readonly backend: Backend
  readonly category: Category
  readonly item: AnyAsset
}

/** Display and modify the properties of an asset. */
function AssetPropertiesInternal(props: AssetPropertiesInternalProps) {
  const { backend, item, category } = props
  const styles = ASSET_PROPERTIES_VARIANTS({})
  const rightPanel = useRightPanelData()
  const spotlightOn = useVueValue(
    React.useCallback(() => rightPanel.context?.spotlightOn, [rightPanel]),
  )

  const closeSpotlight = useEventCallback(() => {
    rightPanel.updateContext('drive', (ctx) => {
      ctx.spotlightOn = undefined
      return ctx
    })
  })
  const { user } = useFullUserSession()
  const isEnterprise = user.plan === Plan.enterprise
  const { getText } = useText()
  const featureFlags = useFeatureFlags()
  const datalinkQuery = useQuery(
    backendQueryOptions(
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
    ),
  )
  const secretSpotlight = useSpotlight({
    enabled: spotlightOn === 'secret',
    close: closeSpotlight,
  })
  const datalinkSpotlight = useSpotlight({
    enabled: spotlightOn === 'datalink',
    close: closeSpotlight,
  })

  const { data: labels = [] } = useQuery(backendQueryOptions(backend, 'listTags', []))
  const self = permissions.tryFindSelfPermission(user, item.permissions)
  const ownsThisAsset = self?.permission === permissions.PermissionAction.own
  const canEditThisAsset =
    ownsThisAsset ||
    self?.permission === permissions.PermissionAction.admin ||
    self?.permission === permissions.PermissionAction.edit
  const isSecret = item.type === AssetType.secret
  const isCredential = isAssetCredential(item)
  const isDatalink = item.type === AssetType.datalink
  const isCloud = backend.type === BackendType.remote
  // Provide an extra `mutationKey` so that it has its own loading state.
  const createDatalinkMutation = useMutation(backendMutationOptions(backend, 'createDatalink'))
  const updateSecretMutation = useMutation(backendMutationOptions(backend, 'updateSecret'))
  const ownerPermission = permissions.tryGetOwnerPermission(item)

  return (
    <div className="flex w-full flex-col gap-8">
      {secretSpotlight.spotlightElement}
      {datalinkSpotlight.spotlightElement}

      {isCloud && (
        <div className={styles.section()}>
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('properties')}
          </Heading>
          <table>
            <tbody>
              <tr data-testid="asset-panel-path" className="h-row">
                <td className="my-auto min-w-side-panel-label p-0">
                  <Text>{getText('path')}</Text>
                </td>
                <td className="w-full p-0">
                  <div className="flex items-center gap-2">
                    <Text className="w-0 grow" truncate="1">
                      {item.ensoPath}
                    </Text>
                    <CopyButton copyText={encodeURI(item.ensoPath)} />
                  </div>
                </td>
              </tr>
              {featureFlags.showDeveloperIds && (
                <tr className="h-row">
                  <td className="my-auto min-w-side-panel-label p-0">
                    <Text color="accent">{getText('assetId')}</Text>
                  </td>
                  <td className="w-full p-0">
                    <div className="flex items-center gap-2">
                      <Text color="accent" className="w-0 grow" truncate="1">
                        {item.id}
                      </Text>
                      <CopyButton copyText={item.id} />
                    </div>
                  </td>
                </tr>
              )}
              {featureFlags.showDeveloperIds && (
                <tr className="h-row">
                  <td className="my-auto min-w-side-panel-label p-0">
                    <Text color="accent">{getText('parentId')}</Text>
                  </td>
                  <td className="w-full p-0">
                    <div className="flex items-center gap-2">
                      <Text color="accent" className="w-0 grow" truncate="1">
                        {item.parentId}
                      </Text>
                      <CopyButton copyText={item.parentId} />
                    </div>
                  </td>
                </tr>
              )}
              {ownerPermission && (
                <tr data-testid="asset-panel-owner" className="h-row">
                  <td className="min-w-side-panel-label p-0">
                    <Text className="inline-block">{getText('owner')}</Text>
                  </td>
                  <td className="w-full p-0">
                    <div className="flex items-center gap-2">
                      <Text className="w-0 grow" truncate="1">
                        {getAssetPermissionName(ownerPermission)}
                      </Text>
                    </div>
                  </td>
                </tr>
              )}
              {featureFlags.showDeveloperIds && ownerPermission && (
                <tr className="h-row">
                  <td className="my-auto min-w-side-panel-label p-0">
                    <Text color="accent">{getText('ownerId')}</Text>
                  </td>
                  <td className="w-full p-0">
                    <div className="flex items-center gap-2">
                      <Text color="accent" className="w-0 grow" truncate="1">
                        {getAssetPermissionId(ownerPermission)}
                      </Text>
                      <CopyButton copyText={getAssetPermissionId(ownerPermission)} />
                    </div>
                  </td>
                </tr>
              )}
              <tr data-testid="asset-panel-modified-at" className="h-row">
                <td className="min-w-side-panel-label p-0">
                  <Text className="inline-block">{getText('modifiedAt')}</Text>
                </td>
                <td className="w-full p-0">
                  <Text className="grow" truncate="1">
                    {toReadableIsoString(new Date(item.modifiedAt))}
                  </Text>
                </td>
              </tr>
              {isEnterprise && (
                <tr data-testid="asset-panel-permissions" className="h-row">
                  <td className="my-auto min-w-side-panel-label p-0">
                    <Text className="inline-block">{getText('sharedWith')}</Text>
                  </td>
                  <td className="flex w-full gap-1 p-0">
                    <SharedWithColumn item={item} state={{ category }} />
                  </td>
                </tr>
              )}
              <tr data-testid="asset-panel-labels" className="h-row">
                <td className="my-auto min-w-side-panel-label p-0">
                  <Text className="inline-block">{getText('labels')}</Text>
                </td>
                <td className="flex w-full gap-1 p-0">
                  {item.labels?.map((value) => {
                    const label = labels.find((otherLabel) => otherLabel.value === value)
                    if (!label) {
                      return null
                    }
                    return (
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

      {isSecret && !isCredential && (
        <div className={styles.section()} {...secretSpotlight.props}>
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('configuration')}
          </Heading>
          <UpsertSecretForm
            key={item.id}
            doCancel="reset"
            secretId={item.id}
            name={item.title}
            doCreate={(title, value) =>
              updateSecretMutation.mutateAsync([item.id, { title, value }, title])
            }
          />
        </div>
      )}

      {isSecret && isCredential && (
        <div className={styles.section()} {...secretSpotlight.props}>
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('configuration')}
          </Heading>
          <table>
            <tbody>
              <tr className="h-row">
                <td className="my-auto min-w-side-panel-label p-0">
                  <Text>{getText('credentialServiceName')}</Text>
                </td>
                <td className="w-full p-0">
                  <div className="flex items-center gap-2">
                    <Text className="w-0 grow" truncate="1">
                      {item.credentialMetadata.serviceName}
                    </Text>
                  </div>
                </td>
              </tr>
              <tr className="h-row">
                <td className="my-auto min-w-side-panel-label p-0">
                  <Text>{getText('credentialState')}</Text>
                </td>
                <td className="w-full p-0">
                  <div className="flex items-center gap-2">
                    <Text className="w-0 grow" truncate="1">
                      {getText(`credentialState${item.credentialMetadata.state}`)}
                    </Text>
                  </div>
                </td>
              </tr>
              {item.credentialMetadata.expirationDate && (
                <tr className="h-row">
                  <td className="my-auto min-w-side-panel-label p-0">
                    <Text>{getText('credentialExpiresAt')}</Text>
                  </td>
                  <td className="w-full p-0">
                    <div className="flex items-center gap-2">
                      <Text className="w-0 grow" truncate="1">
                        {toReadableIsoString(new Date(item.credentialMetadata.expirationDate))}
                      </Text>
                    </div>
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      )}

      {isDatalink && (
        <div className={styles.section()} {...datalinkSpotlight.props}>
          <Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('configuration')}
          </Heading>
          {datalinkQuery.isLoading ?
            <div className="grid place-items-center self-stretch">
              <StatelessSpinner size={48} phase="loading-medium" />
            </div>
          : <Form
              schema={(z) => z.object({ datalink: z.custom((x) => validateDatalink(x)) })}
              defaultValues={{ datalink: datalinkQuery.data }}
              onSubmit={({ datalink }) =>
                createDatalinkMutation.mutateAsync([
                  {
                    datalinkId: item.id,
                    name: item.title,
                    parentDirectoryId: item.parentId,
                    value: datalink,
                  },
                ])
              }
              className="w-full bg-white"
            >
              {(form) => (
                <>
                  <DatalinkFormInput
                    name="datalink"
                    readOnly={!canEditThisAsset}
                    dropdownTitle={getText('type')}
                  />

                  {canEditThisAsset && form.formState.isDirty && (
                    <Button.Group>
                      <Form.Submit>{getText('update')}</Form.Submit>
                      <Form.Reset />
                    </Button.Group>
                  )}
                  <Form.FormError />
                </>
              )}
            </Form>
          }
        </div>
      )}
    </div>
  )
}
