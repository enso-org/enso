/** @file A list of previous versions of an asset. */

import { useMutation, useQueryClient, useSuspenseQuery } from '@tanstack/react-query'

import { uniqueString } from 'enso-common/src/utilities/uniqueString'

import { Result } from '#/components/Result'
import { duplicateProjectMutationOptions } from '#/hooks/backendHooks'
import { useOpenProjectLocally } from '#/hooks/projectHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import type { AnyAsset } from '#/services/Backend'
import { AssetType, BackendType, type S3ObjectVersion, S3ObjectVersionId } from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'
import { useStore } from '#/utilities/zustand'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { assetPanelStore } from '../AssetPanelState'
import { AssetVersion } from './AssetVersion'
import { assetVersionsQueryOptions } from './useAssetVersions'

/** Variables for the "add new version" mutation. */
interface AddNewVersionVariables {
  readonly versionId: S3ObjectVersionId
  readonly placeholderId: S3ObjectVersionId
}

/** Props for an {@link AssetVersions}. */
export interface AssetVersionsProps {
  readonly backend: Backend
}

/** Display a list of previous versions of an asset. */
export function AssetVersions(props: AssetVersionsProps) {
  const { backend } = props
  const { getText } = useText()
  const { item } = useStore(assetPanelStore, (state) => ({ item: state.assetPanelProps.item }), {
    unsafeEnableTransition: true,
  })

  if (backend.type === BackendType.local) {
    return (
      <Result
        status="info"
        centered
        title={getText('assetVersions.localAssetsDoNotHaveVersions')}
      />
    )
  }

  if (item == null) {
    return <Result status="info" centered title={getText('assetVersions.notSelected')} />
  }

  // This is SAFE because we know that the backend is a RemoteBackend.
  // eslint-disable-next-line no-restricted-syntax
  return <AssetVersionsInternal {...props} backend={backend as RemoteBackend} item={item} />
}

/** Props for an {@link AssetVersionsInternal}. */
interface AssetVersionsInternalProps extends AssetVersionsProps {
  readonly item: AnyAsset
  readonly backend: RemoteBackend
}

/** Internal implementation of {@link AssetVersions}. */
function AssetVersionsInternal(props: AssetVersionsInternalProps) {
  const { backend, item } = props

  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  const queryClient = useQueryClient()
  const queryOptions = assetVersionsQueryOptions({ assetId: item.id, backend })

  const versionsQuery = useSuspenseQuery({
    ...queryOptions,
    select: (data) =>
      data.versions.map((version, index) => {
        const number = data.versions.length - index
        const title = getText('versionX', number)

        return {
          ...version,
          number,
          title,
        }
      }),
  })

  const versions = versionsQuery.data
  const latestVersion = versions.find((version) => version.isLatest)

  const restoreMutation = useMutation({
    mutationFn: (variables: AddNewVersionVariables) =>
      backend.restoreAsset(item.id, variables.versionId, item.title),
    onMutate: async (variables) => {
      const newItem = {
        isLatest: false,
        key: uniqueString(),
        lastModified: toRfc3339(new Date()),
        versionId: variables.placeholderId,
      }
      await queryClient.cancelQueries({ queryKey: queryOptions.queryKey })

      const previousVersions = queryClient.getQueryData<S3ObjectVersion[]>(queryOptions.queryKey)

      queryClient.setQueryData(
        queryOptions.queryKey,
        (oldVersions: readonly S3ObjectVersion[] | undefined) => {
          if (oldVersions == null) {
            return [newItem]
          }

          return [newItem, ...oldVersions]
        },
      )

      return previousVersions
    },
    onError: (error: unknown, _variables, context) => {
      toastAndLog('restoreProjectError', error, item.title)
      queryClient.setQueryData(queryOptions.queryKey, context)
    },
    meta: { invalidates: [queryOptions.queryKey], awaitInvalidates: true },
  })

  const openProjectLocally = useOpenProjectLocally()

  const duplicateProjectMutation = useMutation(
    duplicateProjectMutationOptions(backend, queryClient, async (project) => {
      await openProjectLocally(project, backend.type)
    }),
  )

  if (versions.length === 0) {
    return <Result status="info" centered title={getText('noVersionsFound')} />
  }

  if (latestVersion == null) {
    return <Result status="error" centered title={getText('fetchLatestVersionError')} />
  }

  return (
    <div className="flex w-full flex-col">
      {versions.map((version, index) => (
        <div key={version.versionId}>
          <AssetVersion
            version={version}
            otherVersions={versions}
            item={item}
            backend={backend}
            previousVersion={versions[index + 1]}
            doRestore={() =>
              restoreMutation.mutateAsync({
                versionId: version.versionId,
                placeholderId: S3ObjectVersionId(uniqueString()),
              })
            }
            doDuplicate={async () => {
              if (item.type === AssetType.project) {
                await duplicateProjectMutation.mutateAsync([
                  item.id,
                  item.title,
                  item.parentId,
                  version.versionId,
                ])

                return
              }

              await backend.duplicateAsset(item.id, version.versionId, item.title)
            }}
          />
          {index !== versions.length - 1 && <div className="ml-[3px] h-5 w-[0.5px] bg-primary" />}
        </div>
      ))}
    </div>
  )
}
