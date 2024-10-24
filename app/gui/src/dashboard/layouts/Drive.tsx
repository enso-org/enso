/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as appUtils from '#/appUtils'

import * as offlineHooks from '#/hooks/offlineHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetListEventType from '#/events/AssetListEventType'

import AssetPanel from '#/layouts/AssetPanel'
import type * as assetsTable from '#/layouts/AssetsTable'
import AssetsTable from '#/layouts/AssetsTable'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import CategorySwitcher from '#/layouts/CategorySwitcher'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import DriveBar from '#/layouts/DriveBar'
import Labels from '#/layouts/Labels'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

import { useRootDirectoryId } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useDriveStore } from '#/providers/DriveProvider'
import AssetQuery from '#/utilities/AssetQuery'
import * as download from '#/utilities/download'
import * as github from '#/utilities/github'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =============
// === Drive ===
// =============

/** Props for a {@link Drive}. */
export interface DriveProps {
  readonly category: categoryModule.Category
  readonly setCategory: (category: categoryModule.Category) => void
  readonly hidden: boolean
  readonly initialProjectName: string | null
  readonly assetsManagementApiRef: React.Ref<assetsTable.AssetManagementApi>
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export default function Drive(props: DriveProps) {
  const { category, setCategory, hidden, initialProjectName, assetsManagementApiRef } = props

  const { isOffline } = offlineHooks.useOffline()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useFullUserSession()
  const localBackend = backendProvider.useLocalBackend()
  const backend = backendProvider.useBackend(category)
  const { getText } = textProvider.useText()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const [query, setQuery] = React.useState(() => AssetQuery.fromString(''))
  const rootDirectoryId = useRootDirectoryId(backend, category)
  const isCloud = categoryModule.isCloudCategory(category)
  const supportLocalBackend = localBackend != null

  const status =
    isCloud && isOffline ? 'offline'
    : isCloud && !user.isEnabled ? 'not-enabled'
    : 'ok'

  const driveStore = useDriveStore()

  const getTargetDirectory = React.useCallback(
    () => driveStore.getState().targetDirectory,
    [driveStore],
  )

  const doUploadFiles = useEventCallback(async (files: File[]) => {
    if (isCloud && isOffline) {
      // This should never happen, however display a nice error message in case it does.
      toastAndLog('offlineUploadFilesError')
    } else {
      dispatchAssetListEvent({
        type: AssetListEventType.uploadFiles,
        parentKey: getTargetDirectory()?.key ?? rootDirectoryId,
        parentId: getTargetDirectory()?.item.id ?? rootDirectoryId,
        files,
      })
    }
  })

  const doEmptyTrash = React.useCallback(() => {
    dispatchAssetListEvent({ type: AssetListEventType.emptyTrash })
  }, [dispatchAssetListEvent])

  const doCreateDatalink = useEventCallback(async (name: string, value: unknown) => {
    dispatchAssetListEvent({
      type: AssetListEventType.newDatalink,
      parentKey: getTargetDirectory()?.key ?? rootDirectoryId,
      parentId: getTargetDirectory()?.item.id ?? rootDirectoryId,
      name,
      value,
    })
  })

  switch (status) {
    case 'not-enabled': {
      return (
        <result.Result
          status="error"
          title={getText('notEnabledTitle')}
          testId="not-enabled-stub"
          subtitle={`${getText('notEnabledSubtitle')}${localBackend == null ? ' ' + getText('downloadFreeEditionMessage') : ''}`}
        >
          <ariaComponents.ButtonGroup align="center">
            <ariaComponents.Button variant="primary" size="medium" href={appUtils.SUBSCRIBE_PATH}>
              {getText('upgrade')}
            </ariaComponents.Button>

            {!supportLocalBackend && (
              <ariaComponents.Button
                data-testid="download-free-edition"
                size="medium"
                variant="accent"
                onPress={async () => {
                  const downloadUrl = await github.getDownloadUrl()
                  if (downloadUrl == null) {
                    toastAndLog('noAppDownloadError')
                  } else {
                    download.download(downloadUrl)
                  }
                }}
              >
                {getText('downloadFreeEdition')}
              </ariaComponents.Button>
            )}
          </ariaComponents.ButtonGroup>
        </result.Result>
      )
    }
    case 'offline':
    case 'ok': {
      return (
        <div className={tailwindMerge.twMerge('relative flex grow', hidden && 'hidden')}>
          <div
            data-testid="drive-view"
            className="mt-4 flex flex-1 flex-col gap-4 overflow-visible px-page-x"
          >
            <DriveBar
              backend={backend}
              query={query}
              setQuery={setQuery}
              category={category}
              doEmptyTrash={doEmptyTrash}
              doUploadFiles={doUploadFiles}
              doCreateDatalink={doCreateDatalink}
            />

            <div className="flex flex-1 gap-drive overflow-hidden">
              <div className="flex w-36 flex-col gap-drive-sidebar overflow-y-auto py-drive-sidebar-y">
                <CategorySwitcher category={category} setCategory={setCategory} />
                {isCloud && (
                  <Labels
                    backend={backend}
                    draggable={category.type !== 'trash'}
                    query={query}
                    setQuery={setQuery}
                  />
                )}
              </div>
              {status === 'offline' ?
                <result.Result
                  status="info"
                  className="my-12"
                  centered="horizontal"
                  title={getText('cloudUnavailableOffline')}
                  subtitle={`${getText('cloudUnavailableOfflineDescription')} ${supportLocalBackend ? getText('cloudUnavailableOfflineDescriptionOfferLocal') : ''}`}
                >
                  {supportLocalBackend && (
                    <ariaComponents.Button
                      variant="primary"
                      size="small"
                      className="mx-auto"
                      onPress={() => {
                        setCategory({ type: 'local' })
                      }}
                    >
                      {getText('switchToLocal')}
                    </ariaComponents.Button>
                  )}
                </result.Result>
              : <AssetsTable
                  assetManagementApiRef={assetsManagementApiRef}
                  hidden={hidden}
                  query={query}
                  setQuery={setQuery}
                  category={category}
                  initialProjectName={initialProjectName}
                />
              }
            </div>
          </div>
          <AssetPanel backendType={backend.type} category={category} />
        </div>
      )
    }
  }
}
