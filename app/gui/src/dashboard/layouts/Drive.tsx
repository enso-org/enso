/** @file The directory header bar and directory item listing. */
import Offline from '#/assets/offline_filled.svg'
import { Button } from '#/components/Button'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import * as result from '#/components/Result'
import SvgMask from '#/components/SvgMask'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as offlineHooks from '#/hooks/offlineHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import AssetsTable from '#/layouts/AssetsTable'
import { DriveBar } from '#/pages/dashboard/Drive/DriveBar'
import DriveProvider from '#/providers/DriveProvider'
import AssetQuery from '#/utilities/AssetQuery'
import * as download from '#/utilities/download'
import * as github from '#/utilities/github'
import * as appUtils from '$/appUtils'
import { isCloudCategory } from '$/providers/category'
import * as authProvider from '$/providers/react'
import { useBackends, useText } from '$/providers/react'
import {
  useContainerData,
  useDriveCurrentBackend,
  useDriveCurrentCategory,
  useDriveLocation,
} from '$/providers/react/container'
import { BackendType, DirectoryDoesNotExistError } from 'enso-common/src/services/Backend'
import { OfflineError } from 'enso-common/src/utilities/errors'
import * as React from 'react'
import { toast } from 'react-toastify'
import { Suspense } from '../components/Suspense'

/** Properties of {@link Drive} component. */
export interface DriveProperties {
  readonly toolbar: React.RefObject<HTMLElement>
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export const Drive = React.memo(function Drive(props: DriveProperties) {
  return (
    <ErrorBoundary>
      <DriveProvider>
        <DriveInner {...props} />
      </DriveProvider>
    </ErrorBoundary>
  )
})

/** Contains directory path and directory contents (projects, folders, secrets and files). */
function DriveInner(props: DriveProperties) {
  const { isOffline } = offlineHooks.useOffline()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useFullUserSession()
  const { localBackend } = useBackends()
  const { getText } = useText()
  const [category, setCategory] = useDriveCurrentCategory()
  const { setDefaultCategory } = useDriveLocation()

  const isCloud = isCloudCategory(category)

  const supportLocalBackend = localBackend != null

  const status =
    isCloud && isOffline ? 'offline'
    : isCloud && !user.isEnabled ? 'not-enabled'
    : 'ok'

  switch (status) {
    case 'not-enabled': {
      return (
        <result.Result
          status="error"
          title={getText('notEnabledTitle')}
          testId="not-enabled-stub"
          subtitle={`${getText('notEnabledSubtitle')}${localBackend == null ? ' ' + getText('downloadFreeEditionMessage') : ''}`}
        >
          <Button.Group align="center">
            <Button variant="primary" size="medium" href={appUtils.SUBSCRIBE_PATH}>
              {getText('upgrade')}
            </Button>

            {supportLocalBackend ?
              <Button
                size="medium"
                variant="primary"
                onPress={() => {
                  setCategory({ type: 'local' })
                }}
              >
                {getText('switchToLocal')}
              </Button>
            : <Button
                data-testid="download-free-edition"
                size="medium"
                variant="accent"
                onPress={async () => {
                  const downloadUrl = await github.getDownloadUrl()
                  if (downloadUrl == null) {
                    toastAndLog('noAppDownloadError')
                  } else {
                    void download.download({ url: downloadUrl })
                  }
                }}
              >
                {getText('downloadFreeEdition')}
              </Button>
            }
          </Button.Group>
        </result.Result>
      )
    }
    case 'offline':
    case 'ok': {
      return (
        <ErrorBoundary
          onBeforeFallbackShown={({ resetErrorBoundary, error, resetQueries }) => {
            if (error instanceof DirectoryDoesNotExistError) {
              toast.error(getText('directoryDoesNotExistError'), {
                toastId: 'directory-does-not-exist-error',
              })
              setDefaultCategory()
              resetQueries()
              resetErrorBoundary()
            }

            if (error instanceof OfflineError) {
              return <OfflineMessage onPress={resetErrorBoundary} />
            }
          }}
        >
          <Suspense>
            <DriveAssetsView {...props} />
          </Suspense>
        </ErrorBoundary>
      )
    }
  }
}

/** The assets view of the Drive. */
function DriveAssetsView(props: DriveProperties) {
  const { setFocusedPanel } = useContainerData()
  const { isOffline } = offlineHooks.useOffline()
  const associatedBackend = useDriveCurrentBackend()
  const [query, setQuery] = React.useState(() => AssetQuery.fromString(''))
  const isCloud = associatedBackend.type === BackendType.remote
  const isInaccessible = isCloud && isOffline

  const onFocus = useEventCallback(() => {
    setFocusedPanel({ type: 'drive' })
  })

  return (
    <div
      className="relative flex w-full grow flex-col overflow-hidden"
      data-testid="drive-view"
      onFocus={onFocus}
    >
      <DriveBar query={query} setQuery={setQuery} toolbar={props.toolbar} />

      {isInaccessible && <OfflineMessage />}
      {!isInaccessible && (
        <Suspense>
          <ErrorBoundary>
            <AssetsTable query={query} setQuery={setQuery} />
          </ErrorBoundary>
        </Suspense>
      )}
    </div>
  )
}

/** Props for an {@link OfflineMessage}. */
interface OfflineMessageProps {
  readonly onPress?: (() => void) | undefined
}

/**
 * Offline message component.
 * Tells the user that the category selected is unavailable in offline mode.
 */
function OfflineMessage(props: OfflineMessageProps) {
  const { onPress } = props

  const { localBackend } = useBackends()
  const { getText } = useText()
  const [, setCategory] = useDriveCurrentCategory()

  return (
    <result.Result
      status={<SvgMask src={Offline} className="aspect-square h-6" />}
      className="my-12"
      centered="horizontal"
      title={getText('cloudUnavailableOffline')}
      subtitle={`${getText('cloudUnavailableOfflineDescription')} ${localBackend != null ? getText('cloudUnavailableOfflineDescriptionOfferLocal') : ''}`}
    >
      {localBackend != null && (
        <Button
          variant="primary"
          className="mx-auto"
          onPress={() => {
            setCategory({ type: 'local' })
            onPress?.()
          }}
        >
          {getText('switchToLocal')}
        </Button>
      )}
    </result.Result>
  )
}
