/** @file The directory header bar and directory item listing. */
import Offline from '#/assets/offline_filled.svg'
import { Button } from '#/components/Button'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import * as result from '#/components/Result'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import * as offlineHooks from '#/hooks/offlineHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import AssetsTable, { AssetsTableAssetsUnselector } from '#/layouts/AssetsTable'
import CategorySwitcher from '#/layouts/CategorySwitcher'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import { DriveBar } from '#/pages/dashboard/Drive/DriveBar'
import { DirectoryDoesNotExistError } from '#/services/Backend'
import AssetQuery from '#/utilities/AssetQuery'
import * as download from '#/utilities/download'
import * as github from '#/utilities/github'
import { OfflineError } from '#/utilities/HttpClient'
import * as appUtils from '$/appUtils'
import * as authProvider from '$/providers/react'
import { useBackends, useText } from '$/providers/react'
import * as React from 'react'
import { useDeferredValue } from 'react'
import { toast } from 'react-toastify'
import { Suspense } from '../components/Suspense'
import { useCategoriesAPI } from './Drive/Categories/categoriesHooks'

/** Props for a {@link Drive}. */
export interface DriveProps {
  readonly initialProjectName: string | null
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
function Drive(props: DriveProps) {
  const { isOffline } = offlineHooks.useOffline()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { user } = authProvider.useFullUserSession()
  const { localBackend } = useBackends()
  const { getText } = useText()
  const categoriesAPI = useCategoriesAPI()
  const { category, resetCategory, setCategory } = categoriesAPI

  const isCloud = categoryModule.isCloudCategory(category)

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
                  setCategory('local')
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
              resetCategory()
              resetQueries()
              resetErrorBoundary()
            }

            if (error instanceof OfflineError) {
              return (
                <OfflineMessage
                  supportLocalBackend={supportLocalBackend}
                  setCategory={(nextCategory) => {
                    setCategory(nextCategory)
                    resetErrorBoundary()
                  }}
                />
              )
            }
          }}
        >
          <Suspense>
            <DriveAssetsView {...props} category={category} setCategory={setCategory} />
          </Suspense>
        </ErrorBoundary>
      )
    }
  }
}

/** Props for a {@link DriveAssetsView}. */
interface DriveAssetsViewProps extends DriveProps {
  readonly category: Category
  readonly setCategory: (categoryId: Category['id']) => void
}

/** The assets view of the Drive. */
function DriveAssetsView(props: DriveAssetsViewProps) {
  const { category, setCategory, initialProjectName } = props

  const deferredCategory = useDeferredValue(category)

  const { getText } = useText()
  const { isOffline } = offlineHooks.useOffline()
  const { user } = authProvider.useFullUserSession()
  const { localBackend, backendForType } = useBackends()
  const backend = backendForType(category.backend)

  const [query, setQuery] = React.useState(() => AssetQuery.fromString(''))

  const isCloud = categoryModule.isCloudCategory(category)
  const supportLocalBackend = localBackend != null

  const status =
    isCloud && isOffline ? 'offline'
    : isCloud && !user.isEnabled ? 'not-enabled'
    : 'ok'

  return (
    <div className="relative flex h-full w-full">
      <div
        data-testid="drive-view"
        className="mt-4 flex flex-1 flex-col gap-4 overflow-visible px-4"
      >
        <div className="grid flex-1 gap-3 overflow-hidden sm:grid-cols-[180px_minmax(0,1fr)]">
          <div className="grid-col-1 hidden flex-none flex-col gap-drive-sidebar overflow-y-auto overflow-x-hidden pt-1 sm:flex">
            <div className="flex flex-col gap-2">
              <Text variant="subtitle" weight="semibold">
                {getText('category')}
              </Text>
              <CategorySwitcher category={category} setCategoryId={setCategory} />
            </div>

            <AssetsTableAssetsUnselector />
          </div>

          <div className="grid-col-1 sm:grid-col-2 flex flex-col gap-3">
            <DriveBar
              backend={backend}
              query={query}
              setQuery={setQuery}
              category={category}
              setCategoryId={setCategory}
            />

            {status === 'offline' ?
              <OfflineMessage supportLocalBackend={supportLocalBackend} setCategory={setCategory} />
            : <Suspense>
                <ErrorBoundary>
                  <AssetsTable
                    query={query}
                    setQuery={setQuery}
                    category={deferredCategory}
                    initialProjectName={initialProjectName}
                  />
                </ErrorBoundary>
              </Suspense>
            }
          </div>
        </div>
      </div>
    </div>
  )
}

/** Props for an {@link OfflineMessage}. */
interface OfflineMessageProps {
  readonly supportLocalBackend: boolean
  readonly setCategory: (category: categoryModule.Category['id']) => void
}

/**
 * Offline message component.
 * Displays info that the ctegory selected in unavailable
 * in offline mode
 */
function OfflineMessage(props: OfflineMessageProps) {
  const { supportLocalBackend, setCategory } = props
  const { getText } = useText()

  return (
    <result.Result
      status={<SvgMask src={Offline} className="aspect-square h-6" />}
      className="my-12"
      centered="horizontal"
      title={getText('cloudUnavailableOffline')}
      subtitle={`${getText('cloudUnavailableOfflineDescription')} ${supportLocalBackend ? getText('cloudUnavailableOfflineDescriptionOfferLocal') : ''}`}
    >
      {supportLocalBackend && (
        <Button
          variant="primary"
          className="mx-auto"
          onPress={() => {
            setCategory('local')
          }}
        >
          {getText('switchToLocal')}
        </Button>
      )}
    </result.Result>
  )
}

export default React.memo(Drive)
