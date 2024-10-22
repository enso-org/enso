/** @file Check the version. */
import * as React from 'react'

import { useQuery } from '@tanstack/react-query'

import { IS_DEV_MODE } from 'enso-common/src/detect'

import { useToastAndLog } from '#/hooks/toastAndLogHooks'

import { useEnableVersionChecker } from '#/components/Devtools'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'

import { Button, ButtonGroup, Dialog, Text } from '#/components/AriaComponents'

import { download } from '#/utilities/download'
import { getDownloadUrl, getLatestRelease, LATEST_RELEASE_PAGE_URL } from '#/utilities/github'

// ======================
// === VersionChecker ===
// ======================

/** Check the version. */
export default function VersionChecker() {
  const [isOpen, setIsOpen] = React.useState(false)
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const localBackend = useLocalBackend()
  const supportsLocalBackend = localBackend != null
  const enableVersionChecker = useEnableVersionChecker() ?? (!IS_DEV_MODE && supportsLocalBackend)

  const metadataQuery = useQuery({
    queryKey: ['latestRelease', enableVersionChecker],
    queryFn: () => (enableVersionChecker ? getLatestRelease() : null),
  })
  const latestVersion = metadataQuery.data?.tag_name

  React.useEffect(() => {
    if (latestVersion != null && latestVersion !== process.env.ENSO_CLOUD_DASHBOARD_VERSION) {
      setIsOpen(true)
    }
  }, [latestVersion])

  return (
    <Dialog
      title={getText('versionOutdatedTitle')}
      modalProps={{ isOpen }}
      onOpenChange={setIsOpen}
    >
      <div className="flex flex-col gap-3">
        <Text className="text-center text-sm">{getText('versionOutdatedPrompt')}</Text>
        <div className="flex flex-col">
          <Text className="text-center text-sm">
            {getText('yourVersion')}{' '}
            <Text className="text-sm font-semibold text-danger">
              {process.env.ENSO_CLOUD_DASHBOARD_VERSION ?? getText('unknownPlaceholder')}
            </Text>
          </Text>
          <Text className="text-center text-sm">
            {getText('latestVersion')}{' '}
            <Text className="text-sm font-semibold text-accent">
              {latestVersion ?? getText('unknownPlaceholder')}
            </Text>
          </Text>
        </div>
        <ButtonGroup className="justify-center">
          <Button
            size="medium"
            variant="accent"
            onPress={async () => {
              const downloadUrl = await getDownloadUrl()
              if (downloadUrl == null) {
                toastAndLog('noAppDownloadError')
              } else {
                download(downloadUrl)
              }
            }}
          >
            {getText('download')}
          </Button>
          <Button size="medium" href={LATEST_RELEASE_PAGE_URL} target="_blank">
            {getText('seeLatestRelease')}
          </Button>
        </ButtonGroup>
      </div>
    </Dialog>
  )
}
