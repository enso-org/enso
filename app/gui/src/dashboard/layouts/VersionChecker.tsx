/** @file Check the version. */
import { useQuery, useQueryClient } from '@tanstack/react-query'

import DownloadIcon from '#/assets/download.svg'
import NewTabIcon from '#/assets/new_tab.svg'
import SnoozeIcon from '#/assets/snooze.svg'
import { IS_DEV_MODE } from 'enso-common/src/detect'

import { useToastAndLog } from '#/hooks/toastAndLogHooks'

import { useEnableVersionChecker, useSetEnableVersionChecker } from '#/components/Devtools'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'

import { Button, ButtonGroup, Dialog, Text } from '#/components/AriaComponents'

import { Stepper } from '#/components/Stepper'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { download } from '#/utilities/download'
import { getDownloadUrl, getLatestRelease } from '#/utilities/github'
import { startTransition, useState } from 'react'

const CURRENT_VERSION: string = $config.VERSION ?? 'unknown-dev'
const CURRENT_VERSION_IS_DEV = CURRENT_VERSION.endsWith('-dev')
const CURRENT_VERSION_IS_NIGHTLY = CURRENT_VERSION.includes('-nightly')
const CURRENT_VERSION_NUMBER = getVersionNumber(CURRENT_VERSION)
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
const STALE_TIME = 24 * 60 * 60 * 1000 // 1 day
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
const STALE_TIME_ERROR = 10 * 60 * 1000 // 10 minutes

/** Check the version. */
export default function VersionChecker() {
  const [isOpen, setIsOpen] = useState(false)

  const { getText, locale } = useText()
  const toastAndLog = useToastAndLog()
  const localBackend = useLocalBackend()

  const supportsLocalBackend = localBackend != null
  const overrideValue = useEnableVersionChecker()
  const setOverrideValue = useSetEnableVersionChecker()
  const shouldOverride = overrideValue ?? false

  const enableVersionChecker = IS_DEV_MODE ? shouldOverride : supportsLocalBackend

  const queryClient = useQueryClient()
  const metadataQuery = useQuery({
    queryKey: ['latestRelease'],
    queryFn: async () => {
      const latestRelease = await getLatestRelease()
      return { ...latestRelease, isPostponed: false }
    },
    select: (data) => {
      const versionNumber = getVersionNumber(data.tag_name)
      const publishedAt = new Date(data.published_at).toLocaleString(locale, {
        dateStyle: 'long',
      })

      if (versionNumber == null) {
        return {
          versionNumber: CURRENT_VERSION_NUMBER,
          publishedAt,
          tagName: CURRENT_VERSION,
          htmlUrl: data.html_url,
          isPostponed: data.isPostponed,
        }
      }

      return {
        versionNumber,
        publishedAt,
        tagName: data.tag_name,
        htmlUrl: data.html_url,
        isPostponed: data.isPostponed,
      }
    },
    enabled: enableVersionChecker,
    meta: { persist: false },
    staleTime: (query) => {
      if (query.state.error) {
        return STALE_TIME_ERROR
      }

      return STALE_TIME
    },
  })

  const { stepperState, isLastStep, resetStepper } = Stepper.useStepperState({ steps: 2 })

  const remindLater = useEventCallback(() => {
    setIsOpen(false)
    // User asked to be reminded later, so we suppress the dialog from showing again for next 24 hours.
    queryClient.setQueryData(['latestRelease'], { ...metadataQuery.data, isPostponed: true })
  })

  const onDownload = useEventCallback(async () => {
    const downloadUrl = await getDownloadUrl()

    if (downloadUrl == null) {
      toastAndLog('noAppDownloadError')
    } else {
      download(downloadUrl)
      stepperState.nextStep()
    }
  })

  if (!metadataQuery.isSuccess) {
    return null
  }

  if (metadataQuery.data.isPostponed) {
    return null
  }

  const { versionNumber, tagName, htmlUrl, publishedAt } = metadataQuery.data
  const latestVersionNumber = versionNumber
  const latestVersion = tagName

  const shouldBeShown = (() => {
    if (shouldOverride) {
      return true
    }

    if (latestVersionNumber == null) {
      return false
    }

    if (CURRENT_VERSION_NUMBER == null || CURRENT_VERSION_IS_DEV || CURRENT_VERSION_IS_NIGHTLY) {
      return false
    }

    return latestVersionNumber > CURRENT_VERSION_NUMBER
  })()

  if (!shouldBeShown) {
    return null
  }

  if (!isOpen && !isLastStep) {
    startTransition(() => {
      setIsOpen(true)
    })
  }

  return (
    <Dialog
      title={getText('versionOutdatedTitle')}
      size="large"
      modalProps={{ isOpen }}
      isDismissable={isLastStep}
      hideCloseButton={!isLastStep}
      isKeyboardDismissDisabled={!isLastStep}
      onOpenChange={(openChange) => {
        startTransition(() => {
          if (!openChange && overrideValue === true) {
            setOverrideValue(false)
          }

          if (!isLastStep) {
            remindLater()
          }

          resetStepper()

          setIsOpen(openChange)
        })
      }}
    >
      {() => (
        <Stepper state={stepperState} renderStep={null}>
          <Stepper.StepContent index={0}>
            <div className="flex flex-col">
              <Text className="text-center text-sm" balance>
                {getText('versionOutdatedPrompt')}
              </Text>
              <div className="mb-4 mt-3 flex flex-col items-center">
                <Text.Group>
                  <Text variant="h1">{getText('latestVersion', latestVersion, publishedAt)}</Text>

                  <Button
                    variant="link"
                    href={htmlUrl}
                    target="_blank"
                    icon={NewTabIcon}
                    iconPosition="end"
                  >
                    {getText('changeLog')}
                  </Button>

                  <Text variant="body-sm">
                    {getText('yourVersion')}{' '}
                    <Text weight="bold" variant="body">
                      {CURRENT_VERSION}
                    </Text>
                  </Text>
                </Text.Group>
              </div>

              <ButtonGroup className="justify-center">
                <Button
                  size="medium"
                  variant="outline"
                  fullWidth
                  onPress={remindLater}
                  icon={SnoozeIcon}
                  iconPosition="end"
                >
                  {getText('remindMeLater')}
                </Button>
                <Button
                  size="medium"
                  fullWidth
                  variant="primary"
                  onPress={onDownload}
                  icon={DownloadIcon}
                  iconPosition="end"
                >
                  {getText('download')}
                </Button>
              </ButtonGroup>
            </div>
          </Stepper.StepContent>

          <Stepper.StepContent index={1}>
            <div className="flex flex-col items-center text-center">
              <Text balance variant="body">
                {getText('downloadingAppMessage')}
              </Text>

              <Dialog.Close variant="primary" className="mt-4 min-w-48">
                {getText('close')}
              </Dialog.Close>
            </div>
          </Stepper.StepContent>
        </Stepper>
      )}
    </Dialog>
  )
}

/**
 * Get the version number from a version string.
 * @param version - The version string.
 * @returns The version number, or null if the version string is not a valid version number.
 */
function getVersionNumber(version: string) {
  const versionNumber = Number(version.replace('.', ''))
  return isNaN(versionNumber) ? null : versionNumber
}
