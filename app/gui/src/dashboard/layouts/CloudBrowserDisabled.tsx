/**
 * @file Layout that disables the dashboard if the cloud is disabled.
 */

import { Button } from '#/components/Button'
import Page from '#/components/Page'
import { Result } from '#/components/Result'
import { Text } from '#/components/Text'
import { useTimeoutCallback } from '#/hooks/timeoutHooks'
import { download } from '#/utilities/download'
import { getDownloadUrl } from '#/utilities/github'
import { unsafeWriteValue } from '#/utilities/write'
import * as appUtils from '$/appUtils'
import { useText } from '$/providers/react'
import * as React from 'react'

const DEFAULT_REDIRECT_DELAY_MS = 3_000

/** Props for a {@link CloudBrowserDisabledPage}. */
export interface CloudBrowserDisabledLayoutProps {
  /** The delay in milliseconds before redirecting to the desktop edition. */
  readonly redirectDelayMs?: number
  /** The path to redirect to if the user is not a full user. */
  readonly redirectPath?: string
}

/**
 * Layout that disables the dashboard if the cloud is disabled.
 */
export function CloudBrowserDisabledPage(props: CloudBrowserDisabledLayoutProps) {
  const { redirectDelayMs = DEFAULT_REDIRECT_DELAY_MS, redirectPath = '' } = props
  const { getText } = useText()

  const [isRedirecting, setIsRedirecting] = React.useState(true)

  const normalizedRedirectPath = redirectPath.startsWith('/') ? redirectPath.slice(1) : redirectPath

  const path = appUtils.OPEN_IDE_DEEPLINK + normalizedRedirectPath

  useTimeoutCallback({
    callback: () => {
      unsafeWriteValue(window.location, 'href', path)
      setIsRedirecting(false)
    },
    ms: redirectDelayMs,
  })

  return (
    <Page>
      <Result
        status={isRedirecting ? 'loading' : 'info'}
        title={getText('cloudBrowserDisabledTitle')}
        subtitle={getText('cloudBrowserDisabledSubtitle')}
      >
        <Button.Group align="center" verticalAlign="center">
          <Button variant="primary" href={path}>
            {getText('openInDesktop')}
          </Button>

          <Text>{getText('or')}</Text>

          <Button
            variant="outline"
            onPress={async () => {
              const downloadUrl = await getDownloadUrl()

              if (downloadUrl != null) {
                void download({ url: downloadUrl })
              }
            }}
          >
            {getText('downloadIDE')}
          </Button>
        </Button.Group>
      </Result>
    </Page>
  )
}
