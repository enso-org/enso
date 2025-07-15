/** @file Login component responsible for rendering and interactions in sign in flow. */
import { Button } from '#/components/Button'
import { DIALOG_BACKGROUND } from '#/components/Dialog'
import Page from '#/components/Page'
import { Text } from '#/components/Text'
import { useOffline } from '#/hooks/offlineHooks'
import { useSession, useText } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'

const OFFLINE_ALERT_CLASSES = DIALOG_BACKGROUND({
  className: 'flex mt-auto rounded-sm items-center justify-center p-4 px-12 rounded-4xl',
})

/** A form for users to log in. */
export default function Login() {
  const { signIn, signUp } = useSession()
  const { getText } = useText()
  const supportsOffline = useFeatureFlag('enableLocalBackend')
  const { isOffline } = useOffline()

  return (
    <Page>
      {isOffline && (
        <div className={OFFLINE_ALERT_CLASSES}>
          <Text className="text-center" balance elementType="p">
            {getText('loginUnavailableOffline')}{' '}
            {supportsOffline && getText('loginUnavailableOfflineLocal')}
          </Text>
        </div>
      )}
      <div className="flex flex-col gap-2">
        <Button onPress={signIn}>{getText('login')}</Button>
        <Button onPress={signUp}>{getText('register')}</Button>
      </div>
    </Page>
  )
}
