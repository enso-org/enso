/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as router from 'react-router-dom'
import * as z from 'zod'

import { CLOUD_DASHBOARD_DOMAIN } from 'enso-common'

import { FORGOT_PASSWORD_PATH, REGISTRATION_PATH } from '#/appUtils'
import CreateAccountIcon from '#/assets/create_account.svg'
import GithubIcon from '#/assets/github.svg'
import GoogleIcon from '#/assets/google.svg'
import { Button, Form, Input } from '#/components/AriaComponents'
import Link from '#/components/Link'
import TextLink from '#/components/TextLink'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { useMutation } from '@tanstack/react-query'

// =============
// === Login ===
// =============

/** A form for users to log in. */
export default function Login() {
  const location = router.useLocation()
  const auth = useAuth()
  const { getText } = useText()
  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email')
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  const form = Form.useForm({
    schema: z.object({
      email: z.string(),
      password: z.string(),
    }),
  })

  const signInWithGoogleMutation = useMutation({ mutationFn: auth.signInWithGoogle })
  const signInWithGitHubMutation = useMutation({ mutationFn: auth.signInWithGitHub })
  const signInWithPasswordMutation = useMutation({
    mutationFn: ({ email, password }: { email: string; password: string }) =>
      auth.signInWithPassword(email, password),
  })
  const signInWithGoogle = signInWithGoogleMutation.mutateAsync
  const signInWithGitHub = signInWithGitHubMutation.mutateAsync
  const signInWithPassword = signInWithPasswordMutation.mutateAsync

  return (
    <AuthenticationPage
      title={getText('loginToYourAccount')}
      supportsOffline={supportsOffline}
      footer={
        <Link
          openInBrowser={localBackend != null}
          to={
            localBackend != null ?
              'https://' + CLOUD_DASHBOARD_DOMAIN + REGISTRATION_PATH
            : REGISTRATION_PATH
          }
          icon={CreateAccountIcon}
          text={getText('dontHaveAnAccount')}
        />
      }
    >
      <div className="flex flex-col gap-auth">
        <Button
          size="large"
          variant="outline"
          icon={GoogleIcon}
          onPress={async () => {
            await signInWithGoogle()
          }}
        >
          {getText('signUpOrLoginWithGoogle')}
        </Button>
        <Button
          size="large"
          variant="outline"
          icon={GithubIcon}
          onPress={async () => {
            await signInWithGitHub()
          }}
        >
          {getText('signUpOrLoginWithGitHub')}
        </Button>
      </div>
      <div />
      <Form
        form={form}
        className="flex flex-col gap-6"
        onSubmit={(values) => signInWithPassword(values)}
      >
        {({ register }) => (
          <>
            <Input
              autoFocus
              required
              type="email"
              autoComplete="email"
              defaultValue={initialEmail ?? ''}
              placeholder={getText('emailPlaceholder')}
              {...register('email')}
            />
            <div className="flex w-full flex-col">
              <Input
                required
                type="password"
                autoComplete="current-password"
                placeholder={getText('passwordPlaceholder')}
                error={getText('passwordValidationError')}
                {...register('password')}
              />
              <TextLink to={FORGOT_PASSWORD_PATH} text={getText('forgotYourPassword')} />
            </div>

            <Form.FormError />
            <Form.Submit className="w-full">{getText('login')}</Form.Submit>
          </>
        )}
      </Form>
    </AuthenticationPage>
  )
}
