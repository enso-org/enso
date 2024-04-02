/** @file Container responsible for rendering and interactions in first half of forgot password
 * flow. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import GoBackIcon from 'enso-assets/go_back.svg'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import Input from '#/components/Input'
import Link from '#/components/Link'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import SubmitButton from '#/components/SubmitButton'

import * as eventModule from '#/utilities/event'

// ======================
// === ForgotPassword ===
// ======================

/** A form for users to request for their password to be reset. */
export default function ForgotPassword() {
  const { forgotPassword } = authProvider.useAuth()
  const { getText } = textProvider.useText()
  const [email, setEmail] = React.useState('')

  return (
    <AuthenticationPage
      title={getText('forgotYourPassword')}
      footer={<Link to={appUtils.LOGIN_PATH} icon={GoBackIcon} text={getText('goBackToLogin')} />}
      onSubmit={async event => {
        event.preventDefault()
        await forgotPassword(email)
      }}
    >
      <Input
        autoFocus
        required
        validate
        type="email"
        autoComplete="email"
        icon={AtIcon}
        placeholder={getText('emailPlaceholder')}
        value={email}
        setValue={setEmail}
      />
      <SubmitButton
        text={getText('sendLink')}
        icon={ArrowRightIcon}
        onPress={eventModule.submitForm}
      />
    </AuthenticationPage>
  )
}
