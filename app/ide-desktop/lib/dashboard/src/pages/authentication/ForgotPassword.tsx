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
import SubmitButton from '#/components/SubmitButton'

// ======================
// === ForgotPassword ===
// ======================

/** A form for users to request for their password to be reset. */
export default function ForgotPassword() {
  const { forgotPassword } = authProvider.useAuth()
  const { getText } = textProvider.useText()
  const [email, setEmail] = React.useState('')

  return (
    <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
      <form
        className="flex flex-col gap-6 bg-frame-selected rounded-2xl shadow-md p-8 w-full max-w-md"
        onSubmit={async event => {
          event.preventDefault()
          await forgotPassword(email)
        }}
      >
        <div className="font-medium self-center text-xl">{getText('forgotYourPassword')}</div>
        <Input
          required
          validate
          type="email"
          autoComplete="email"
          label={getText('email')}
          icon={AtIcon}
          placeholder={getText('emailPlaceholder')}
          value={email}
          setValue={setEmail}
        />
        <SubmitButton text={getText('sendLink')} icon={ArrowRightIcon} />
      </form>
      <Link to={appUtils.LOGIN_PATH} icon={GoBackIcon} text={getText('goBackToLogin')} />
    </div>
  )
}
