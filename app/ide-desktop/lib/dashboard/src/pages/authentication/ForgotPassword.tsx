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
    <div className="flex min-h-screen flex-col items-center justify-center gap-auth text-sm text-primary">
      <form
        className="flex w-full max-w-md flex-col gap-auth rounded-auth bg-selected-frame p-auth shadow-md"
        onSubmit={async event => {
          event.preventDefault()
          await forgotPassword(email)
        }}
      >
        <div className="self-center text-xl font-medium">{getText('forgotYourPassword')}</div>
        <Input
          required
          validate
          type="email"
          autoComplete="email"
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
