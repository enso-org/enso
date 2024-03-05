/** @file Container responsible for rendering and interactions in setting username flow, after
 * registration. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'

import Input from '#/components/Input'
import SubmitButton from '#/components/SubmitButton'

// ===================
// === SetUsername ===
// ===================

/** A form for users to set their username upon registration. */
export default function SetUsername() {
  const { setUsername: authSetUsername } = authProvider.useAuth()
  const { email } = authProvider.usePartialUserSession()
  const { backend } = backendProvider.useBackend()

  const [username, setUsername] = React.useState('')

  return (
    <div className="flex min-h-screen flex-col items-center justify-center gap-auth text-sm text-primary">
      <form
        data-testid="set-username-panel"
        className="flex w-full max-w-md flex-col gap-auth rounded-auth bg-selected-frame p-auth shadow-md"
        onSubmit={async event => {
          event.preventDefault()
          await authSetUsername(backend, username, email)
        }}
      >
        <div className="self-center text-xl font-medium">Set your username</div>
        <Input
          id="username"
          type="text"
          name="username"
          autoComplete="off"
          icon={AtIcon}
          placeholder="Enter your username"
          value={username}
          setValue={setUsername}
        />
        <SubmitButton text="Set username" icon={ArrowRightIcon} />
      </form>
    </div>
  )
}
