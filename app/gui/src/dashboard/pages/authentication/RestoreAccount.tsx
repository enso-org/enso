/** @file Restore an account that has been deleted. */

import UntrashIcon from '#/assets/untrash.svg'

import { useAuth } from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import { LOGIN_PATH } from '#/appUtils'
import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import { useSessionAPI } from '#/providers/SessionProvider'
import { useNavigate } from 'react-router'

/** Restore an account that has been deleted. */
export default function RestoreAccount() {
  const { getText } = textProvider.useText()
  const { restoreUser } = useAuth()
  const { signOut } = useSessionAPI()
  const navigate = useNavigate()

  return (
    <div className="flex h-full w-full overflow-auto">
      <div className="flex min-h-96 w-full flex-col items-center justify-center">
        <SvgMask
          src={UntrashIcon}
          className="bg-icon-selected mb-4 aspect-square h-12"
          color={''}
        />
        <aria.Heading level={1} className="mb-4 text-3xl">
          {getText('restoreAccount')}
        </aria.Heading>

        <p className="max-w-[36rem] text-balance text-center">
          {getText('restoreAccountDescription')}
        </p>

        <div className="mt-8 flex items-center gap-8">
          <ariaComponents.Button
            onPress={async () => {
              await restoreUser()
            }}
            variant="icon"
            className="flex items-center justify-center gap-2 rounded-full bg-blue-600 px-4 py-auth-input-y text-white transition-all duration-auth selectable enabled:active"
          >
            {getText('restoreAccountSubmit')}
          </ariaComponents.Button>

          <ariaComponents.Button
            variant="icon"
            onPress={async () => {
              await signOut().then(() => {
                navigate(LOGIN_PATH)
              })
            }}
          >
            {getText('signOutShortcut')}
          </ariaComponents.Button>
        </div>
      </div>
    </div>
  )
}
