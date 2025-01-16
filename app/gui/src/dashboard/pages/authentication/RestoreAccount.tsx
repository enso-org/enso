/** @file Restore an account that has been deleted. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import UntrashIcon from '#/assets/untrash.svg'

import { useAuth } from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import { LOGIN_PATH } from '#/appUtils'
import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import { useSessionAPI } from '#/providers/SessionProvider'
import { useNavigate } from 'react-router'

// ======================
// === RestoreAccount ===
// ======================

/** Restore an account that has been deleted. */
export default function RestoreAccount() {
  const { getText } = textProvider.useText()
  const { restoreUser } = useAuth()
  const { signOut } = useSessionAPI()
  const navigate = useNavigate()

  const signOutMutation = reactQuery.useMutation({
    mutationFn: signOut,
    onSuccess: () => {
      navigate(LOGIN_PATH)
    },
  })
  const restoreAccountMutation = reactQuery.useMutation({
    mutationFn: () => restoreUser(),
  })

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
              await restoreAccountMutation.mutateAsync()
            }}
            loading={restoreAccountMutation.isPending}
            isDisabled={restoreAccountMutation.isPending}
            variant="icon"
            className="flex items-center justify-center gap-icon-with-text rounded-full bg-blue-600 px-4 py-auth-input-y text-white transition-all duration-auth selectable enabled:active"
          >
            {getText('restoreAccountSubmit')}
          </ariaComponents.Button>

          <ariaComponents.Button
            variant="icon"
            loading={signOutMutation.isPending}
            isDisabled={signOutMutation.isPending}
            onPress={async () => {
              await signOutMutation.mutateAsync()
            }}
          >
            {getText('signOutShortcut')}
          </ariaComponents.Button>
        </div>
      </div>
    </div>
  )
}
