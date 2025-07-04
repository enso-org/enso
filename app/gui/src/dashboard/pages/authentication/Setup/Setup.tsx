/** @file Setup page */
import { Loader } from '#/components/Loader'
import Page from '#/components/Page'
import { useMount } from '#/hooks/mountHooks'
import { UserSessionType } from '$/providers/auth'
import { useAuth, useUserSession } from '$/providers/react'

/** Setup page */
export function Setup() {
  const { setUsername } = useAuth()
  const session = useUserSession()

  useMount(() => {
    if (session?.type === UserSessionType.partial) {
      void setUsername(session.email)
    }
  })

  return (
    <Page>
      <Loader className="h-full w-full" />
    </Page>
  )
}
