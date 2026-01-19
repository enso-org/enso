/** @file A page. */
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { ModalWrapper } from '#/components/ModalWrapper'
import InfoBar from '#/layouts/InfoBar'
import { useLocalStorage, useSession } from '$/providers/react'
import * as React from 'react'
import { toast } from 'react-toastify'

/** Props for a {@link Page}. */
export interface PageProps extends Readonly<React.PropsWithChildren> {
  readonly hideInfoBar?: true
  readonly hideModalWrapper?: true
}

/** A page. */
export default function Page(props: PageProps) {
  const { hideInfoBar = false, hideModalWrapper = false, children } = props
  const { signOut } = useSession()
  // const openedProjects = useOpenedProjects()
  const localStorage = useLocalStorage()

  return (
    <>
      <ErrorBoundary
        onReset={async () => {
          toast.error('An unexpected error occurred. You have been signed out.')
          await signOut()
          // This doesn't work because of issues with closing projects on logout,
          // causing the app to crash instead.
          // openedProjects.closeAllProjects()
          localStorage.set('openedTabs', [])
          // Hard reload to reset the app state.
          location.reload()
        }}
      >
        {children}
      </ErrorBoundary>
      {!hideInfoBar && (
        <div className="fixed right top z-1 m-2.5 text-primary">
          <InfoBar />
        </div>
      )}
      {!hideModalWrapper && <ModalWrapper />}
    </>
  )
}
