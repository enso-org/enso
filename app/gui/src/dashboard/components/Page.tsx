/** @file A page. */
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { ModalWrapper } from '#/components/ModalWrapper'
import InfoBar from '#/layouts/InfoBar'
import { useSession } from '$/providers/react'
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

  return (
    <>
      <ErrorBoundary
        onReset={async () => {
          toast.error('An unexpected error occurred. You have been signed out.')
          await signOut()
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
