/** @file A page. */
import { ErrorBoundary } from '#/components/ErrorBoundary'
import Portal from '#/components/Portal'
import InfoBar from '#/layouts/InfoBar'
import TheModal from '#/pages/dashboard/components/TheModal'
import * as React from 'react'

/** Props for a {@link Page}. */
export interface PageProps extends Readonly<React.PropsWithChildren> {
  readonly hideInfoBar?: true
}

/** A page. */
export default function Page(props: PageProps) {
  const { hideInfoBar = false, children } = props

  return (
    <>
      <ErrorBoundary>{children}</ErrorBoundary>
      {!hideInfoBar && (
        <div className="fixed right top z-1 m-2.5 text-primary">
          <InfoBar />
        </div>
      )}
      <ErrorBoundary>
        <Portal>
          <div className="select-none text-xs text-primary">
            <TheModal />
          </div>
        </Portal>
      </ErrorBoundary>
    </>
  )
}
