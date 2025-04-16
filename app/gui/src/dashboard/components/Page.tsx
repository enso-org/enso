/** @file A page. */
import * as React from 'react'

import InfoBar from '#/layouts/InfoBar'

import TheModal from '#/components/dashboard/TheModal'
import Portal from '#/components/Portal'

/** Props for a {@link Page}. */
export interface PageProps extends Readonly<React.PropsWithChildren> {
  readonly hideInfoBar?: true
  readonly hideChat?: boolean
}

/** A page. */
export default function Page(props: PageProps) {
  const { hideInfoBar = false, children } = props
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)

  return (
    <>
      {children}
      {!hideInfoBar && (
        <div className="fixed right top z-1 m-2.5 text-primary">
          <InfoBar isHelpChatOpen={isHelpChatOpen} setIsHelpChatOpen={setIsHelpChatOpen} />
        </div>
      )}
      <Portal>
        <div className="select-none text-xs text-primary">
          <TheModal />
        </div>
      </Portal>
    </>
  )
}
