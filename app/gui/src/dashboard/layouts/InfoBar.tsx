/** @file A toolbar containing chat and the user menu. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import { InfoMenu } from '#/layouts/InfoMenu'
import { useText } from '$/providers/react'
import { memo } from 'react'

/** A toolbar containing chat and the user menu. */
function InfoBar() {
  const { getText } = useText()

  return (
    <div className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame backdrop-blur-default">
      <Dialog.Trigger>
        <Button
          size="custom"
          variant="icon"
          className="flex size-row-h select-none items-center overflow-clip rounded-full"
        >
          <Icon
            icon="enso_logo"
            alt={getText('openInfoMenu')}
            className="pointer-events-none size-7"
          />
        </Button>
        <InfoMenu />
      </Dialog.Trigger>
    </div>
  )
}

export default memo(InfoBar)
