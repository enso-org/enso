/** @file A toolbar containing chat and the user menu. */
import { Button, DialogTrigger } from '#/components/AriaComponents'
import { Icon } from '#/components/Icon'
import InfoMenu from '#/layouts/InfoMenu'
import { useText } from '#/providers/TextProvider'
import { memo } from 'react'

/** A toolbar containing chat and the user menu. */
function InfoBar() {
  const { getText } = useText()

  return (
    <div className="pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-user-bar rounded-full bg-frame backdrop-blur-default">
      <DialogTrigger>
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
      </DialogTrigger>
      {/* Required for shortcuts to work. */}
      <div className="hidden">
        <InfoMenu hidden />
      </div>
    </div>
  )
}

export default memo(InfoBar)
