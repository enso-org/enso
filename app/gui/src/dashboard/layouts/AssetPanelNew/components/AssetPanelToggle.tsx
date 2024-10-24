/**
 * @file
 * Toggle for opening the asset panel.
 */
import RightPanelIcon from '#/assets/right_panel.svg'
import { Button } from '#/components/AriaComponents'

import {
  useDriveStore,
  useIsAssetPanelVisible,
  useSetIsAssetPanelPermanentlyVisible,
  useSetIsAssetPanelTemporarilyVisible,
} from '#/providers/DriveProvider'
import { useText } from '#/providers/TextProvider'
import { AnimatePresence, motion } from 'framer-motion'
import { useId } from 'react'

/**
 * Props for a {@link AssetPanelToggle}.
 */
export interface AssetPanelToggleProps {
  readonly className?: string
  readonly showWhen?: 'collapsed' | 'expanded'
}

/**
 * Toggle for opening the asset panel.
 */
export function AssetPanelToggle(props: AssetPanelToggleProps) {
  const { className, showWhen = 'collapsed' } = props

  const driveStore = useDriveStore()
  const isAssetPanelVisible = useIsAssetPanelVisible()
  const setIsAssetPanelPermanentlyVisible = useSetIsAssetPanelPermanentlyVisible()
  const setIsAssetPanelTemporarilyVisible = useSetIsAssetPanelTemporarilyVisible()

  const shouldShow = showWhen === 'collapsed' ? !isAssetPanelVisible : isAssetPanelVisible

  const id = useId()

  const { getText } = useText()

  return (
    <AnimatePresence mode="sync">
      {shouldShow && (
        <motion.div
          className={className}
          layout
          layoutId={`asset-panel-toggle-${id}`}
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
        >
          <Button
            size="medium"
            variant="custom"
            isActive={isAssetPanelVisible}
            icon={RightPanelIcon}
            aria-label={getText('openAssetPanel')}
            onPress={() => {
              const isAssetPanelTemporarilyVisible =
                driveStore.getState().isAssetPanelTemporarilyVisible
              if (isAssetPanelTemporarilyVisible) {
                setIsAssetPanelTemporarilyVisible(false)
                setIsAssetPanelPermanentlyVisible(false)
              } else {
                setIsAssetPanelPermanentlyVisible(!isAssetPanelVisible)
              }
            }}
          />
        </motion.div>
      )}
    </AnimatePresence>
  )
}
