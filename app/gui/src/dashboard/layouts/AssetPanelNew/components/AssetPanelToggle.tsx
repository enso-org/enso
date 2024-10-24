/**
 * @file
 * Toggle for opening the asset panel.
 */
import RightPanelIcon from '#/assets/right_panel.svg'
import { Button } from '#/components/AriaComponents'

import {
  useIsAssetPanelVisible,
  useSetIsAssetPanelPermanentlyVisible,
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

  const isAssetPanelVisible = useIsAssetPanelVisible()
  const setIsAssetPanelPermanentlyVisible = useSetIsAssetPanelPermanentlyVisible()

  const shouldShow = showWhen === 'collapsed' ? !isAssetPanelVisible : isAssetPanelVisible

  const id = useId()

  const { getText } = useText()

  return (
    <AnimatePresence mode="sync">
      {shouldShow && (
        <motion.div className={className} layoutId={`asset-panel-toggle-${id}`}>
          <Button
            size="medium"
            variant="custom"
            isActive={isAssetPanelVisible}
            icon={RightPanelIcon}
            aria-label={getText('openAssetPanel')}
            onPress={() => {
              setIsAssetPanelPermanentlyVisible(!isAssetPanelVisible)
            }}
          />
        </motion.div>
      )}
    </AnimatePresence>
  )
}
