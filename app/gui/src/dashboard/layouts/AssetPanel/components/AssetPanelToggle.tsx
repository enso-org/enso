/**
 * @file
 * Toggle for opening the asset panel.
 */
import RightPanelIcon from '#/assets/right_panel.svg'
import { Button } from '#/components/AriaComponents'

import { useText } from '#/providers/TextProvider'
import { AnimatePresence, motion } from 'framer-motion'
import { memo } from 'react'
import { useIsAssetPanelHidden, useSetIsAssetPanelHidden } from '../AssetPanelState'

import { useEventCallback } from '#/hooks/eventCallbackHooks'

/**
 * Props for a {@link AssetPanelToggle}.
 */
export interface AssetPanelToggleProps {
  readonly className?: string
  readonly showWhen?: 'collapsed' | 'expanded'
  readonly getTranslation?: () => number
}

const COLLAPSED_X_TRANSLATION = 16

/**
 * Toggle for opening the asset panel.
 */
export const AssetPanelToggle = memo(function AssetPanelToggle(props: AssetPanelToggleProps) {
  const {
    className,
    showWhen = 'collapsed',
    getTranslation = () => COLLAPSED_X_TRANSLATION,
  } = props

  const { getText } = useText()
  const isAssetPanelHidden = useIsAssetPanelHidden()
  const setIsAssetPanelHidden = useSetIsAssetPanelHidden()

  const canDisplay = showWhen === 'collapsed' ? isAssetPanelHidden : !isAssetPanelHidden

  const toggleAssetPanel = useEventCallback(() => {
    setIsAssetPanelHidden(!isAssetPanelHidden)
  })

  return (
    <AnimatePresence initial={!canDisplay} mode="sync">
      {canDisplay && (
        <motion.div
          className={className}
          layout="position"
          initial={{
            opacity: 0,
            filter: 'blur(4px)',
            x: showWhen === 'collapsed' ? getTranslation() : -getTranslation(),
          }}
          animate={{ opacity: 1, filter: 'blur(0px)', x: 0 }}
          exit={{
            opacity: 0,
            filter: 'blur(4px)',
            x: showWhen === 'collapsed' ? getTranslation() : -getTranslation(),
          }}
        >
          <Button
            size="medium"
            variant="custom"
            isActive={!isAssetPanelHidden}
            icon={RightPanelIcon}
            aria-label={getText('openAssetPanel')}
            onPress={toggleAssetPanel}
          />
        </motion.div>
      )}
    </AnimatePresence>
  )
})
