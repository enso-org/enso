/**
 * @file
 * Toggle for opening the asset panel.
 */
import RightPanelIcon from '#/assets/right_panel.svg'
import { Button } from '#/components/AriaComponents'

import {
  useIsAssetPanelExpanded,
  useIsAssetPanelHidden,
  useIsAssetPanelVisible,
  useSetIsAssetPanelExpanded,
  useSetIsAssetPanelHidden,
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

  const isAssetPanelHidden = useIsAssetPanelHidden()
  const setIsAssetPanelHidden = useSetIsAssetPanelHidden()

  const shouldShow = showWhen === 'collapsed' ? isAssetPanelHidden : !isAssetPanelHidden

  const id = useId()

  const { getText } = useText()

  return (
    <AnimatePresence mode="sync">
      {shouldShow && (
        <motion.div
          className={className}
          layout
          layoutId={`asset-panel-toggle-${id}`}
          initial={{
            opacity: 0,
            filter: 'blur(8px)',
            x: showWhen === 'collapsed' ? 16 : -16,
          }}
          animate={{ opacity: 1, filter: 'blur(0px)', x: 0 }}
          exit={{
            opacity: 0,
            filter: 'blur(4px)',
            x: showWhen === 'collapsed' ? 16 : -16,
          }}
        >
          <Button
            size="medium"
            variant="custom"
            isActive={!isAssetPanelHidden}
            icon={RightPanelIcon}
            aria-label={getText('openAssetPanel')}
            onPress={() => {
              setIsAssetPanelHidden(!isAssetPanelHidden)
            }}
          />
        </motion.div>
      )}
    </AnimatePresence>
  )
}
