/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import SettingsIcon from 'enso-assets/settings.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import Button from '#/components/Button'

import * as backendModule from '#/services/Backend'

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps {
  /** When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`. */
  readonly invisible?: boolean
  readonly isAssetPanelEnabled: boolean
  readonly setIsAssetPanelEnabled: React.Dispatch<React.SetStateAction<boolean>>
}

/** A menubar for displaying asset information. */
// This parameter will be used in the future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export default function AssetInfoBar(props: AssetInfoBarProps) {
  const { invisible = false, isAssetPanelEnabled, setIsAssetPanelEnabled } = props
  const { backend } = backendProvider.useBackend()
  const rootRef = React.useRef<HTMLDivElement>(null)
  const navigator2D = navigator2DProvider.useNavigator2D()

  React.useEffect(() => {
    const root = rootRef.current
    if (invisible || root == null) {
      return
    } else {
      navigator2D.register(root)
      return () => {
        navigator2D.unregister(root)
      }
    }
  }, [invisible, navigator2D])

  return (
    <div
      ref={rootRef}
      className={`pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-icons rounded-full bg-frame px-icons-x ${
        backend.type === backendModule.BackendType.remote ? '' : 'invisible'
      }`}
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <Button
        alt={isAssetPanelEnabled ? 'Close Asset Panel' : 'Open Asset Panel'}
        active={isAssetPanelEnabled}
        image={SettingsIcon}
        error="Select exactly one asset to see its settings."
        onClick={() => {
          setIsAssetPanelEnabled(visible => !visible)
        }}
      />
    </div>
  )
}
