/** @file Local storage keys for the asset panel. */
import { ASSET_PANEL_TABS } from '#/layouts/AssetPanel/types'
import { defineLocalStorageKey } from '#/providers/LocalStorageProvider'

export const { key: storedAssetPanelTab } = defineLocalStorageKey('assetPanelTab', {
  schema: (z) => z.enum(ASSET_PANEL_TABS),
})

export const { key: storedAssetPanelWidth } = defineLocalStorageKey('assetPanelWidth', {
  schema: (z) => z.number().int(),
})

export const { key: storedIsAssetPanelHidden } = defineLocalStorageKey('isAssetPanelHidden', {
  schema: (z) => z.boolean(),
})

export const { key: storedIsAssetPanelVisible } = defineLocalStorageKey('isAssetPanelVisible', {
  schema: (z) => z.boolean(),
})
