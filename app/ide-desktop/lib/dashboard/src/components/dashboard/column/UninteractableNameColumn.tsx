/** @file The icon and name of an {@link backendModule.AnyAsset}. */
import * as React from 'react'

import * as assetRowUtils from '#/components/dashboard/AssetRow/assetRowUtils'
import type * as column from '#/components/dashboard/column'
import NameColumn from '#/components/dashboard/column/NameColumn'

import * as backendModule from '#/services/Backend'

// ================================
// === UninteractableNameColumn ===
// ================================

/** Props for a {@link NameColumn}. */
export interface UninteractableNameColumnProps
  extends Omit<
    column.AssetColumnProps,
    'isSoleSelected' | 'rowState' | 'selected' | 'setItem' | 'setRowState' | 'setSelected'
  > {}

/** The icon and name of an {@link backendModule.AnyAsset}.
 * All state in this variant is replaced with default state;
 * all callbacks are replaced with no-ops. */
export default function UninteractableNameColumn(props: UninteractableNameColumnProps) {
  return (
    <NameColumn
      {...props}
      // Default states.
      isSoleSelected={false}
      selected={false}
      rowState={assetRowUtils.INITIAL_ROW_STATE}
      // The drag placeholder cannot be interacted with.
      setSelected={() => {}}
      setItem={() => {}}
      setRowState={() => {}}
    />
  )
}
