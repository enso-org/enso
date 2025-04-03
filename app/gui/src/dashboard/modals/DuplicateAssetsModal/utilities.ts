/** @file Utilities for `DuplicateAssetsModal`. */
import { setModal, unsetModal } from '#/providers/ModalProvider'
import { createElement } from 'react'
import {
  ResolveDuplicationsModal,
  type ResolvedDuplication,
  type ResolveDuplicationsProps,
} from './DuplicateAssetsModal'

/** Options for resolving duplicates. */
export interface ResolveDuplicationsOptions
  extends Pick<ResolveDuplicationsProps, 'conflictingIds' | 'targetId'> {}

/** Function for resolving duplicates. */
export async function resolveDuplications(props: ResolveDuplicationsOptions) {
  const { targetId, conflictingIds } = props

  return new Promise<readonly ResolvedDuplication[]>((resolve, reject) => {
    setModal(
      createElement(ResolveDuplicationsModal, {
        targetId,
        conflictingIds,
        onSubmit: resolve,
        onCancel: reject,
      }),
    )
  }).finally(unsetModal)
}
