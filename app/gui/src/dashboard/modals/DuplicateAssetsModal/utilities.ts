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
  extends Omit<ResolveDuplicationsProps, 'onCancel' | 'onSubmit'> {}

/** Function for resolving duplicates. */
export async function resolveDuplications(props: ResolveDuplicationsOptions) {
  return new Promise<readonly ResolvedDuplication[]>((resolve, reject) => {
    setModal(
      createElement(ResolveDuplicationsModal, {
        onSubmit: resolve,
        onCancel: reject,
        ...props,
      }),
    )
  }).finally(unsetModal)
}
