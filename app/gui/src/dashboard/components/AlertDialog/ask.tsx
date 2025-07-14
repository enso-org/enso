/**
 * @file
 *
 * Alert function is a helper function to create an alert dialog,
 * and return a promise that resolves when the user confirms the dialog,
 * or rejects when the user cancels the dialog.
 */

import type { AlertDialogProps, Confirmable } from './AlertDialog'

import { setModal, unsetModal } from '#/providers/ModalProvider'
import type { ComponentType } from 'react'

/**
 * Options for the {@link alert} function.
 */
export interface AlertOptions extends Omit<AlertDialogProps, 'onCancel' | 'onConfirm'> {}

/**
 * The resolution of the alert dialog.
 */
export type Resolution = 'confirm' | 'dismiss'

/**
 * Create an alert dialog and return a promise that resolves when the user confirms the dialog,
 * or rejects when the user cancels the dialog.
 */
export function ask<Component extends ComponentType<Confirmable & P>, P extends object>(
  // eslint-disable-next-line @typescript-eslint/naming-convention
  ConfirmableComponent: Component,
  additionalProps?: P,
) {
  return new Promise<Resolution>((resolve) => {
    const dismiss = () => {
      resolve('dismiss')
    }

    const confirm = () => {
      resolve('confirm')
    }

    // @ts-expect-error For some reason `<ComponentType />` can't be used as an argument to `setModal`.
    setModal(<ConfirmableComponent {...additionalProps} onCancel={dismiss} onConfirm={confirm} />)
  }).finally(unsetModal)
}
