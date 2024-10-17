/**
 * @file
 *
 * Close button for a dialog.
 */
import * as React from 'react'

import invariant from 'tiny-invariant'

import * as eventCallback from '#/hooks/eventCallbackHooks'

import * as button from '../Button'
import * as dialogProvider from './DialogProvider'

/** Props for {@link Close} component. */
export type CloseProps = button.ButtonProps

/** Close button for a dialog. */
export function Close(props: CloseProps) {
  const dialogContext = dialogProvider.useDialogContext()

  invariant(dialogContext, 'Close must be used inside a DialogProvider')

  const onPressCallback = eventCallback.useEventCallback<
    NonNullable<button.ButtonProps['onPress']>
  >((event) => {
    dialogContext.close()
    return props.onPress?.(event)
  })

  return <button.Button {...props} onPress={onPressCallback} />
}
