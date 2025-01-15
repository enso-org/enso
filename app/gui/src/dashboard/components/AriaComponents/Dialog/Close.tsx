/**
 * @file
 *
 * Close button for a dialog.
 */

import invariant from 'tiny-invariant'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { type ButtonProps, Button } from '../Button'
import * as dialogProvider from './DialogProvider'

/** Props for {@link Close} component. */
export type CloseProps = ButtonProps

/** Close button for a dialog. */
export function Close(props: CloseProps) {
  const dialogContext = dialogProvider.useDialogContext()

  invariant(dialogContext, 'Close must be used inside a DialogProvider')

  const onPressCallback = useEventCallback<NonNullable<ButtonProps['onPress']>>((event) => {
    dialogContext.close()
    return props.onPress?.(event)
  })

  return <Button {...props} onPress={onPressCallback} />
}
