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
export type CloseProps<IconType extends string> = ButtonProps<IconType>

/** Close button for a dialog. */
export function Close<IconType extends string>(props: CloseProps<IconType>) {
  const dialogContext = dialogProvider.useDialogContext()

  invariant(dialogContext, 'Close must be used inside a DialogProvider')

  const onPressCallback = useEventCallback<NonNullable<ButtonProps<IconType>['onPress']>>((event) => {
    dialogContext.close()
    return props.onPress?.(event)
  })

  return <Button {...props} onPress={onPressCallback} />
}
