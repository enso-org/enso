/** @file Close button for a dialog. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { type ButtonProps, Button } from '../Button'
import { useDialogStrictContext } from './hooks'

/** Props for {@link Close} component. */
export type CloseProps<IconType extends string> = ButtonProps<IconType>

/** Close button for a dialog. */
export function Close<IconType extends string>(props: CloseProps<IconType>) {
  const dialogContext = useDialogStrictContext()

  const onPressCallback = useEventCallback<NonNullable<ButtonProps<IconType>['onPress']>>(
    (event) => {
      dialogContext.close()
      return props.onPress?.(event)
    },
  )

  return <Button {...props} onPress={onPressCallback} />
}
