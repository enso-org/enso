/**
 * @file
 *
 * Alert dialogs are used to confirm or cancel an action.
 */

import { useText } from '$/providers/react'
import type { ZodEnum, ZodObject } from 'zod'
import { Button } from '../Button'
import { Dialog, type DialogProps } from '../Dialog'
import { Form, type SubmitProps, type TSchema } from '../Form'
import { Text } from '../Text'
import { AlertDialogProvider } from './AlertDialogProvider'

/**
 * Props for the children of the {@link AlertDialog} component.
 */
interface AlertDialogChildrenRenderProps {
  readonly confirm: () => Promise<void> | void
  readonly dismiss: () => Promise<void> | void
}

/**
 * An arbitrary component that can be used to confirm or cancel an action.
 */
export interface Confirmable {
  readonly onConfirm?: (() => Promise<void> | void) | undefined
  readonly onCancel?: (() => Promise<void> | void) | undefined
}

/**
 * Props for the {@link AlertDialog} component.
 */
export interface AlertDialogProps extends Omit<DialogProps, 'children'>, Confirmable {
  readonly children: React.ReactNode | ((props: AlertDialogChildrenRenderProps) => React.ReactNode)
  readonly title: string
  readonly message?: string
  readonly isDestructive?: boolean
  readonly confirm?:
    | React.ReactElement<AlertDialogDismissProps<string>>
    | string
    | (() => React.ReactElement<AlertDialogDismissProps<string>> | string | null | undefined)
    | null
    | undefined
  readonly cancel?:
    | React.ReactElement<AlertDialogConfirmProps<string>>
    | string
    | (() => React.ReactElement<AlertDialogConfirmProps<string>> | string | null | undefined)
    | null
    | undefined
}

/**
 * A dialog that is used to confirm or cancel an action.
 */
export function AlertDialog(props: AlertDialogProps) {
  const { getText } = useText()

  const { confirm = getText('confirm'), cancel = getText('cancel') } = props

  const { title, onConfirm, onCancel, isDestructive = false, children, ...rest } = props

  const cancelButton = typeof cancel === 'function' ? cancel() : cancel
  const confirmButton = typeof confirm === 'function' ? confirm() : confirm

  return (
    <Dialog
      role="alertdialog"
      size="small"
      isKeyboardDismissDisabled
      isDismissable={false}
      title={title}
      closeButton="none"
      {...rest}
    >
      <AlertDialogProvider isDestructive={isDestructive}>
        <Form
          schema={(z) => z.object({ response: z.enum(['confirm', 'cancel']) })}
          onSubmit={({ response }) => {
            if (response === 'confirm') {
              return onConfirm?.()
            }

            return onCancel?.()
          }}
          method="dialog"
        >
          <AlertDialogBody children={children} />

          <Button.Group align="end">
            {typeof cancelButton === 'string' ?
              <DismissAlertDialog isLoading={false} variant="ghost">
                {cancelButton}
              </DismissAlertDialog>
            : cancelButton}

            {typeof confirmButton === 'string' ?
              <ConfirmAlertDialog>{confirmButton}</ConfirmAlertDialog>
            : confirmButton}
          </Button.Group>

          <Form.FormError />
        </Form>
      </AlertDialogProvider>
    </Dialog>
  )
}

/**
 * Props for the {@link AlertDialog.Body} component.
 */
interface AlertDialogBodyProps {
  readonly children: AlertDialogProps['children']
}

/**
 * A body for the {@link AlertDialog} component.
 */
function AlertDialogBody(props: AlertDialogBodyProps) {
  const { children } = props
  const { dismiss, confirm } = AlertDialogProvider.useContextStrict()

  const renderChildren = typeof children === 'function' ? children({ dismiss, confirm }) : children

  return typeof renderChildren === 'string' ? <Text>{renderChildren}</Text> : renderChildren
}

/** Props for the {@link DismissAlertDialog} component. */
export type AlertDialogDismissProps<IconType extends string> = SubmitProps<
  IconType,
  TSchema<ZodObject<{ response: ZodEnum<['cancel']> }>>,
  'response',
  unknown
>

/** A button that dismisses an alert dialog. */
function DismissAlertDialog<IconType extends string>(props: AlertDialogDismissProps<IconType>) {
  const form = Form.useFormContext(props.form)

  return (
    <Form.Submit
      name="response"
      value="cancel"
      variant="ghost"
      isLoading={false}
      isDisabled={form.formState.isSubmitting}
      {...props}
    />
  )
}

/**
 * Props for the {@link AlertDialog.Confirm} component.
 */
export type AlertDialogConfirmProps<IconType extends string> = SubmitProps<
  IconType,
  TSchema<ZodObject<{ response: ZodEnum<['confirm']> }>>,
  'response',
  unknown
> & {
  readonly variant?: 'delete' | 'primary'
}

/** A button that confirms an alert dialog. */
function ConfirmAlertDialog<IconType extends string>(props: AlertDialogConfirmProps<IconType>) {
  const { isDestructive } = AlertDialogProvider.useContextStrict()
  const { variant = isDestructive ? 'delete' : 'primary', ...rest } = props

  return <Form.Submit autoFocus name="response" value="confirm" variant={variant} {...rest} />
}

AlertDialog.Trigger = Dialog.Trigger
