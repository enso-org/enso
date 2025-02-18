/** @file A modal for creating and editing a credential. */
import { ButtonGroup, Dialog, DialogDismiss, Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { CredentialId } from '#/services/Backend'

/** Props for a {@link UpsertCredentialModal}. */
export interface UpsertCredentialModalProps {
  readonly noDialog?: boolean
  readonly id: CredentialId | null
  readonly name: string | null
  readonly defaultOpen?: boolean
  readonly doCreate: (name: string, value: string) => Promise<void> | void
  /** Defaults to `true`. */
  readonly canCancel?: boolean
  /** Defaults to `false`. */
  readonly canReset?: boolean
}

/** A modal for creating and editing a credential. */
export default function UpsertCredentialModal(props: UpsertCredentialModalProps) {
  const { noDialog = false, id, name: nameRaw, defaultOpen, doCreate } = props
  const { canCancel = true, canReset = false } = props
  const { getText } = useText()

  const isCreatingCredential = id == null

  const form = Form.useForm({
    method: 'dialog',
    schema: (z) =>
      z.object({ title: z.string().min(1, getText('emptyStringError')), value: z.string() }),
    defaultValues: { title: nameRaw ?? '', value: '' },
    onSubmit: async ({ title, value }) => {
      await doCreate(title, value)
      form.reset({ title, value })
    },
  })

  const content = (
    <Form form={form} testId="upsert-secret-modal" gap="none" className="w-full">
      <Input
        form={form}
        name="title"
        autoFocus
        autoComplete="off"
        label={getText('name')}
        placeholder={getText('secretNamePlaceholder')}
      />
      <Input
        form={form}
        name="value"
        type="password"
        autoComplete="off"
        label={getText('value')}
        placeholder={
          nameRaw == null ? getText('secretValuePlaceholder') : getText('secretValueHidden')
        }
      />
      <ButtonGroup className="mt-2">
        <Form.Submit>{isCreatingCredential ? getText('create') : getText('update')}</Form.Submit>
        {canCancel && <DialogDismiss />}
        {canReset && <Form.Reset>{getText('cancel')}</Form.Reset>}
      </ButtonGroup>
    </Form>
  )

  return noDialog ? content : (
      <Dialog
        title={isCreatingCredential ? getText('newCredential') : getText('editCredential')}
        modalProps={defaultOpen == null ? {} : { defaultOpen }}
        isDismissable={false}
      >
        {content}
      </Dialog>
    )
}
