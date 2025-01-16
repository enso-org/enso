/** @file Modal for confirming delete of any type of asset. */
import { ButtonGroup, Dialog, DialogDismiss, Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { SecretId } from '#/services/Backend'

// =========================
// === UpsertSecretModal ===
// =========================

/** Props for a {@link UpsertSecretModal}. */
export interface UpsertSecretModalProps {
  readonly noDialog?: boolean
  readonly id: SecretId | null
  readonly name: string | null
  readonly defaultOpen?: boolean
  readonly doCreate: (name: string, value: string) => Promise<void> | void
  /** Defaults to `true`. */
  readonly canCancel?: boolean
  /** Defaults to `false`. */
  readonly canReset?: boolean
}

/** A modal for creating and editing a secret. */
export default function UpsertSecretModal(props: UpsertSecretModalProps) {
  const { noDialog = false, id, name: nameRaw, defaultOpen, doCreate } = props
  const { canCancel = true, canReset = false } = props
  const { getText } = useText()

  const isCreatingSecret = id == null

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
        <Form.Submit>{isCreatingSecret ? getText('create') : getText('update')}</Form.Submit>
        {canCancel && <DialogDismiss />}
        {canReset && <Form.Reset>{getText('cancel')}</Form.Reset>}
      </ButtonGroup>
    </Form>
  )

  return noDialog ? content : (
      <Dialog
        title={isCreatingSecret ? getText('newSecret') : getText('editSecret')}
        modalProps={defaultOpen == null ? {} : { defaultOpen }}
        isDismissable={false}
      >
        {content}
      </Dialog>
    )
}
