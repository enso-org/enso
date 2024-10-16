/** @file Modal for confirming delete of any type of asset. */
import { ButtonGroup, Dialog, Form, INPUT_STYLES, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { SecretId } from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'

// =========================
// === UpsertSecretModal ===
// =========================

const CLASSIC_INPUT_STYLES = tv({
  extend: INPUT_STYLES,
  slots: {
    base: '',
    textArea: 'rounded-full border-0.5 border-primary/20 px-1.5',
    inputContainer: 'before:h-0 after:h-0.5',
  },
})

const CLASSIC_FIELD_STYLES = tv({
  extend: Form.FIELD_STYLES,
  slots: {
    base: '',
    label: 'px-2',
  },
})

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
  const isNameEditable = nameRaw == null

  const form = Form.useForm({
    method: 'dialog',
    schema: (z) =>
      z.object({ name: z.string().min(1, getText('emptyStringError')), value: z.string() }),
    defaultValues: { name: nameRaw ?? '', value: '' },
    onSubmit: async ({ name, value }) => {
      await doCreate(name, value)
    },
  })

  const content = (
    <Form form={form} testId="upsert-secret-modal" gap="none" className="w-full">
      {isNameEditable && (
        <Input
          form={form}
          name="name"
          size="custom"
          rounded="full"
          autoFocus={isNameEditable}
          autoComplete="off"
          isDisabled={!isNameEditable}
          label={getText('name')}
          placeholder={getText('secretNamePlaceholder')}
          variants={CLASSIC_INPUT_STYLES}
          fieldVariants={CLASSIC_FIELD_STYLES}
        />
      )}
      <Input
        form={form}
        name="value"
        type="password"
        size="custom"
        rounded="full"
        autoFocus={!isNameEditable}
        autoComplete="off"
        label={getText('value')}
        placeholder={
          isNameEditable ? getText('secretValuePlaceholder') : getText('secretValueHidden')
        }
        variants={CLASSIC_INPUT_STYLES}
        fieldVariants={CLASSIC_FIELD_STYLES}
      />
      <ButtonGroup className="mt-2">
        <Form.Submit>{isCreatingSecret ? getText('create') : getText('update')}</Form.Submit>
        {canCancel && <Form.Submit action="cancel" />}
        {canReset && <Form.Reset action="cancel" />}
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
