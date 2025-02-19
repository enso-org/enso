/** @file A modal for creating and editing a credential. */
import { Dialog, Dropdown, Form, Input } from '#/components/AriaComponents'
import { CREDENTIAL_INFOS } from '#/data/serviceCredentials'
import { useText } from '#/providers/TextProvider'
import type { SecretId } from '#/services/Backend'
import { useState } from 'react'

/** Props for a {@link UpsertCredentialModal}. */
export interface UpsertCredentialModalProps {
  readonly noDialog?: boolean
  readonly id: SecretId | null
  readonly name: string | null
  readonly defaultOpen?: boolean
  readonly doCreate: (name: string, type: string, value: unknown) => Promise<void> | void
  /** Defaults to `true`. */
  readonly canCancel?: boolean
  /** Defaults to `false`. */
  readonly canReset?: boolean
}

/** A modal for creating and editing a credential. */
export default function UpsertCredentialModal(props: UpsertCredentialModalProps) {
  const {
    noDialog = false,
    id,
    name: nameRaw,
    defaultOpen,
    doCreate,
    canCancel = true,
    canReset = false,
  } = props
  const { getText } = useText()
  const [credentialInfo, setCredentialType] = useState(CREDENTIAL_INFOS[0])

  const isCreatingCredential = id == null

  const form = Form.useForm({
    method: 'dialog',
    schema: (z) => z.object({ title: z.string().min(1, getText('emptyStringError')) }),
    defaultValues: { title: nameRaw ?? '' },
    onSubmit: () => {},
  })
  const title = form.watch('title')

  const content = (
    <Form form={form} testId="upsert-credential-modal" gap="none" className="w-full">
      <Input
        form={form}
        name="title"
        autoFocus
        autoComplete="off"
        label={getText('name')}
        placeholder={getText('credentialNamePlaceholder')}
      />
      <Dropdown
        items={CREDENTIAL_INFOS}
        selectedIndex={CREDENTIAL_INFOS.indexOf(credentialInfo)}
        onChange={setCredentialType}
      >
        {({ item: { nameId } }) => getText(nameId)}
      </Dropdown>
      <credentialInfo.component
        isCreating={isCreatingCredential}
        canCancel={canCancel}
        canReset={canReset}
        upsertCredential={async (value) => {
          await doCreate(title, credentialInfo.credentialType, value)
        }}
      />
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
