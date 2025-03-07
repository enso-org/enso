/** @file A modal for creating and editing a credential. */
import { Dialog, Form, FormDropdown, Input } from '#/components/AriaComponents'
import { CREDENTIAL_INFOS, type CredentialInfo } from '#/data/serviceCredentials'
import { useText } from '#/providers/TextProvider'
import type { SecretId } from '#/services/Backend'
import { openInNewBrowserTab } from '#/utilities/window'

/** Props for a {@link UpsertCredentialModal}. */
export interface UpsertCredentialModalProps {
  readonly noDialog?: boolean
  readonly id: SecretId | null
  readonly name: string | null
  readonly defaultOpen?: boolean
  readonly doCreate: (name: string, type: string, value: unknown) => Promise<SecretId>
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

  const isCreatingCredential = id == null

  const form = Form.useForm({
    method: 'dialog',
    schema: (z) =>
      z.object({
        title: z.string().min(1, getText('emptyStringError')),
        credentialInfo: z.custom<CredentialInfo>(),
      }),
    defaultValues: { title: nameRaw ?? '', credentialInfo: CREDENTIAL_INFOS[0] },
    onSubmit: () => {},
  })
  const title = form.watch('title')
  const credentialInfo = form.watch('credentialInfo')

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
      <FormDropdown
        form={form}
        isRequired
        name="credentialInfo"
        label={getText('credentialTypeLabel')}
        items={CREDENTIAL_INFOS}
      >
        {({ item: { nameId } }) => getText(nameId)}
      </FormDropdown>
      <credentialInfo.component
        isCreating={isCreatingCredential}
        canCancel={canCancel}
        canReset={canReset}
        upsertCredential={async (value, makeAuthorizeUrl) => {
          const secretId = await doCreate(title, credentialInfo.credentialType, value)
          const authorizeUrl = makeAuthorizeUrl(secretId)
          openInNewBrowserTab(authorizeUrl)
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
