/** @file A modal for creating and editing a credential. */
import { uuidv4 } from 'lib0/random.js'
import { Dialog, Dropdown, Text } from '#/components/AriaComponents'
import { CREDENTIAL_INFOS, type CredentialInfo } from '#/data/serviceCredentials'
import { CredentialsFormButtons } from '#/data/serviceCredentials/CredentialsFormButtons'
import { useText } from '#/providers/TextProvider'
import type { CredentialMetadata, SecretId } from '#/services/Backend'
import { openInNewBrowserTab } from '#/utilities/window'
import { useState } from 'react'

/** Props for a {@link CreateCredentialModal}. */
export interface CreateCredentialModalProps {
  readonly noDialog?: boolean,
  readonly doCreate: (name: string, value: CredentialMetadata) => Promise<SecretId>
}

/** A modal for creating a credential. */
export default function CreateCredentialModal(props: CreateCredentialModalProps) {
  const {
    noDialog = false,
    doCreate
  } = props
  const { getText } = useText()

  const _todo = doCreate
/*
    onSubmit: (values, submittedForm) => {
      const nonce = uuidv4()
      console.log("Submitting", values, submittedForm, nonce)
      const secretId = doCreate(values.title, undefined as unknown as CredentialMetadata)
      // const authorizeUrl = makeAuthorizeUrl(secretId)
      // openInNewBrowserTab(authorizeUrl)
    },
*/
  const [selectedChildIndex, setSelectedChildIndex] = useState<number>(0)

  const selectedItem = CREDENTIAL_INFOS[selectedChildIndex]
  const content = (
    <div className="w-full">
      <Dropdown
        aria-label={getText('credentialTypeLabel')}
        items={CREDENTIAL_INFOS}
        selectedIndex={selectedChildIndex}
        className="w-full self-start"
        onChange={(_childSchema, index) => {
          setSelectedChildIndex(index)
        }}
      >
        {({ item }) => <Text slot="label">{getText(item.nameId)}</Text>}
      </Dropdown>
      {selectedItem && (<selectedItem.form />)}
    </div>
  )

  return noDialog ? content : (
      <Dialog
        title={getText('newCredential')}
        isDismissable={false}
      >
        {content}
      </Dialog>
    )
}
