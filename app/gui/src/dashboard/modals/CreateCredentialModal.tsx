/** @file A modal for creating and editing a credential. */
import { Dialog, Dropdown, Text } from '#/components/AriaComponents'
import { CREDENTIAL_INFOS } from '#/data/serviceCredentials'
import { makeCredentialCreationHandler } from '#/data/serviceCredentials/logic'
import { useText } from '#/providers/TextProvider'
import type { CredentialMetadata, SecretId } from '#/services/Backend'
import { useState } from 'react'

/** Props for a {@link CreateCredentialModal}. */
export interface CreateCredentialModalProps {
  readonly noDialog?: boolean
  readonly doCreate: (name: string, value: CredentialMetadata) => Promise<SecretId>
}

/** A modal for creating a credential. */
export default function CreateCredentialModal(props: CreateCredentialModalProps) {
  const { noDialog = false, doCreate } = props
  const { getText } = useText()
  const [selectedChildIndex, setSelectedChildIndex] = useState<number>(0)
  const createCredentialsHandler = makeCredentialCreationHandler(doCreate)

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
      {selectedItem && <selectedItem.form createCredentials={createCredentialsHandler} />}
    </div>
  )

  return noDialog ? content : (
      <Dialog title={getText('newCredential')} isDismissable={false}>
        {content}
      </Dialog>
    )
}
