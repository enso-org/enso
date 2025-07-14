/** @file A modal for creating and editing a credential. */
import { Dialog } from '#/components/Dialog'
import { Dropdown } from '#/components/Inputs/Dropdown'
import { Text } from '#/components/Text'
import { CREDENTIAL_INFOS } from '#/data/serviceCredentials'
import { makeCredentialCreationHandler } from '#/data/serviceCredentials/logic'
import type { CredentialConfig, SecretId } from '#/services/Backend'
import { useText } from '$/providers/react'
import { useState } from 'react'

/** Props for a {@link CreateCredentialForm}. */
export interface CreateCredentialFormProps {
  readonly doCreate: (name: string, value: CredentialConfig) => Promise<SecretId>
}

/** A modal for creating a credential. */
export function CreateCredentialForm(props: CreateCredentialFormProps) {
  const { doCreate } = props
  const { getText } = useText()
  const [selectedChildIndex, setSelectedChildIndex] = useState<number>(0)
  const createCredentialsHandler = makeCredentialCreationHandler(doCreate)

  const selectedItem = CREDENTIAL_INFOS[selectedChildIndex]

  return (
    <>
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
    </>
  )
}

/** Props for a {@link CreateCredentialModal}. */
export interface CreateCredentialModalProps extends CreateCredentialFormProps {}

/** A modal for creating a credential. */
export function CreateCredentialModal(props: CreateCredentialModalProps) {
  const { getText } = useText()

  return (
    <Dialog title={getText('newCredential')} isDismissable={false}>
      <CreateCredentialForm {...props} />
    </Dialog>
  )
}
