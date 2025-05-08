/** @file A modal for creating and editing a credential. */
import { Dialog, Dropdown, Text } from '#/components/AriaComponents'
import { CREDENTIAL_INFOS } from '#/data/serviceCredentials'
import { makeCredentialCreationHandler } from '#/data/serviceCredentials/logic'
import type { CredentialConfig, SecretId } from '#/services/Backend'
import { useText } from '$/providers/react'
import invariant from 'tiny-invariant'
import { z } from 'zod'
import { useSearchParamsState } from '../hooks/searchParamsStateHooks'

/** Props for a {@link CreateCredentialForm}. */
export interface CreateCredentialFormProps {
  readonly doCreate: (name: string, value: CredentialConfig) => Promise<SecretId>
}

/** A modal for creating a credential. */
export function CreateCredentialForm(props: CreateCredentialFormProps) {
  const { doCreate } = props
  const { getText } = useText()
  const [selectedChildIndex, setSelectedChildIndex] = useSearchParamsState(
    'credentialsModal_service',
    CREDENTIAL_INFOS[0].credentialType,
    // eslint-disable-next-line no-restricted-syntax
    z.enum(CREDENTIAL_INFOS.map((info) => info.credentialType) as [string, ...string[]]),
  )
  const createCredentialsHandler = makeCredentialCreationHandler(doCreate)

  const selectedIndex = CREDENTIAL_INFOS.findIndex(
    (info) => info.credentialType === selectedChildIndex,
  )
  const selectedItem = CREDENTIAL_INFOS[selectedIndex]

  return (
    <>
      <Dropdown
        aria-label={getText('credentialTypeLabel')}
        items={CREDENTIAL_INFOS}
        selectedIndex={selectedIndex}
        className="w-full self-start"
        onChange={(_childSchema, index) => {
          const nextItem = CREDENTIAL_INFOS[index]
          invariant(nextItem != null, 'Invalid index')
          setSelectedChildIndex(nextItem.credentialType)
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
