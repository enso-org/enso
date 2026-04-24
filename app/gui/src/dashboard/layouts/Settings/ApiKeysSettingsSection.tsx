/** @file Settings section for viewing and managing API keys. */
import { Alert } from '#/components/Alert'
import { Cell, Column, Row, Table, TableBody, TableHeader } from '#/components/aria'
import { Button, CopyButton } from '#/components/Button'
import { Dialog, Popover, type DialogProps } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Selector } from '#/components/Inputs/Selector'
import { Scroller } from '#/components/Scroller'
import { Text } from '#/components/Text'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { setModal } from '#/providers/ModalProvider'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends, useText } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { useSuspenseQuery } from '@tanstack/react-query'
import {
  API_KEY_EXPIRES_IN_VALUES,
  ApiKeyExpiresIn,
  type ApiKey,
} from 'enso-common/src/services/Backend'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'

const COLUMN_STYLES =
  'w-full border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0'

/** Settings tab for viewing and managing API keys. */
export function ApiKeySettingsSection() {
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const { data: apiKeys } = useSuspenseQuery(backendQueryOptions(backend, 'listApiKeys', []))
  const apiKeyLimit = useFeatureFlag('apiKeyLimit')
  const apiKeysLeft = apiKeyLimit - apiKeys.length
  const canCreateMoreApiKeys = apiKeysLeft > 0

  return (
    <div className="flex min-h-0 flex-1 flex-col gap-2">
      <Button.Group verticalAlign="center" className="flex-initial">
        <Popover.Trigger>
          <Button isDisabled={!canCreateMoreApiKeys} variant="outline">
            {getText('newApiKey')}
          </Button>
          <Popover size="small" placement="bottom left">
            <NewApiKeyForm />
          </Popover>
        </Popover.Trigger>
        <Text>
          {apiKeysLeft <= 0 ?
            getText('youHaveTheMaximumNumberOfApiKeys')
          : getText('youCanCreateXMoreApiKeys', apiKeysLeft)}
        </Text>
      </Button.Group>
      <Scroller
        scrollbar
        orientation="vertical"
        className="min-h-0 flex-1"
        shadowStartClassName="mt-8"
      >
        <Table
          aria-label={getText('apiKeys')}
          className="max-w-3xl table-fixed self-start rounded-rows"
        >
          <TableHeader className="sticky top-0 z-1 h-row bg-dashboard">
            <Column isRowHeader className={`${COLUMN_STYLES} w-48 min-w-48`}>
              {getText('name')}
            </Column>
            <Column isRowHeader className={`${COLUMN_STYLES} w-80 min-w-80`}>
              {getText('description')}
            </Column>
            <Column isRowHeader className={`${COLUMN_STYLES} w-40 min-w-40`}>
              {getText('createdAt')}
            </Column>
            <Column isRowHeader className={`${COLUMN_STYLES} w-40 min-w-40`}>
              {getText('lastUsedAt')}
            </Column>
            <Column isRowHeader className={`${COLUMN_STYLES} w-40 min-w-40`}>
              {getText('expiresIn')}
            </Column>
            <Column isRowHeader className={COLUMN_STYLES}>
              {getText('actions')}
            </Column>
          </TableHeader>
          <TableBody items={apiKeys} dependencies={[apiKeys]} className="select-text">
            {apiKeys.length === 0 ?
              <Row className="h-10">
                <Cell
                  ref={(el) => {
                    if (!el) {
                      return
                    }
                    // This is SAFE; `react-aria-components` simply is missing types.
                    // This will be unnecessary when the `react-aria-components` dependency is updated as it adds support for `colSpan`.
                    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-magic-numbers
                    ;(el as HTMLTableCellElement).colSpan = 999
                  }}
                  className="px-2.5 placeholder"
                >
                  {getText('youHaveNoApiKeys')}
                </Cell>
              </Row>
            : (apiKey) => <ApiKeyRow apiKey={apiKey} />}
          </TableBody>
        </Table>
      </Scroller>
    </div>
  )
}

/** Props for an {@link ApiKeyRow}. */
interface ApiKeyRowProps {
  /** The API key to display in the row. */
  readonly apiKey: ApiKey
}

/** A row in the {@link ApiKeySettingsSection} table. */
function ApiKeyRow(props: ApiKeyRowProps) {
  const { apiKey } = props
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const deleteApiKey = useMutationCallback(backendMutationOptions(backend, 'deleteApiKey'))

  return (
    <Row className="group h-row rounded-rows-child">
      <Cell className="border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
        {apiKey.name}
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
        {apiKey.description}
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        {toReadableIsoString(new Date(apiKey.createdAt))}
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        {apiKey.lastUsedAt ? toReadableIsoString(new Date(apiKey.lastUsedAt)) : getText('never')}
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        {apiKey.expiresIn !== ApiKeyExpiresIn.Indefinetly && apiKey.expiresAt ?
          toReadableIsoString(new Date(apiKey.expiresAt))
        : getText('never')}
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        <Button.GroupJoin
          className="shrink-0 grow-0"
          buttonVariants={{ size: 'small', variant: 'outline' }}
        >
          <Popover.Trigger>
            <Button icon="trash" className="text-delete">
              {getText('delete')}
            </Button>
            <ConfirmDeleteModal
              actionText={getText('deleteApiKeyConfirmation', apiKey.name)}
              onConfirm={() => deleteApiKey([apiKey.id])}
            />
          </Popover.Trigger>
        </Button.GroupJoin>
      </Cell>
    </Row>
  )
}

/** Props for a {@link ApiKeyDialog}. */
interface ApiKeyDialogProps extends DialogProps {
  readonly apiKey: ApiKey
}

/** Dialog propmpted after successful api key submit. Shows the api key secret to the user. */
function ApiKeyDialog(props: ApiKeyDialogProps) {
  const { apiKey, type = 'modal', ...dialogProps } = props
  const { getText } = useText()

  return (
    <Dialog size="xlarge" type={type} title={getText('keyId')} {...dialogProps}>
      <div className="relative flex items-center gap-4">
        <div className="flex flex-col">
          <Alert variant="outline" icon="warning">
            {getText('accessKeyAlert')}
          </Alert>

          <table>
            <tbody>
              <tr>
                <td>{getText('keyId')}</td>
                <td>{getText('secretId')}</td>
              </tr>
              <tr>
                <td>
                  <CopyButton copyText={apiKey.id} size="small">
                    {apiKey.id}
                  </CopyButton>
                </td>
                <td>
                  <CopyButton copyText={apiKey.secretId ?? ''} size="small">
                    {apiKey.secretId}
                  </CopyButton>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </Dialog>
  )
}

/** A form to create an API key. */
function NewApiKeyForm() {
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const { data: apiKeys } = useSuspenseQuery(backendQueryOptions(backend, 'listApiKeys', []))
  const apiKeyNames = new Set(apiKeys.map((apiKey) => apiKey.name))
  const createApiKey = useMutationCallback(backendMutationOptions(backend, 'createApiKey'))

  return (
    <Form
      schema={(z) =>
        z.object({
          name: z
            .string()
            .min(1)
            .refine((name) => !apiKeyNames.has(name), getText('duplicateApiKeyError')),
          description: z.string(),
          expiresIn: z.nativeEnum(ApiKeyExpiresIn),
        })
      }
      method="dialog"
      onSubmit={(values) => createApiKey([values])}
      onSubmitSuccess={(apiKey) =>
        setModal(<ApiKeyDialog modalProps={{ defaultOpen: true }} apiKey={apiKey} />)
      }
    >
      <Text.Heading variant="subtitle">{getText('newApiKey')}</Text.Heading>
      <Input name="name" label={getText('name')} />
      <Input name="description" label={getText('description')} />
      <Selector items={API_KEY_EXPIRES_IN_VALUES} name="expiresIn" label={getText('expiresIn')} />
      <Button.Group className="relative">
        <Form.Submit />
        <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
      </Button.Group>
      <Form.FormError />
    </Form>
  )
}
