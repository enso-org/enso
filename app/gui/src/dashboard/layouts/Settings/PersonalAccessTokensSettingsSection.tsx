/** @file Settings tab for viewing and editing roles for all users in the organization. */
import { Cell, Column, Row, Table, TableBody, TableHeader } from '#/components/aria'
import { Button } from '#/components/Button'
import { Dialog, Popover } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Scroller } from '#/components/Scroller'
import { Text } from '#/components/Text'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import type { PersonalAccessToken } from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends, useText } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'

const PERSONAL_ACCESS_TOKEN_SETTINGS_SECTION_STYLES = tv({
  base: '',
  slots: {
    tableContainer: 'min-h-0 flex-1',
    table: 'max-w-3xl table-fixed self-start rounded-rows',
    column:
      'w-full border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0',
  },
})

/** Settings tab for viewing and editing organization teams. */
export function PersonalAccessTokensSettingsSection() {
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const { data: personalAccessTokens } = useSuspenseQuery(
    backendQueryOptions(backend, 'listPersonalAccessTokens', []),
  )

  const styles = PERSONAL_ACCESS_TOKEN_SETTINGS_SECTION_STYLES({})

  return (
    <div className="flex min-h-0 flex-1 flex-col gap-2">
      <Button.Group verticalAlign="center" className="flex-initial">
        <Popover.Trigger>
          <Button variant="outline">{getText('newPersonalAccessToken')}</Button>
          <Popover size="small" placement="bottom left">
            <NewPersonalAccessTokenForm />
          </Popover>
        </Popover.Trigger>
      </Button.Group>
      <Scroller
        scrollbar
        orientation="vertical"
        className={styles.tableContainer()}
        shadowStartClassName="mt-8"
      >
        <Table aria-label={getText('personalAccessTokens')} className={styles.table()}>
          <TableHeader className="sticky top-0 z-1 h-row bg-dashboard">
            <Column isRowHeader className={styles.column({ className: 'w-48 min-w-48' })}>
              {getText('name')}
            </Column>
            <Column isRowHeader className={styles.column({ className: 'w-40 min-w-40' })}>
              {getText('createdAt')}
            </Column>
            <Column isRowHeader className={styles.column({ className: 'w-40 min-w-40' })}>
              {getText('lastUsedAt')}
            </Column>
            <Column isRowHeader className={styles.column()}>
              {getText('actions')}
            </Column>
          </TableHeader>
          <TableBody
            items={personalAccessTokens}
            dependencies={[personalAccessTokens]}
            className="select-text"
          >
            {personalAccessTokens.length === 0 ?
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
                  {getText('youHaveNoPersonalAccessTokens')}
                </Cell>
              </Row>
            : (personalAccessToken) => (
                <PersonalAccessTokenRow personalAccessToken={personalAccessToken} />
              )
            }
          </TableBody>
        </Table>
      </Scroller>
    </div>
  )
}

/** Props for a {@link PersonalAccessTokenRow}. */
interface PersonalAccessTokenRowProps {
  /** The personal access token to display in the row. */
  readonly personalAccessToken: PersonalAccessToken
}

/** A row in the {@link PersonalAccessTokensSettingsSection} table. */
function PersonalAccessTokenRow(props: PersonalAccessTokenRowProps) {
  const { personalAccessToken } = props
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const deletePersonalAccessToken = useMutationCallback(
    backendMutationOptions(backend, 'deletePersonalAccessToken'),
  )

  return (
    <Row className="group h-row rounded-rows-child">
      <Cell className="min-w-48 max-w-80 border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
        {personalAccessToken.name}
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        {toReadableIsoString(new Date(personalAccessToken.createdAt))}
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        {personalAccessToken.lastUsedAt ?
          toReadableIsoString(new Date(personalAccessToken.lastUsedAt))
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
              actionText={getText(
                'deletePersonalAccessTokenConfirmation',
                personalAccessToken.name,
              )}
              onConfirm={() => deletePersonalAccessToken([personalAccessToken.id])}
            />
          </Popover.Trigger>
        </Button.GroupJoin>
      </Cell>
    </Row>
  )
}

/** A form to create a personal access token. */
function NewPersonalAccessTokenForm() {
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const { data: personalAccessTokens } = useSuspenseQuery(
    backendQueryOptions(backend, 'listPersonalAccessTokens', []),
  )
  const personalAccessTokenNames = new Set(personalAccessTokens.map((token) => token.name))
  const createPersonalAccessToken = useMutationCallback(
    backendMutationOptions(backend, 'createPersonalAccessToken'),
  )

  return (
    <Form
      schema={(z) =>
        z.object({
          name: z
            .string()
            .min(1)
            .refine(
              (name) => !personalAccessTokenNames.has(name),
              getText('duplicatePersonalAccessTokenError'),
            ),
        })
      }
      method="dialog"
      onSubmit={({ name }) => createPersonalAccessToken([{ name }])}
    >
      <Text.Heading variant="subtitle">{getText('newPersonalAccessToken')}</Text.Heading>
      <Input name="name" label={getText('name')} />
      <Button.Group className="relative">
        <Form.Submit />
        <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
      </Button.Group>
      <Form.FormError />
    </Form>
  )
}
