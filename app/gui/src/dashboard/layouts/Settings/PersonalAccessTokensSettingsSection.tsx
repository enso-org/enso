/** @file Settings tab for viewing and editing roles for all users in the organization. */
import { Cell, Column, Row, Table, TableBody, TableHeader } from '#/components/aria'
import { Scroller } from '#/components/Scroller'
import { backendQueryOptions } from '#/hooks/backendHooks'
import type { PersonalAccessToken } from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { useBackends, useFullUserSession, useText } from '$/providers/react'
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
  const { user } = useFullUserSession()
  const { data: personalAccessTokens } = useSuspenseQuery(
    backendQueryOptions(backend, 'listPersonalAccessTokens', []),
  )
  const isAdmin = user.isOrganizationAdmin

  const styles = PERSONAL_ACCESS_TOKEN_SETTINGS_SECTION_STYLES({})

  return (
    <div className="flex min-h-0 flex-1 flex-col gap-2">
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
            <Column isRowHeader className={styles.column({ className: 'w-[21rem] min-w-[21rem]' })}>
              {getText('createdAt')}
            </Column>
            <Column isRowHeader className={styles.column({ className: 'w-[21rem] min-w-[21rem]' })}>
              {getText('lastUsedAt')}
            </Column>
            {isAdmin && (
              <Column isRowHeader className={styles.column()}>
                {getText('actions')}
              </Column>
            )}
          </TableHeader>
          <TableBody
            items={personalAccessTokens}
            dependencies={[personalAccessTokens]}
            className="select-text"
          >
            {personalAccessTokens.length === 0 ?
              <Row className="h-10">
                <Cell className="col-span-2 px-2.5 placeholder">
                  {isAdmin ?
                    getText('youHaveNoUserGroupsAdmin')
                  : getText('youHaveNoUserGroupsNonAdmin')}
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
  const { getText } = useText()

  return (
    <Row className="group h-row rounded-rows-child">
      <Cell className="min-w-48 max-w-80 border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
        <span className="block text-sm">{personalAccessToken.name}</span>
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        <span className="block text-sm">
          {toReadableIsoString(new Date(personalAccessToken.createdAt))}
        </span>
      </Cell>
      <Cell className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
        <div className="flex flex-col">
          {personalAccessToken.lastUsedAt ?
            <span className="block text-sm text-primary/40">
              {toReadableIsoString(new Date(personalAccessToken.lastUsedAt))}
            </span>
          : <span className="block text-sm text-primary/40">{getText('never')}</span>}
        </div>
      </Cell>
    </Row>
  )
}
