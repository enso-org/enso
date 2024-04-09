/** @file A list of members in the organization. */
import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

/** A list of members in the organization. */
export default function MembersTable() {
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const members = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const isLoading = members == null

  return (
    <aria.Table
      aria-label={getText('users')}
      selectionMode="multiple"
      className="table-fixed self-start rounded-rows"
    >
      <aria.TableHeader className="h-row">
        <aria.Column
          isRowHeader
          className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
        >
          {getText('name')}
        </aria.Column>
        <aria.Column className="w-members-email-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
          {getText('email')}
        </aria.Column>
      </aria.TableHeader>
      <aria.TableBody className="select-text">
        {isLoading ? (
          <aria.Row className="h-row">
            <aria.Cell
              ref={element => {
                if (element != null) {
                  element.colSpan = 2
                }
              }}
              className="rounded-full bg-transparent"
            >
              <div className="flex justify-center">
                <StatelessSpinner size={32} state={statelessSpinner.SpinnerState.loadingMedium} />
              </div>
            </aria.Cell>
          </aria.Row>
        ) : (
          members.map(member => (
            <aria.Row key={member.userId} className="h-row">
              <aria.Cell className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                {member.name}
              </aria.Cell>
              <aria.Cell className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                {member.email}
              </aria.Cell>
            </aria.Row>
          ))
        )}
      </aria.TableBody>
    </aria.Table>
  )
}
