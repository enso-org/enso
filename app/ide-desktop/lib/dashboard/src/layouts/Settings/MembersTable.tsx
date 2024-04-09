/** @file A list of members in the organization. */
import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

/** A list of members in the organization. */
export default function MembersTable() {
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const members = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const isLoading = members == null

  return (
    <table className="table-fixed self-start rounded-rows">
      <thead>
        <tr className="h-row">
          <th className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
            {getText('name')}
          </th>
          <th className="w-members-email-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
            {getText('email')}
          </th>
        </tr>
      </thead>
      <tbody className="select-text">
        {isLoading ? (
          <tr className="h-row">
            <td colSpan={2} className="rounded-full bg-transparent">
              <div className="flex justify-center">
                <StatelessSpinner size={32} state={statelessSpinner.SpinnerState.loadingMedium} />
              </div>
            </td>
          </tr>
        ) : (
          members.map(member => (
            <tr key={member.userId} className="h-row">
              <td className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                {member.name}
              </td>
              <td className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                {member.email}
              </td>
            </tr>
          ))
        )}
      </tbody>
    </table>
  )
}
