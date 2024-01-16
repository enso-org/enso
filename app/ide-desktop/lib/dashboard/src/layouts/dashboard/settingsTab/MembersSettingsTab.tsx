/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import * as backendProvider from '#/providers/BackendProvider'
import type * as backend from '#/services/backend'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

// ==========================
// === MembersSettingsTab ===
// ==========================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsTab() {
    const { backend } = backendProvider.useBackend()
    const [members, setMembers] = React.useState<backend.SimpleUser[] | null>(null)
    const isLoading = members == null

    React.useEffect(() => {
        void (async () => {
            setMembers(await backend.listUsers())
        })()
    }, [backend])

    return (
        <div className="flex flex-col gap-8">
            <div className="flex flex-col gap-2.5">
                <h3 className="font-bold text-xl h-9.5 py-0.5">Members</h3>
                <table className="self-start table-fixed">
                    <thead>
                        <tr className="h-8">
                            <th className="text-left bg-clip-padding border-transparent border-l-2 border-r-2 last:border-r-0 text-sm font-semibold w-32">
                                Name
                            </th>
                            <th className="text-left bg-clip-padding border-transparent border-l-2 border-r-2 last:border-r-0 text-sm font-semibold w-48">
                                Email
                            </th>
                        </tr>
                    </thead>
                    <tbody className="select-text">
                        {isLoading ? (
                            <tr className="h-8">
                                <td colSpan={2}>
                                    <StatelessSpinner
                                        size={32}
                                        state={statelessSpinner.SpinnerState.loadingMedium}
                                    />
                                </td>
                            </tr>
                        ) : (
                            members.map(member => (
                                <tr key={member.id} className="h-8">
                                    <td>{member.name}</td>
                                    <td>{member.email}</td>
                                </tr>
                            ))
                        )}
                    </tbody>
                </table>
            </div>
        </div>
    )
}
