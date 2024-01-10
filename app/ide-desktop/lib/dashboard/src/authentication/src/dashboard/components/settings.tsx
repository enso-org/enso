/** @file Settings screen. */
import * as React from 'react'

import * as authProvider from '../../authentication/providers/auth'

import AccountSettingsTab from './settingsTab/accountSettingsTab'
import SettingsSidebar from './settingsSidebar'
import SettingsTab from './settingsTab/settingsTab'

// ================
// === Settings ===
// ================

/** Settings screen. */
export default function Settings() {
    const [settingsTab, setSettingsTab] = React.useState(SettingsTab.account)
    const { organization } = authProvider.useNonPartialUserSession()

    let content: JSX.Element
    switch (settingsTab) {
        case SettingsTab.account: {
            content = <AccountSettingsTab />
            break
        }
        default: {
            // This case should be removed when all settings tabs are implemented.
            content = <></>
            break
        }
    }

    return (
        <div className="flex flex-col gap-8">
            <div className="flex gap-2.5 font-bold text-xl h-9.5 px-4.75">
                <span className="py-0.5">Settings for </span>
                <div className="rounded-full leading-144.5 bg-frame h-9 px-2.25 pt-0.5 pb-1.25">
                    {organization?.name ?? '(Unknown Organization)'}
                </div>
            </div>
            <div className="flex gap-8 pl-3">
                <SettingsSidebar settingsTab={settingsTab} setSettingsTab={setSettingsTab} />
                {content}
            </div>
        </div>
    )
}
