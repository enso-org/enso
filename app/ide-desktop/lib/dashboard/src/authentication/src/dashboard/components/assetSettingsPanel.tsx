/** @file A panel containing the description and settings for an asset. */
import * as React from 'react'

import * as assetEvent from '../events/assetEvent'
import * as backend from '../backend'

import * as column from '../column'
import AssetInfoBar from './assetInfoBar'
import UserBar from './userBar'

// ==========================
// === AssetSettingsPanel ===
// ==========================

/** The subset of {@link AssetSettingsPanelProps} that are required to be supplied by the row. */
export interface AssetSettingsPanelRequiredProps {
    item: backend.AnyAsset
    setItem: React.Dispatch<React.SetStateAction<backend.AnyAsset>>
    /** This must be supplied by the row as the dashboard container does not have access to it. */
    dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
}

/** Props for a {@link AssetSettingsPanel}. */
export interface AssetSettingsPanelProps extends AssetSettingsPanelRequiredProps {
    isHelpChatOpen: boolean
    setIsHelpChatOpen: React.Dispatch<React.SetStateAction<boolean>>
    setIsSettingsPanelVisible: React.Dispatch<React.SetStateAction<boolean>>
    onSignOut: () => void
}

/** A panel containing the description and settings for an asset. */
export default function AssetSettingsPanel(props: AssetSettingsPanelProps) {
    const {
        item: rawItem,
        setItem: rawSetItem,
        isHelpChatOpen,
        setIsHelpChatOpen,
        setIsSettingsPanelVisible,
        onSignOut,
        dispatchAssetEvent,
    } = props
    const [item, innerSetItem] = React.useState(rawItem)
    const setItem = React.useCallback(
        (valueOrUpdater: React.SetStateAction<backend.AnyAsset>) => {
            innerSetItem(valueOrUpdater)
            rawSetItem(valueOrUpdater)
        },
        [/* should never change */ rawSetItem]
    )
    return (
        <div
            className="absolute flex flex-col h-full border-black-a12 border-l-2 gap-8 w-120 pl-3 pr-4 py-2.25"
            onClick={event => {
                event.stopPropagation()
            }}
        >
            <div className="flex">
                {/* Spacing. */}
                <div className="grow" />
                <div className="flex gap-2">
                    <AssetInfoBar
                        canToggleSettingsPanel={true}
                        isSettingsPanelVisible={true}
                        setIsSettingsPanelVisible={setIsSettingsPanelVisible}
                    />
                    <UserBar
                        isHelpChatOpen={isHelpChatOpen}
                        setIsHelpChatOpen={setIsHelpChatOpen}
                        onSignOut={onSignOut}
                    />
                </div>
            </div>
            <div className="flex flex-col items-start gap-1">
                <span className="text-lg leading-144.5 h-7 py-px">Description</span>
                <div className="py-1">
                    <span className="leading-170 py-px">
                        This connector can be used to get the latest user data (purchase history,
                        website activities, etc.)
                    </span>
                </div>
            </div>
            <div className="flex flex-col items-start gap-2">
                <span className="text-lg leading-144.5 h-7 py-px">Settings</span>
                <table>
                    <tbody>
                        <tr>
                            <td className="min-w-32 px-0 py-1">
                                <span className="inline-block leading-170 h-6 py-px">
                                    Shared with
                                </span>
                            </td>
                            <td className="p-0 w-full">
                                <column.SharedWithColumn
                                    item={item}
                                    setItem={setItem}
                                    state={{ dispatchAssetEvent }}
                                />
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>
    )
}
