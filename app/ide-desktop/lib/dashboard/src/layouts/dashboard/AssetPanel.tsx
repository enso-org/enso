/** @file A panel containing the description and settings for an asset. */
import * as React from 'react'

import type * as assetEvent from '#/events/assetEvent'
import AssetProperties from '#/layouts/dashboard/AssetProperties'
import type Category from '#/layouts/dashboard/CategorySwitcher/Category'
import type * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import UserBar from '#/layouts/dashboard/UserBar'
import type * as backendModule from '#/services/backend'
import type * as assetTreeNode from '#/utilities/assetTreeNode'

import AssetInfoBar from '#/components/dashboard/AssetInfoBar'

// =====================
// === AssetPanelTab ===
// =====================

/** Determines the content of the {@link AssetPanel}. */
enum AssetPanelTab {
    properties = 'properties',
    versions = 'versions',
}

// ==================
// === AssetPanel ===
// ==================

/** The subset of {@link AssetPanelProps} that are required to be supplied by the row. */
export interface AssetPanelRequiredProps {
    item: assetTreeNode.AssetTreeNode
    setItem: React.Dispatch<React.SetStateAction<assetTreeNode.AssetTreeNode>>
}

/** Props for an {@link AssetPanel}. */
export interface AssetPanelProps extends AssetPanelRequiredProps {
    supportsLocalBackend: boolean
    page: pageSwitcher.Page
    category: Category
    isHelpChatOpen: boolean
    setIsHelpChatOpen: React.Dispatch<React.SetStateAction<boolean>>
    setIsSettingsPanelVisible: React.Dispatch<React.SetStateAction<boolean>>
    dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
    projectAsset: backendModule.ProjectAsset | null
    setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
    doRemoveSelf: () => void
    onSignOut: () => void
}

/** A panel containing the description and settings for an asset. */
export default function AssetPanel(props: AssetPanelProps) {
    const { item, setItem, supportsLocalBackend, page, category } = props
    const { isHelpChatOpen, setIsHelpChatOpen, setIsSettingsPanelVisible } = props
    const { dispatchAssetEvent, projectAsset, setProjectAsset, doRemoveSelf, onSignOut } = props

    const [tab, setTab] = React.useState(AssetPanelTab.properties)

    return (
        <div
            className="absolute flex flex-col h-full border-black/[0.12] border-l-2 gap-8 w-120 pl-3 pr-4 py-2.25"
            onClick={event => {
                event.stopPropagation()
            }}
        >
            <div className="flex">
                <button
                    className={`rounded-full leading-5 px-2 hover:bg-frame transition-colors ${
                        tab !== AssetPanelTab.versions ? '' : 'bg-frame'
                    }`}
                    onClick={() => {
                        setTab(oldTab =>
                            oldTab === AssetPanelTab.versions
                                ? AssetPanelTab.properties
                                : AssetPanelTab.versions
                        )
                    }}
                >
                    Versions
                </button>
                {/* Spacing. */}
                <div className="grow" />
                <div className="flex gap-2">
                    <AssetInfoBar
                        canToggleSettingsPanel={true}
                        isSettingsPanelVisible={true}
                        setIsSettingsPanelVisible={setIsSettingsPanelVisible}
                    />
                    <UserBar
                        supportsLocalBackend={supportsLocalBackend}
                        isHelpChatOpen={isHelpChatOpen}
                        setIsHelpChatOpen={setIsHelpChatOpen}
                        onSignOut={onSignOut}
                        page={page}
                        projectAsset={projectAsset}
                        setProjectAsset={setProjectAsset}
                        doRemoveSelf={doRemoveSelf}
                    />
                </div>
            </div>
            {tab === AssetPanelTab.properties && (
                <AssetProperties
                    item={item}
                    setItem={setItem}
                    category={category}
                    dispatchAssetEvent={dispatchAssetEvent}
                />
            )}
        </div>
    )
}
