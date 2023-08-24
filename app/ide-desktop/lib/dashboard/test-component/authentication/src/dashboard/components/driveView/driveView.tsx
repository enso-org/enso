/** @file A wrapper around {@link DriveView}. */
import * as React from 'react'

import type * as assetListEvent from '../../../../../../src/authentication/src/dashboard/events/assetListEvent'
import * as backendProvider from '../../../../../../src/authentication/src/providers/backend'
import * as hooks from '../../../../../../src/authentication/src/hooks'
import * as http from '../../../../../../src/authentication/src/http'
import * as remoteBackend from '../../../../../../src/authentication/src/dashboard/remoteBackend'

import * as pageSwitcher from '../../../../../../src/authentication/src/dashboard/components/pageSwitcher'
import DriveView from '../../../../../../src/authentication/src/dashboard/components/driveView'

/* eslint-disable @typescript-eslint/no-magic-numbers */

/** A wrapper around {@link DriveView}. */
export default function DriveViewWrapper() {
    const client = new http.Client(new Headers([['Authorization', '']]))
    const backend = new remoteBackend.RemoteBackend(client, console)
    const [assetListEvents, dispatchAssetListEvent] =
        hooks.useEvent<assetListEvent.AssetListEvent>()
    return (
        <backendProvider.BackendProvider initialBackend={backend}>
            <DriveView
                page={pageSwitcher.Page.drive}
                hidden={false}
                appRunner={{
                    runApp: async () => {
                        // Ignored.
                    },
                    stopApp: () => {
                        // Ignored.
                    },
                }}
                query={''}
                assetListEvents={assetListEvents}
                dispatchAssetListEvent={dispatchAssetListEvent}
                initialProjectName={null}
                isListingLocalDirectoryAndWillFail={false}
                isListingRemoteDirectoryAndWillFail={false}
                isListingRemoteDirectoryWhileOffline={false}
                loadingProjectManagerDidFail={false}
                doCloseEditor={() => {
                    // Ignored.
                }}
                doOpenEditor={() => {
                    // Ignored.
                }}
            />
        </backendProvider.BackendProvider>
    )
}
