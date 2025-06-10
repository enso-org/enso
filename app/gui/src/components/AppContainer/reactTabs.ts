import { AssetProperties as ReactAssetProperties } from '#/layouts/AssetPanel/components/AssetProperties'
import { AssetVersions as ReactAssetVersions } from '#/layouts/AssetPanel/components/AssetVersions'
import { ProjectExecutionsCalendar as ReactProjectExecutionsCalendar } from '#/layouts/AssetPanel/components/ProjectExecutionsCalendar'
import { ProjectSessions as ReactProjectSessions } from '#/layouts/AssetPanel/components/ProjectSessions'
import { suspendedReactComponent } from '@/util/react'
import * as react from 'react'

const ReactDrive = react.lazy(() => import('#/layouts/Drive'))
const ReactEditor = react.lazy(() => import('#/layouts/Editor'))
const ReactSettings = react.lazy(() => import('#/layouts/Settings'))

export const Drive = suspendedReactComponent(ReactDrive)
export const Editor = suspendedReactComponent(ReactEditor)
export const Settings = suspendedReactComponent(ReactSettings)
export const AssetProperties = suspendedReactComponent(ReactAssetProperties)
export const AssetVersions = suspendedReactComponent(ReactAssetVersions)
export const ProjectExecutionsCalendar = suspendedReactComponent(ReactProjectExecutionsCalendar)
export const ProjectSessions = suspendedReactComponent(ReactProjectSessions)
