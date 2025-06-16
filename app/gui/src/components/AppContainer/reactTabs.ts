import { AssetProperties as ReactAssetProperties } from '$/panels/AssetProperties'
import { AssetVersions as ReactAssetVersions } from '$/panels/AssetVersions'
import { ProjectExecutionsCalendar as ReactProjectExecutionsCalendar } from '$/panels/ProjectExecutionsCalendar'
import { ProjectSessions as ReactProjectSessions } from '$/panels/ProjectSessions'
import { suspendedReactComponent } from '@/util/react'
import * as react from 'react'

const ReactDrive = react.lazy(() => import('$/data-catalog/Drive'))
const ReactEditor = react.lazy(() => import('#/Editor'))
const ReactSettings = react.lazy(() =>
  import('$/settings').then(({ Settings }) => ({ default: Settings })),
)

export const Drive = suspendedReactComponent(ReactDrive)
export const Editor = suspendedReactComponent(ReactEditor)
export const Settings = suspendedReactComponent(ReactSettings)
export const AssetProperties = suspendedReactComponent(ReactAssetProperties)
export const AssetVersions = suspendedReactComponent(ReactAssetVersions)
export const ProjectExecutionsCalendar = suspendedReactComponent(ReactProjectExecutionsCalendar)
export const ProjectSessions = suspendedReactComponent(ReactProjectSessions)
