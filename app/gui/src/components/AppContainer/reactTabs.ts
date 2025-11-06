import { AssetProperties as ReactAssetProperties } from '#/layouts/AssetPanel/components/AssetProperties'
import { AssetVersions as ReactAssetVersions } from '#/layouts/AssetPanel/components/AssetVersions'
import { ProjectExecutionsCalendar as ReactProjectExecutionsCalendar } from '#/layouts/AssetPanel/components/ProjectExecutionsCalendar'
import { ProjectSessions as ReactProjectSessions } from '#/layouts/AssetPanel/components/ProjectSessions'
import { Drive as ReactDrive } from '#/layouts/Drive'
import ReactSettings from '#/layouts/Settings'
import { suspendedReactComponent } from '@/util/react'

export const Drive = suspendedReactComponent(ReactDrive)
export const Settings = suspendedReactComponent(ReactSettings)
export const AssetProperties = suspendedReactComponent(ReactAssetProperties)
export const AssetVersions = suspendedReactComponent(ReactAssetVersions)
export const ProjectExecutionsCalendar = suspendedReactComponent(ReactProjectExecutionsCalendar)
export const ProjectSessions = suspendedReactComponent(ReactProjectSessions)
