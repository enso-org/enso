import { Suspense } from '#/components/Suspense'
import { AssetProperties as ReactAssetProperties } from '#/layouts/AssetPanel/components/AssetProperties'
import { AssetVersions as ReactAssetVersions } from '#/layouts/AssetPanel/components/AssetVersions'
import { ProjectExecutionsCalendar as ReactProjectExecutionsCalendar } from '#/layouts/AssetPanel/components/ProjectExecutionsCalendar'
import { ProjectSessions as ReactProjectSessions } from '#/layouts/AssetPanel/components/ProjectSessions'
import * as react from 'react'
import { applyPureReactInVue } from 'veaury'

const ReactDrive = react.lazy(() => import('#/layouts/Drive'))
const ReactEditor = react.lazy(() => import('#/layouts/Editor'))
const ReactSettings = react.lazy(() => import('#/layouts/Settings'))

function Suspended<P>(Component: (props: P) => react.ReactNode) {
  return (props: P & JSX.IntrinsicAttributes) => (
    <Suspense>
      <Component {...props} />
    </Suspense>
  )
}

export const Drive = applyPureReactInVue(Suspended(ReactDrive))
export const Editor = applyPureReactInVue(Suspended(ReactEditor))
export const Settings = applyPureReactInVue(Suspended(ReactSettings))
export const AssetProperties = applyPureReactInVue(Suspended(ReactAssetProperties))
export const AssetVersions = applyPureReactInVue(Suspended(ReactAssetVersions))
export const ProjectExecutionsCalendar = applyPureReactInVue(
  Suspended(ReactProjectExecutionsCalendar),
)
export const ProjectSessions = applyPureReactInVue(Suspended(ReactProjectSessions))
