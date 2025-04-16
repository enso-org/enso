import { Suspense } from '#/components/Suspense'
import { AssetDocs as ReactAssetDocs } from '#/layouts/AssetPanel/components/AssetDocs.tsx'
import { AssetProperties as ReactAssetProperties } from '#/layouts/AssetPanel/components/AssetProperties'
import { AssetVersions as ReactAssetVersions } from '#/layouts/AssetPanel/components/AssetVersions'
import { ProjectExecutionsCalendar as ReactProjectExecutionsCalendar } from '#/layouts/AssetPanel/components/ProjectExecutionsCalendar'
import { ProjectSessions as ReactProjectSessions } from '#/layouts/AssetPanel/components/ProjectSessions'
import { Category } from '#/layouts/CategorySwitcher/Category'
import { useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import Backend from '#/services/Backend'
import { useBackends } from '$/providers/react'
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

interface BasedOnCategoryProps {
  backend: Backend
  category: Category
  isReadonly: boolean
}

function WithCategory(Component: (props: BasedOnCategoryProps) => react.ReactNode) {
  return () => {
    const { category } = useCategoriesAPI()
    const deferredCategory = react.useDeferredValue(category)
    const { backendByCategory } = useBackends()
    const backend = backendByCategory(category)
    const isReadonly = category.type === 'trash'
    return <Component backend={backend} category={deferredCategory} isReadonly={isReadonly} />
  }
}

export const Drive = applyPureReactInVue(Suspended(ReactDrive))
export const Editor = applyPureReactInVue(Suspended(ReactEditor))
export const Settings = applyPureReactInVue(Suspended(ReactSettings))
export const AssetProperties = applyPureReactInVue(Suspended(WithCategory(ReactAssetProperties)))
export const AssetVersions = applyPureReactInVue(Suspended(WithCategory(ReactAssetVersions)))

export const ProjectExecutionsCalendar = applyPureReactInVue(
  Suspended(WithCategory(ReactProjectExecutionsCalendar)),
)
export const ProjectSessions = applyPureReactInVue(Suspended(WithCategory(ReactProjectSessions)))
export const AssetDocs = applyPureReactInVue(Suspended(WithCategory(ReactAssetDocs)))
