/** @file types related to Dashboard component. */
import type { BackendType, ProjectAsset } from '#/services/Backend'

/** Dashboard properties */
export interface DashboardProps {
  readonly projectToOpen?:
    | { readonly asset: ProjectAsset; readonly backend: BackendType }
    | undefined
}
