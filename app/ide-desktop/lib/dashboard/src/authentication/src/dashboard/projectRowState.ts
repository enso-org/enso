/** @file State for each row of the table of projects. */
import * as optimistic from './optimistic'

/** Data associated with a project, used for rendering. */
export interface ProjectRowState {
    isRunning: boolean
    status: optimistic.OptimisticStatus
}

/** The default {@link ProjectRowState} associated with a {@link backendModule.Project}. */
export const INITIAL_ROW_STATE: ProjectRowState = Object.freeze({
    isRunning: false,
    status: optimistic.OptimisticStatus.present,
})
