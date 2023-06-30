/** @file State for each row of the table of projects. */

// =======================
// === ProjectRowState ===
// =======================

/** Data associated with a project, used for rendering. */
export interface ProjectRowState {
    isRunning: boolean
}

/** The default {@link ProjectRowState} associated with a {@link backendModule.Project}. */
export const INITIAL_ROW_STATE: ProjectRowState = Object.freeze({
    isRunning: false,
})
