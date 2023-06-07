/** @file Discriminated unions used as event messages, sent through React states. */

import * as backendModule from './backend'

// ====================
// === ProjectEvent ===
// ====================

/** Possible types of project state change. */
export enum ProjectEventType {
    open = 'open',
    cancelOpeningAll = 'cancelOpeningAll',
}

/** Properties common to all project state change events. */
interface ProjectBaseEvent<Type extends ProjectEventType> {
    type: Type
}

/** Requests the specified project to be opened. */
export interface ProjectOpenEvent extends ProjectBaseEvent<ProjectEventType.open> {
    /** This must be a name because it may be specified by name on the command line.
     * Note that this will not work properly with the cloud backend if there are multiple projects
     * with the same name. */
    projectId: backendModule.ProjectId
}

/** Requests the specified project to be opened. */
export interface ProjectCancelOpeningAllEvent
    extends ProjectBaseEvent<ProjectEventType.cancelOpeningAll> {}

/** Every possible type of project event. */
export type ProjectEvent = ProjectCancelOpeningAllEvent | ProjectOpenEvent
