/** @file Events related to changes in project state. */
import * as backendModule from '../backend'

// Note: Events are currently sent from the parent. This causes a re-render of its entire subtree
// and potentially degrades performance. Realistically, the performance degradation will not be
// noticeable, but it is worth considering for improvements at some point in the future.

// ====================
// === ProjectEvent ===
// ====================

/** Possible types of project state change. */
export enum ProjectEventType {
    open = 'open',
    showAsOpening = 'show-as-opening',
    cancelOpeningAll = 'cancel-opening-all',
}

/** Properties common to all project state change events. */
interface ProjectBaseEvent<Type extends ProjectEventType> {
    type: Type
}

/** A signal to open the specified project. */
export interface ProjectOpenEvent extends ProjectBaseEvent<ProjectEventType.open> {
    projectId: backendModule.ProjectId
}

/** A signal to display the specified project as opening, but not actually send the call to the
 * backend. */
export interface ProjectShowAsOpeningEvent
    extends ProjectBaseEvent<ProjectEventType.showAsOpening> {
    projectId: backendModule.ProjectId
}

/** A signal to stop automatically opening any project that is currently opening. */
export interface ProjectCancelOpeningAllEvent
    extends ProjectBaseEvent<ProjectEventType.cancelOpeningAll> {}

/** Every possible type of project event. */
export type ProjectEvent =
    | ProjectCancelOpeningAllEvent
    | ProjectOpenEvent
    | ProjectShowAsOpeningEvent
