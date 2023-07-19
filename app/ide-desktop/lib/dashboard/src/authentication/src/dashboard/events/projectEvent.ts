/** @file Events related to changes in project state. */
import * as backendModule from '../backend'
import * as spinner from '../components/spinner'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        projectEvent: ProjectEvent
    }
}

// ====================
// === ProjectEvent ===
// ====================

/** Possible types of project state change. */
export enum ProjectEventType {
    create = 'create',
    open = 'open',
    showAsOpening = 'show-as-opening',
    cancelOpeningAll = 'cancel-opening-all',
    deleteMultiple = 'delete-multiple',
}

/** Properties common to all project state change events. */
interface ProjectBaseEvent<Type extends ProjectEventType> {
    type: Type
}

/** A signal to create a project. */
export interface ProjectCreateEvent extends ProjectBaseEvent<ProjectEventType.create> {
    placeholderId: backendModule.ProjectId
    templateId: string | null
    onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null
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

/** A signal to delete multiple projects. */
export interface ProjectDeleteMultipleEvent
    extends ProjectBaseEvent<ProjectEventType.deleteMultiple> {
    projectIds: Set<backendModule.ProjectId>
}

/** Every possible type of project event. */
export type ProjectEvent =
    | ProjectCancelOpeningAllEvent
    | ProjectCreateEvent
    | ProjectDeleteMultipleEvent
    | ProjectOpenEvent
    | ProjectShowAsOpeningEvent
