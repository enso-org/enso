/** @file Events related to changes in the project list. */
import * as spinner from '../components/spinner'

// This is required, to whitelist this event.
// eslint-disable-next-line no-restricted-syntax
declare module '../../hooks' {
    /** A map containing all known event types. */
    export interface KnownEventsMap {
        projectListEvent: ProjectListEvent
    }
}

// ========================
// === ProjectListEvent ===
// ========================

/** Possible changes to the project list. */
export enum ProjectListEventType {
    create = 'create',
    delete = 'delete',
}

/** Properties common to all project list events. */
interface ProjectListBaseEvent<Type extends ProjectListEventType> {
    type: Type
}

/** A signal to create a new project. */
interface ProjectListCreateEvent extends ProjectListBaseEvent<ProjectListEventType.create> {
    templateId: string | null
    onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null
}

/** A signal to delete a project. */
interface ProjectListDeleteEvent extends ProjectListBaseEvent<ProjectListEventType.delete> {
    projectId: string
}

/** Every possible type of project list event. */
export type ProjectListEvent = ProjectListCreateEvent | ProjectListDeleteEvent
