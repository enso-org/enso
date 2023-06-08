/** @file Events related to changes in the directory list. */

// Note: Events are currently sent from the parent. This causes a re-render of its entire subtree
// and potentially degrades performance. Realistically, the performance degradation will not be
// noticeable, but it is worth considering for improvements at some point in the future.

// ======================
// === DirectoryEvent ===
// ======================

/** Possible types of project state change. */
export enum DirectoryEventType {
    createProject = 'create-project',
    createDirectory = 'create-directory',
}

/** Properties common to all project state change events. */
interface DirectoryBaseEvent<Type extends DirectoryEventType> {
    type: Type
}

/** A signal to create a new project. */
export interface DirectoryCreateProjectEvent
    extends DirectoryBaseEvent<DirectoryEventType.createProject> {
    templateId: string | null
}

/** A signal to create a new directory. */
export interface DirectoryCreateDirectoryEvent
    extends DirectoryBaseEvent<DirectoryEventType.createDirectory> {}

/** Every possible type of directory event. */
export type DirectoryEvent = DirectoryCreateDirectoryEvent | DirectoryCreateProjectEvent
