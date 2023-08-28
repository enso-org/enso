/** @file A LocalStorage data manager. */
import * as common from 'enso-common'

import * as backend from './backend'
import * as column from './column'

import * as pageSwitcher from './components/pageSwitcher'

// ====================
// === LocalStorage ===
// ====================

/** All possible keys for a {@link LocalStorage}. */
export enum LocalStorageKey {
    page = 'page',
    backendType = 'backend-type',
    extraColumns = 'extra-columns',
    isTemplatesListOpen = 'is-templates-list-open',
    projectStartupInfo = 'project-startup-info',
    driveCategory = 'drive-category',
}

/** The data that can be stored in a {@link LocalStorage}. */
interface LocalStorageData {
    [LocalStorageKey.page]: pageSwitcher.Page
    [LocalStorageKey.backendType]: backend.BackendType
    [LocalStorageKey.extraColumns]: column.ExtraColumn[]
    [LocalStorageKey.isTemplatesListOpen]: boolean
    [LocalStorageKey.projectStartupInfo]: backend.ProjectStartupInfo
    [LocalStorageKey.driveCategory]: backend.FilterBy
}

/** Whether each {@link LocalStorageKey} is user specific.
 * The type annotation ensures that this object MUST be edited when a new {@link LocalStorageKey}
 * is added. */
const IS_USER_SPECIFIC: Record<LocalStorageKey, boolean> = {
    [LocalStorageKey.page]: false,
    [LocalStorageKey.backendType]: false,
    [LocalStorageKey.extraColumns]: false,
    [LocalStorageKey.isTemplatesListOpen]: false,
    [LocalStorageKey.projectStartupInfo]: true,
    [LocalStorageKey.driveCategory]: false,
}

/** A LocalStorage data manager. */
export class LocalStorage {
    localStorageKey = common.PRODUCT_NAME.toLowerCase()
    // This is SAFE, as this app is the only one that writes to this `localStorage` key.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    protected values: Partial<LocalStorageData> = JSON.parse(
        localStorage.getItem(this.localStorageKey) ?? '{}'
    )

    /** Retrieve an entry from the stored data. */
    get<K extends LocalStorageKey>(key: K) {
        return this.values[key]
    }

    /** Write an entry to the stored data, and save. */
    set<K extends LocalStorageKey>(key: K, value: LocalStorageData[K]) {
        this.values[key] = value
        this.save()
    }

    /** Delete an entry from the stored data, and save. */
    delete<K extends LocalStorageKey>(key: K) {
        const oldValue = this.values[key]
        // The key being deleted is one of a statically known set of keys.
        // eslint-disable-next-line @typescript-eslint/no-dynamic-delete
        delete this.values[key]
        this.save()
        return oldValue
    }

    /** Delete user-specific entries from the stored data, and save. */
    clearUserSpecificEntries() {
        for (const [key, isUserSpecific] of Object.entries(IS_USER_SPECIFIC)) {
            if (isUserSpecific) {
                // This is SAFE. The only reason this does not typecheck is because `Object.entries`
                // types the keys as `strings`, because objects may have extra keys due to width
                // subtyping.
                // eslint-disable-next-line no-restricted-syntax
                this.delete(key as LocalStorageKey)
            }
        }
    }

    /** Save the current value of the stored data.. */
    protected save() {
        localStorage.setItem(this.localStorageKey, JSON.stringify(this.values))
    }
}
