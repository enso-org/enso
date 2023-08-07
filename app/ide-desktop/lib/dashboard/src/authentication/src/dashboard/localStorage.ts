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
}

/** The data that can be stored in a {@link LocalStorage}. */
interface LocalStorageData {
    [LocalStorageKey.page]: pageSwitcher.Page
    [LocalStorageKey.backendType]: backend.BackendType
    [LocalStorageKey.extraColumns]: column.ExtraColumn[]
    [LocalStorageKey.isTemplatesListOpen]: boolean
    [LocalStorageKey.projectStartupInfo]: backend.ProjectStartupInfo
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

    /** Delete all entries from the stored data, and save. */
    clear() {
        this.values = {}
        localStorage.removeItem(this.localStorageKey)
    }

    /** Save the current value of the stored data.. */
    protected save() {
        localStorage.setItem(this.localStorageKey, JSON.stringify(this.values))
    }
}
