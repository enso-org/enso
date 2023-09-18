/** @file A LocalStorage data manager. */
import * as common from 'enso-common'

import * as array from './array'
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
    protected values: Partial<LocalStorageData>

    /** Create a {@link LocalStorage}. */
    constructor() {
        const savedValues: unknown = JSON.parse(localStorage.getItem(this.localStorageKey) ?? '{}')
        this.values = {}
        if (typeof savedValues === 'object' && savedValues != null) {
            const backendTypes = Object.values(backend.BackendType)
            if (LocalStorageKey.page in savedValues) {
                const pages = Object.values(pageSwitcher.Page)
                if (array.includesPredicate(pages)(savedValues[LocalStorageKey.page])) {
                    this.values[LocalStorageKey.page] = savedValues[LocalStorageKey.page]
                }
            }
            if (LocalStorageKey.backendType in savedValues) {
                if (
                    array.includesPredicate(backendTypes)(savedValues[LocalStorageKey.backendType])
                ) {
                    this.values[LocalStorageKey.backendType] =
                        savedValues[LocalStorageKey.backendType]
                }
            }
            if (
                LocalStorageKey.extraColumns in savedValues &&
                Array.isArray(savedValues[LocalStorageKey.extraColumns])
            ) {
                this.values[LocalStorageKey.extraColumns] = savedValues[
                    LocalStorageKey.extraColumns
                ].filter(array.includesPredicate(column.EXTRA_COLUMNS))
            }
            if (LocalStorageKey.isTemplatesListOpen in savedValues) {
                this.values[LocalStorageKey.isTemplatesListOpen] = Boolean(
                    savedValues[LocalStorageKey.isTemplatesListOpen]
                )
            }
            if (LocalStorageKey.projectStartupInfo in savedValues) {
                const savedInfo = savedValues[LocalStorageKey.projectStartupInfo]
                if (typeof savedInfo !== 'object' || savedInfo == null) {
                    // Ignored - the saved value is invalid.
                } else if (
                    !('accessToken' in savedInfo) ||
                    typeof savedInfo.accessToken !== 'string'
                ) {
                    // Ignored - the saved value is invalid.
                } else if (
                    !('backendType' in savedInfo) ||
                    !array.includesPredicate(backendTypes)(savedInfo.backendType)
                ) {
                    // Ignored - the saved value is invalid.
                } else if (!('project' in savedInfo) || !('projectAsset' in savedInfo)) {
                    // Ignored - the saved value is invalid.
                } else {
                    this.values[LocalStorageKey.projectStartupInfo] = {
                        // These type assertions are UNSAFE, however correctly type-checking these
                        // would be quite complicated.
                        // eslint-disable-next-line no-restricted-syntax
                        project: savedInfo.project as backend.Project,
                        // eslint-disable-next-line no-restricted-syntax
                        projectAsset: savedInfo.projectAsset as backend.ProjectAsset,
                        backendType: savedInfo.backendType,
                        accessToken: savedInfo.accessToken,
                    }
                }
            }
            if (
                this.values[LocalStorageKey.projectStartupInfo] == null &&
                this.values[LocalStorageKey.page] === pageSwitcher.Page.editor
            ) {
                this.values[LocalStorageKey.page] = pageSwitcher.Page.drive
            }
        }
    }

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
