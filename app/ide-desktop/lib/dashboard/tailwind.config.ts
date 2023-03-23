/** @file Configuration for Tailwind. */
import * as path from 'node:path'
import * as url from 'node:url'

// =================
// === Constants ===
// =================

const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// =====================
// === Configuration ===
// =====================

// This is a third-party API that we do not control.
/* eslint-disable no-restricted-syntax, @typescript-eslint/naming-convention */
export const content = [THIS_PATH + '/src/**/*.tsx']
export const theme = {
    extend: {
        colors: {
            /** The default color of all text. */
            primary: '#3e515fe5',
            'label-bg': '#3e515f14',
            'perm-owner': '#51626e',
            'perm-admin': '#e06a50',
            'perm-write': '#efa043',
            'perm-read': '#b6cb34',
            'perm-exec': '#ad69e3',
            'perm-docs-write': '#2db1c3',
            'perm-none': '#3e515f14',
        },
    },
}
