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

// The names come from a third-party API and cannot be changed.
/* eslint-disable no-restricted-syntax, @typescript-eslint/naming-convention */
export const content = [THIS_PATH + '/src/**/*.tsx']
export const theme = {
    extend: {
        colors: {
            // Should be `#3e515fe5`, but `bg-opacity` does not work with RGBA.
            /** The default color of all text. */
            primary: '#52636f',
            // Should be `#3e515f14`, but `bg-opacity` does not work with RGBA.
            label: '#f0f1f3',
            help: '#3f68ce',
            warning: '#eab120',
            'severe-warning': '#e06740',
            'perm-owner': '#51626e',
            'perm-admin': '#e06a50',
            'perm-write': '#efa043',
            'perm-read': '#b6cb34',
            'perm-exec': '#ad69e3',
            'perm-docs-write': '#2db1c3',
            // Should be `#3e515f14`, but `bg-opacity` does not work with RGBA.
            'perm-none': '#f0f1f3',
        },
        flexGrow: {
            2: '2',
        },
        fontSize: {
            vs: '0.8125rem',
        },
        spacing: {
            '140': '35rem',
        },
        boxShadow: {
            soft: `0 0.5px 2.2px 0px #00000008, 0 1.2px 5.3px 0px #0000000b, \
0 2.3px 10px 0 #0000000e, 0 4px 18px 0 #00000011, 0 7.5px 33.4px 0 #00000014, \
0 18px 80px 0 #0000001c`,
            'inset-t-lg': `inset 0 1px 1.4px -1.4px #00000002, \
inset 0 2.4px 3.4px -3.4px #00000003, inset 0 4.5px 6.4px -6.4px #00000004, \
inset 0 8px 11.4px -11.4px #00000005, inset 0 15px 21.3px -21.3px #00000006, \
inset 0 36px 51px -51px #00000014`,
            'inset-b-lg': `inset 0 -1px 1.4px -1.4px #00000002, \
inset 0 -2.4px 3.4px -3.4px #00000003, inset 0 -4.5px 6.4px -6.4px #00000004, \
inset 0 -8px 11.4px -11.4px #00000005, inset 0 -15px 21.3px -21.3px #00000006, \
inset 0 -36px 51px -51px #00000014`,
            'inset-v-lg': `inset 0 1px 1.4px -1.4px #00000002, \
inset 0 2.4px 3.4px -3.4px #00000003, inset 0 4.5px 6.4px -6.4px #00000004, \
inset 0 8px 11.4px -11.4px #00000005, inset 0 15px 21.3px -21.3px #00000006, \
inset 0 36px 51px -51px #00000014, inset 0 -1px 1.4px -1.4px #00000002, \
inset 0 -2.4px 3.4px -3.4px #00000003, inset 0 -4.5px 6.4px -6.4px #00000004, \
inset 0 -8px 11.4px -11.4px #00000005, inset 0 -15px 21.3px -21.3px #00000006, \
inset 0 -36px 51px -51px #00000014`,
        },
        animation: {
            'spin-ease': 'spin cubic-bezier(0.67, 0.33, 0.33, 0.67) 1.5s infinite',
        },
        transitionProperty: {
            width: 'width',
            'stroke-dasharray': 'stroke-dasharray',
        },
        transitionDuration: {
            '90000': '90000ms',
        },
        gridTemplateColumns: {
            'fill-60': 'repeat(auto-fill, minmax(15rem, 1fr))',
        },
    },
}
