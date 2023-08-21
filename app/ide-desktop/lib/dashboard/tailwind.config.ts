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
            /** The default color of all text. */
            // This should be named "regular".
            primary: 'rgba(0, 0, 0, 0.60)',
            'not-selected': 'rgba(0, 0, 0, 0.40)',
            'icon-selected': 'rgba(0, 0, 0, 0.50)',
            'icon-not-selected': 'rgba(0, 0, 0, 0.30)',
            chat: '#484848',
            'ide-bg': '#ebeef1',
            'ide-bg-dark': '#d0d3d6',
            selected: 'rgba(255, 255, 255, 0.40)',
            // Should be `#3e515f14`, but `bg-opacity` does not work with RGBA.
            label: '#f0f1f3',
            help: '#3f68ce',
            invite: '#0e81d4',
            cloud: '#0666be',
            delete: 'rgba(243, 24, 10, 0.87)',
            dim: 'rgba(0, 0, 0, 0.25)',
            frame: 'rgba(255, 255, 255, 0.40)',
            'frame-selected': 'rgba(255, 255, 255, 0.70)',
            'tag-text': 'rgba(255, 255, 255, 0.90)',
            'tag-text-2': 'rgba(0, 0, 0, 0.60)',
            'permission-owner': 'rgba(236, 2, 2, 0.70)',
            'permission-admin': 'rgba(252, 60, 0, 0.70)',
            'permission-edit': 'rgba(255, 138, 0, 0.90)',
            'permission-read': 'rgba(152, 174, 18, 0.80)',
            'permission-docs': 'rgba(91, 8, 226, 0.64)',
            'permission-exec': 'rgba(236, 2, 2, 0.70)',
            'permission-view': 'rgba(0, 0, 0, 0.10)',
            'call-to-action': '#fa6c08',
            'black-a5': 'rgba(0, 0, 0, 0.05)',
            'black-a10': 'rgba(0, 0, 0, 0.10)',
            'black-a16': 'rgba(0, 0, 0, 0.16)',
            'black-a30': 'rgba(0, 0, 0, 0.30)',
            'black-a50': 'rgba(0, 0, 0, 0.50)',
            'gray-350': '#b7bcc5',
        },
        fontSize: {
            xs: '0.71875rem',
            sm: '0.8125rem',
        },
        lineHeight: {
            '170': '170%',
        },
        spacing: {
            '0.75': '0.1875rem',
            '1.75': '0.4375rem',
            '2.25': '0.5625rem',
            '3.25': '0.8125rem',
            '4.75': '1.1875rem',
            '5.5': '1.375rem',
            '6.5': '1.625rem',
            '9.5': '2.375rem',
            '13': '3.25rem',
            '18': '4.5rem',
            '29': '7.25rem',
            '30': '7.5rem',
            '30.25': '7.5625rem',
            '42': '10.5rem',
            '51.5': '12.875rem',
            '54': '13.5rem',
            '57.5': '14.375rem',
            '62': '15.5rem',
            '70': '17.5rem',
            '83.5': '20.875rem',
            '98.25': '24.5625rem',
            '112.5': '28.125rem',
            '115.25': '28.8125rem',
            '140': '35rem',
            '10lh': '10lh',
        },
        minWidth: {
            '20': '5rem',
            '36': '9rem',
            '40': '10rem',
            '60': '15rem',
            '80': '20rem',
            '96': '24rem',
        },
        opacity: {
            '1/3': '.33333333',
        },
        zIndex: {
            '1': '1',
        },
        backdropBlur: {
            xs: '2px',
        },
        borderWidth: { '0.5': '0.5px' },
        boxShadow: {
            soft: `0 0.5px 2.2px 0px #00000008, 0 1.2px 5.3px 0px #0000000b, \
0 2.3px 10px 0 #0000000e, 0 4px 18px 0 #00000011, 0 7.5px 33.4px 0 #00000014, \
0 18px 80px 0 #0000001c`,
            'soft-dark': `0 0.5px 2.2px 0px #00000010, 0 1.2px 5.3px 0px #00000014, \
0 2.3px 10px 0 #0000001c, 0 4px 18px 0 #00000022, 0 7.5px 33.4px 0 #00000028, \
0 18px 80px 0 #00000038`,
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
            'grid-template-rows': 'grid-template-rows',
        },
        transitionDuration: {
            '5000': '5000ms',
            '90000': '90000ms',
        },
        gridTemplateRows: {
            '0fr': '0fr',
            '1fr': '1fr',
        },
        gridTemplateColumns: {
            'fill-60': 'repeat(auto-fill, minmax(15rem, 1fr))',
        },
    },
}
