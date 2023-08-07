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
            primary: 'rgba(0, 0, 0, 0.60)',
            chat: '#484848',
            'ide-bg': '#ebeef1',
            'ide-bg-dark': '#d0d3d6',
            selected: 'rgba(255, 255, 255, 0.40)',
            // Should be `#3e515f14`, but `bg-opacity` does not work with RGBA.
            label: '#f0f1f3',
            help: '#3f68ce',
            cloud: '#0666be',
            v3: '#252423',
            youtube: '#c62421',
            discord: '#404796',
            frame: 'rgba(255, 255, 255, 0.40)',
            'frame-selected': 'rgba(255, 255, 255, 0.70)',
            'frame-bg': 'rgba(255, 255, 255, 0.40)',
            'frame-selected-bg': 'rgba(255, 255, 255, 0.70)',
            'tag-text': 'rgba(255, 255, 255, 0.90)',
            'tag-text-2': 'rgba(0, 0, 0, 0.60)',
            'permission-owner': 'rgba(236, 2, 2, 0.70)',
            'permission-admin': 'rgba(252, 60, 0, 0.70)',
            'permission-edit': 'rgba(255, 138, 0, 0.90)',
            'permission-read': 'rgba(152, 174, 18, 0.80)',
            'permission-exec': 'rgba(236, 2, 2, 0.70)',
            'permission-docs': 'rgba(91, 8, 226, 0.64)',
            'permission-view': 'rgba(0, 0, 0, 0.10)',
            'call-to-action': '#fa6c08',
            'gray-350': '#b7bcc5',
        },
        flexGrow: {
            2: '2',
        },
        fontSize: {
            xs: '0.71875rem',
            vs: '0.8125rem',
            sm: '0.8125rem',
            xl: '1.1875rem',
            '4xl': '2.375rem',
        },
        lineHeight: {
            '144.5': '144.5%',
        },
        spacing: {
            '0.75': '0.1875rem',
            '1.75': '0.4375rem',
            '2.25': '0.5625rem',
            '3.25': '0.8125rem',
            '3.5': '0.875rem',
            '4.5': '1.125rem',
            '4.75': '1.1875rem',
            '5.5': '1.375rem',
            '9.5': '2.375rem',
            '9.75': '2.4375rem',
            '18': '4.5rem',
            '29': '7.25rem',
            '30': '7.5rem',
            '42': '10.5rem',
            '45': '11.25rem',
            '51': '12.75rem',
            '54': '13.5rem',
            '70': '17.5rem',
            '83.5': '20.875rem',
            '98.25': '24.5625rem',
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
        backdropBlur: {
            xs: '2px',
        },
        boxShadow: {
            soft: `0 0.5px 2.2px 0px #00000008, 0 1.2px 5.3px 0px #0000000b, \
0 2.3px 10px 0 #0000000e, 0 4px 18px 0 #00000011, 0 7.5px 33.4px 0 #00000014, \
0 18px 80px 0 #0000001c`,
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
            'fill-75': 'repeat(auto-fill, minmax(18.75rem, 1fr))',
        },
    },
}
