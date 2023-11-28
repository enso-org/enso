/** @file Configuration for Tailwind. */

export const content = ['./src/**/*.tsx', './src/**/*.ts']
export const important = `:is(.enso-dashboard, .enso-chat)`
export const theme = {
    extend: {
        colors: {
            /** The default color of all text. */
            // This should be named "regular".
            primary: 'rgb(0 0 0 / 0.6)',
            'not-selected': 'rgb(0 0 0 / 0.4)',
            'icon-selected': 'rgb(0 0 0 / 0.5)',
            'icon-not-selected': 'rgb(0 0 0 / 0.3)',
            chat: '#484848',
            'ide-bg': '#ebeef1',
            'ide-bg-dark': '#d0d3d6',
            selected: 'rgb(255 255 255 / 0.4)',
            // Should be `#3e515f14`, but `bg-opacity` does not work with RGBA.
            label: '#f0f1f3',
            help: '#3f68ce',
            invite: '#0e81d4',
            cloud: '#0666be',
            share: '#64b526',
            inversed: '#ffffff',
            green: '#3e8b29',
            delete: 'rgb(243 24 10 / 0.87)',
            v3: '#252423',
            youtube: '#c62421',
            discord: '#404796',
            dim: 'rgb(0 0 0 / 0.25)',
            frame: 'rgb(255 255 255 / 0.4)',
            'frame-selected': 'rgb(255 255 255 / 0.7)',
            'tag-text': 'rgb(255 255 255 / 0.9)',
            'tag-text-2': 'rgb(0 0 0 / 0.6)',
            'permission-owner': 'rgb(236 2 2 / 0.7)',
            'permission-admin': 'rgb(252 60 0 / 0.7)',
            'permission-edit': 'rgb(255 138 0 / 0.9)',
            'permission-read': 'rgb(152 174 18 / 0.8)',
            'permission-docs': 'rgb(91 8 226 / 0.64)',
            'permission-exec': 'rgb(236 2 2 / 0.7)',
            'permission-view': 'rgb(0 0 0 / 0.1)',
            'label-running-project': '#257fd2',
            'label-low-resources': '#ff6b18',
            'call-to-action': '#fa6c08',
            'gray-350': '#b7bcc5',
        },
        fontSize: {
            xs: '0.71875rem',
            sm: '0.8125rem',
            xl: '1.1875rem',
            '4xl': '2.375rem',
        },
        borderRadius: {
            '4xl': '2rem',
        },
        lineHeight: {
            '144.5': '144.5%',
            '170': '170%',
        },
        spacing: {
            '0.75': '0.1875rem',
            '1.25': '0.3125rem',
            '1.75': '0.4375rem',
            '2.25': '0.5625rem',
            '3.25': '0.8125rem',
            '3.5': '0.875rem',
            '4.5': '1.125rem',
            '4.75': '1.1875rem',
            '5.5': '1.375rem',
            '6.5': '1.625rem',
            '9.5': '2.375rem',
            '9.75': '2.4375rem',
            '13': '3.25rem',
            '18': '4.5rem',
            '25': '6.25rem',
            '29': '7.25rem',
            '30': '7.5rem',
            '30.25': '7.5625rem',
            '42': '10.5rem',
            '45': '11.25rem',
            '51': '12.75rem',
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
            '31.5': '7.875rem',
            '33.25': '8.3125rem',
            '40': '10rem',
            '61.25': '15.3125rem',
            '80': '20rem',
            '96': '24rem',
        },
        opacity: {
            '1/3': '.33333333',
        },
        zIndex: {
            '1': '1',
            '3': '3',
        },
        backdropBlur: {
            xs: '2px',
        },
        borderWidth: { '0.5': '0.5px' },
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
