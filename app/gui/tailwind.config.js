/** @file Configuration for Tailwind. */
import { fileURLToPath } from 'node:url'
import animate from 'tailwindcss-animate'
import reactAriaComponents from 'tailwindcss-react-aria-components'
import plugin from 'tailwindcss/plugin.js'

const HERE_PATH = fileURLToPath(new URL('.', import.meta.url))

export default /** @satisfies {import('tailwindcss').Config} */ ({
  content: [`${HERE_PATH}/src/**/*.tsx`, `${HERE_PATH}/src/**/*.ts`],
  theme: {
    extend: {
      cursor: {
        unset: 'unset',
      },
      colors: {
        // While these COULD ideally be defined as CSS variables, then their opacity cannot be
        // modified.
        /** The default color of all text. */
        // This should be named "regular".
        primary: 'rgb(var(--color-primary-rgb) / var(--color-primary-opacity))',
        invert: 'rgb(var(--color-invert-rgb) / var(--color-invert-opacity))',
        accent: 'rgb(var(--color-accent-rgb) / 100%)',
        danger: 'rgb(var(--color-danger-rgb) / 100%)',
        'accent-dark': '#3e9152',
        'hover-bg': 'rgb(0 0 0 / 10%)',
        frame: 'rgb(255 255 255 / 40%)',
        'selected-frame': 'rgb(255 255 255 / 70%)',
        'ide-bg': '#ebeef1',
        selected: 'rgb(255 255 255 / 40%)',
        'not-selected': 'rgb(0 0 0 / 30%)',
        // Should be `#3e515f14`, but `bg-opacity` does not work with RGBA.
        label: '#f0f1f3',
        help: '#3f68ce',
        invite: '#0e81d4',
        share: '#64b526',
        inversed: '#ffffff',
        green: '#3e8b29',
        delete: 'rgb(243 24 10 / 87%)',
        v3: '#252423',
        youtube: '#c62421',
        discord: '#404796',
        'selection-brush': 'lch(70% 0 0 / 50%)',
        dim: 'rgb(0 0 0 / 25%)',
        'dim-darker': 'rgb(0 0 0 / 40%)',
        'tag-text': 'rgb(255 255 255 / 90%)',
        'tag-text-2': 'rgb(0 0 0 / 60%)',
        'permission-owner': 'rgb(236 2 2 / 70%)',
        'permission-admin': 'rgb(252 60 0 / 70%)',
        'permission-edit': 'rgb(255 138 0 / 90%)',
        'permission-read': 'rgb(152 174 18 / 80%)',
        'permission-docs': 'rgb(91 8 226 / 64%)',
        'permission-exec': 'rgb(236 2 2 / 70%)',
        'permission-view': 'rgb(0 0 0 / 10%)',
        'label-running-project': '#257fd2',
        'label-low-resources': '#ff6b18',
        'call-to-action': 'rgb(250 108 8)',
        'black-a5': 'rgb(0 0 0 / 5%)',
        'black-a10': 'rgb(0 0 0 / 10%)',
        'black-a16': 'rgb(0 0 0 / 16%)',
        'black-a30': 'rgb(0 0 0 / 30%)',
        'black-a50': 'rgb(0 0 0 / 50%)',
        'gray-350': '#b7bcc5',
      },
      fontFamily: {
        naming: ['"Enso Naming"', '"Enso"', '"M PLUS 1"'],
      },
      fontSize: {
        '2xs': '10.5px',
        xs: '11.5px',
        sm: '13px',
        xl: '19px',
        '3xl': '32px',
        '4xl': '38px',
        'auth-heading': 'var(--auth-heading-font-size)',
      },
      borderRadius: {
        inherit: 'inherit',
        '2.5xl': '1.25rem',
        '4xl': '2rem',
        default: 'var(--default-corner-radius)',
        'button-focus-ring': 'var(--button-focus-ring-corner-radius)',
        auth: 'var(--auth-corner-radius)',
        input: 'var(--input-corner-radius)',
        'permission-type-selector': 'var(--permission-type-selector-corner-radius)',
        'menu-entry': 'var(--menu-entry-corner-radius)',
        'selection-brush': 'var(--selection-brush-corner-radius)',
        'chat-input': 'var(--chat-input-corner-radius)',
        'small-rectangle-button': 'var(--small-rectangle-button-corner-radius)',
      },
      spacing: {
        DEFAULT: '0',
        'project-icon': 'var(--project-icon-size)',
        'profile-picture-large': 'var(--profile-picture-large-size)',
        'radio-button': 'var(--radio-button-size)',
        'radio-button-dot': 'var(--radio-button-dot-size)',
        'extended-editor-menu': 'var(--extended-editor-menu-size)',
        'plus-icon': 'var(--plus-icon-size)',
        'chat-profile-picture': 'var(--chat-profile-picture-size)',
        'selection-brush-border': 'var(--selection-brush-border-width)',
        'row-h': 'var(--row-height)',
        'text-h': 'var(--text-height)',
        'asset-panel-w': 'var(--asset-panel-width)',
        'indent-1': 'var(--indent-1-size)',
        'indent-2': 'var(--indent-2-size)',
        'indent-3': 'var(--indent-3-size)',
        'indent-4': 'var(--indent-4-size)',
        'indent-5': 'var(--indent-5-size)',
        'indent-6': 'var(--indent-6-size)',
        'indent-7': 'var(--indent-7-size)',
        'indent-8': 'var(--indent-8-size)',
        'indent-9': 'var(--indent-9-size)',
        'indent-10': 'var(--indent-10-size)',
      },
      width: {
        container: '100cqw',
        'profile-picture-caption': 'var(--profile-picture-caption-width)',
        'settings-main-section': 'var(--settings-main-section-width)',
        'user-account-settings-label': 'var(--user-account-settings-label-width)',
        'change-password-settings-label': 'var(--change-password-settings-label-width)',
        'organization-settings-label': 'var(--organization-settings-label-width)',
        'backend-switcher-option': 'var(--backend-switcher-option-width)',
        'side-panel': 'var(--side-panel-width)',
        'context-menu': 'var(--context-menu-width)',
        'context-menu-macos': 'var(--context-menu-macos-width)',
        'json-schema-text-input': 'var(--json-schema-text-input-width)',
        'json-schema-object-key': 'var(--json-schema-object-key-width)',
        'json-schema-dropdown-title': 'var(--json-schema-dropdown-title-width)',
        'asset-search-bar': 'var(--asset-search-bar-width)',
        'asset-search-bar-wide': 'var(--asset-search-bar-wide-width)',
        chat: 'var(--chat-width)',
        'chat-indicator': 'var(--chat-indicator-width)',
        'modal-label': 'var(--modal-label-width)',
        'settings-sidebar': 'var(--settings-sidebar-width)',
        'asset-panel': 'var(--asset-panel-width)',
        'permission-type-selector': 'var(--permission-type-selector-width)',
        'drive-sidebar': 'var(--drive-sidebar-width)',
        'permission-type': 'var(--permission-type-width)',
        'auth-icon-container': 'var(--auth-icon-container-width)',
        'side-panel-label': 'var(--side-panel-label-width)',
        'date-picker': 'var(--date-picker-width)',
        'date-cell': 'var(--date-cell-width)',

        'manage-labels-modal': 'var(--manage-labels-modal-width)',
        'confirm-delete-modal': 'var(--confirm-delete-modal-width)',
        'confirm-delete-user-modal': 'var(--confirm-delete-user-modal-width)',
        'duplicate-assets-modal': 'var(--duplicate-assets-modal-width)',
        'capture-keyboard-shortcut-modal': 'var(--capture-keyboard-shortcut-modal-width)',
        'new-label-modal': 'var(--new-label-modal-width)',
        'invite-users-modal': 'var(--invite-users-modal-width)',
        'manage-permissions-modal': 'var(--manage-permissions-modal-width)',
        'upsert-data-link-modal': 'var(--upsert-data-link-modal-width)',
        'upsert-data-link-modal-max': 'var(--upsert-data-link-modal-max-width)',
        'upsert-secret-modal': 'var(--upsert-secret-modal-width)',

        'members-name-column': 'var(--members-name-column-width)',
        'members-email-column': 'var(--members-email-column-width)',
        'keyboard-shortcuts-icon-column': 'var(--keyboard-shortcuts-icon-column-width)',
        'keyboard-shortcuts-name-column': 'var(--keyboard-shortcuts-name-column-width)',
        'keyboard-shortcuts-description-column':
          'var(--keyboard-shortcuts-description-column-width)',
        'drive-name-column': 'var(--drive-name-column-width)',
        'drive-modified-column': 'var(--drive-modified-column-width)',
        'drive-shared-with-column': 'var(--drive-shared-with-column-width)',
        'drive-labels-column': 'var(--drive-labels-column-width)',
        'drive-accessed-by-projects-column': 'var(--drive-accessed-by-projects-column-width)',
        'drive-accessed-data-column': 'var(--drive-accessed-data-column-width)',
        'drive-docs-column': 'var(--drive-docs-column-width)',
      },
      minWidth: ({ theme }) => ({ .../** @type {{}}*/ (theme('width')) }),
      maxWidth: ({ theme }) => ({ .../** @type {{}}*/ (theme('width')) }),
      height: {
        row: 'var(--row-height)',
        'table-row': 'var(--table-row-height)',
        text: 'var(--text-height)',
        heading: 'var(--heading-height)',
        'news-item': 'var(--news-item-height)',
        sample: 'var(--sample-height)',
        'sample-image': 'var(--sample-image-height)',
        'sample-info': 'var(--sample-info-height)',
        'side-panel-heading': 'var(--side-panel-heading-height)',
        'chat-thread-list': 'var(--chat-thread-list-height)',
        'payment-form': 'var(--payment-form-height)',
        'paragraph-input': 'var(--paragraph-input-height)',
        'dropdown-items': 'var(--dropdown-items-height)',
        'manage-permissions-modal-permissions-list':
          'var(--manage-permissions-modal-permissions-list-height)',
        'manage-labels-list': 'var(--manage-labels-list-height)',
        'search-suggestions-list': 'var(--search-suggestions-list-height)',
      },
      minHeight: ({ theme }) => ({ .../** @type {{}}*/ (theme('height')) }),
      maxHeight: ({ theme }) => ({ .../** @type {{}}*/ (theme('height')) }),
      opacity: {
        full: '100%',
        unimportant: 'var(--unimportant-opacity)',
      },
      gap: {
        modal: 'var(--modal-gap)',
        subheading: 'var(--subheading-gap)',
        icons: 'var(--icons-gap)',
        colors: 'var(--colors-gap)',
        'samples-icon-with-text': 'var(--samples-icon-with-text-gap)',
        'icon-with-text': 'var(--icon-with-text-gap)',
        'input-with-button': 'var(--input-with-button-gap)',
        'user-bar': 'var(--user-bar-gap)',
        'top-bar': 'var(--top-bar-gap)',
        'top-bar-right': 'var(--top-bar-right-gap)',
        auth: 'var(--auth-gap)',
        'auth-link': 'var(--auth-link-gap)',
        'drive-sidebar': 'var(--drive-sidebar-gap)',
        home: 'var(--home-gap)',
        drive: 'var(--drive-gap)',
        'status-page': 'var(--status-page-gap)',
        'menu-entry': 'var(--menu-entry-gap)',
        'news-items': 'var(--news-items-gap)',
        sample: 'var(--sample-gap)',
        samples: 'var(--samples-gap)',
        settings: 'var(--settings-gap)',
        'settings-header': 'var(--settings-header-gap)',
        'settings-section': 'var(--settings-section-gap)',
        'settings-subsection': 'var(--settings-subsection-gap)',
        'settings-section-header': 'var(--settings-section-header-gap)',
        'settings-entry': 'var(--settings-entry-gap)',
        'settings-sidebar': 'var(--settings-sidebar-gap)',
        'new-empty-project': 'var(--new-empty-project-gap)',
        modifiers: 'var(--modifiers-gap)',
        'modifiers-macos': 'var(--modifiers-macos-gap)',
        'side-panel': 'var(--side-panel-gap)',
        'side-panel-section': 'var(--side-panel-section-gap)',
        'asset-search-bar': 'var(--asset-search-bar-gap)',
        'drive-bar': 'var(--drive-bar-gap)',
        'column-items': 'var(--column-items-gap)',
        labels: 'var(--labels-gap)',
        'label-icons': 'var(--label-icons-gap)',
        'user-menu': 'var(--user-menu-gap)',
        'user-permission': 'var(--user-permission-gap)',
        'name-column-icon': 'var(--name-column-icon-gap)',
        'permission-type-button': 'var(--permission-type-button-gap)',
        'modal-tabs': 'var(--modal-tabs-gap)',
        'dropdown-arrow': 'var(--dropdown-arrow-gap)',
        'context-menus': 'var(--context-menus-gap)',
        'asset-panel': 'var(--asset-panel-gap)',
        'search-suggestions': 'var(--search-suggestions-gap)',
        'keyboard-shortcuts-button': 'var(--keyboard-shortcuts-button-gap)',
        'chat-buttons': 'var(--chat-buttons-gap)',
      },
      padding: {
        'top-bar': 'var(--top-bar-padding)',
        modal: 'var(--modal-padding)',
        'modal-wide': 'var(--modal-wide-padding)',
        auth: 'var(--auth-padding)',
        'page-x': 'var(--page-padding-x)',
        'heading-x': 'var(--heading-padding-x)',
        'heading-y': 'var(--heading-padding-y)',
        'auth-input-y': 'var(--auth-input-padding-y)',
        'auth-input-r': 'var(--auth-input-padding-right)',
        'auth-link-x': 'var(--auth-link-padding-x)',
        'auth-link-y': 'var(--auth-link-padding-y)',
        'text-link-x': 'var(--text-link-padding-x)',
        'drive-sidebar-y': 'var(--drive-sidebar-padding-y)',
        'radio-button-dot': 'var(--radio-button-dot-padding)',
        'chat-y': 'chat-padding-y',
        'cell-x': 'var(--cell-padding-x)',
        'button-x': 'var(--button-padding-x)',
        'icons-x': 'var(--icons-padding-x)',
        'drive-bar-y': 'var(--drive-bar-padding-y)',
        'selector-x': 'var(--selector-padding-x)',
        'selector-y': 'var(--selector-padding-y)',
        'menu-entry': 'var(--menu-entry-padding)',
        'context-menu-entry-x': 'var(--context-menu-entry-padding-x)',
        'profile-picture-caption-y': 'var(--profile-picture-caption-padding-y)',
        'delete-user-account-button-x': 'var(--delete-user-account-button-padding-x)',
        'context-menu': 'var(--context-menu-padding)',
        'input-x': 'var(--input-padding-x)',
        'multiline-input': 'var(--multiline-input-padding)',
        'json-schema-object-input': 'var(--json-schema-object-input-padding)',
        'name-column-x': 'var(--name-column-padding-x)',
        'name-column-y': 'var(--name-column-padding-y)',
        'home-page-b': 'var(--home-page-padding-b)',
        version: 'var(--version-padding)',
        'label-x': 'var(--label-padding-x)',
        'sidebar-section-heading-x': 'var(--sidebar-section-heading-padding-x)',
        'sidebar-section-heading-y': 'var(--sidebar-section-heading-padding-y)',
        'permission-type-selector': 'var(--permission-type-selector-padding)',
        'permission-type-button': 'var(--permission-type-button-padding)',
        'permission-type-y': 'var(--permission-type-padding-y)',
        'modal-tab-bar-x': 'var(--modal-tab-bar-padding-x)',
        'manage-permissions-modal-input': 'var(--manage-permissions-modal-input-padding)',
        'modal-invite-button-text-y': 'var(--modal-invite-button-text-padding-y)',
        'home-section-x': 'var(--home-section-padding-x)',
        'sample-description-x': 'var(--sample-description-padding-x)',
        'sample-description-t': 'var(--sample-description-padding-top)',
        'sample-description-b': 'var(--sample-description-padding-bottom)',
        'news-item-description': 'var(--news-item-description-padding)',
        'news-item-subtitle-y': 'var(--news-item-subtitle-padding-y)',
        'font-awesome-icon-x': 'var(--font-awesome-icon-padding-x)',
        'search-suggestions': 'var(--search-suggestions-padding)',
        'search-suggestion-y': 'var(--search-suggestion-padding-y)',
        'side-panel-heading-y': 'var(--side-panel-heading-padding-y)',
        'icon-column-r': 'var(--icon-column-padding-right)',
        'asset-panel-l': 'var(--asset-panel-padding-left)',
        'auth-icon-container-w': 'var(--auth-icon-container-width)',
        'side-panel-description-y': 'var(--side-panel-description-padding-y)',
        'date-input': 'var(--date-input-padding)',
        'chat-thread-button': 'var(--chat-thread-button-padding)',
        'chat-form': 'var(--chat-form-padding)',
        'chat-input': 'var(--chat-input-padding)',
        'chat-button-x': 'var(--chat-button-padding-x)',
        'chat-button-y': 'var(--chat-button-padding-y)',
        'chat-reaction-bar-y': 'var(--chat-reaction-bar-padding-y)',
        'chat-reaction': 'var(--chat-reaction-padding)',
        'missing-functionality-text-x': 'var(--missing-functionality-text-padding-x)',
      },
      margin: {
        'name-column-icon': 'var(--name-column-icon-margin)',
        'button-px': 'var(--button-padding-x)',
        'tick-cross-button': 'var(--tick-cross-button-margin)',
        'search-suggestion': 'var(--search-suggestion-margin)',
        'multiline-input-p': 'var(--multiline-input-padding)',
        'close-icon': 'var(--close-icon-margin)',
        'date-input-gap': 'var(--date-input-gap)',
        'date-input-calendar-gap': 'var(--date-input-calendar-gap)',
        'chat-header-x': 'var(--chat-header-margin-x)',
        'chat-header-t': 'var(--chat-header-margin-top)',
        'chat-form-x': 'var(--chat-form-margin-x)',
        'chat-form-y': 'var(--chat-form-margin-y)',
        'chat-message-x': 'var(--chat-message-margin-x)',
        'chat-message-y': 'var(--chat-message-margin-y)',
        'chat-profile-picture-y': 'var(--chat-profile-picture-margin-y)',
        'chat-message-info-x': 'var(--chat-message-info-margin-x)',
        'chat-reaction-bar-py': 'var(--chat-reaction-bar-padding-y)',
        'chat-reaction-bar': 'var(--chat-reaction-bar-margin)',
        'chat-reaction': 'var(--chat-reaction-margin)',
        'separator-y': 'var(--separator-margin-y)',
        'sidebar-section-heading-b': 'var(--sidebar-section-heading-margin-b)',
        'context-menu-entry-px': 'var(--context-menu-entry-padding-x)',
        'text-link-px': 'var(--text-link-padding-x)',
      },
      lineHeight: {
        snug: 'var(--snug-line-height)',
        cozy: 'var(--cozy-line-height)',
        'chat-thread-title': 'var(--chat-thread-title-line-height)',
      },
      zIndex: {
        1: '1',
        spotlight: '2',
        tooltip: '3',
      },
      backdropBlur: {
        xs: '2px',
        default: 'var(--default-backdrop-blur)',
      },
      borderWidth: {
        0.5: '0.5px',
        'selection-brush': 'var(--selection-brush-border-width)',
      },
      boxShadow: {
        soft: `0 0.5px 2.2px 0px #00000008, 0 1.2px 5.3px 0px #0000000b, \
0 2.3px 10px 0 #0000000e, 0 4px 18px 0 #00000011, 0 7.5px 33.4px 0 #00000014, \
0 18px 80px 0 #0000001c`,
        softer: `0 0.5px 2.2px 0px rgb(0 0 0 / 0.84%), 0 1.2px 5.65px 0px rgb(0 0 0 / 1.21%), \
0 2.25px 10.64px 0 rgb(0 0 0 / 1.5%), 0 4px 19px 0 rgb(0 0 0 / 1.79%), 0 7.5px 35.5px 0 rgb(0 0 0 / 2.16%), \
0 18px 85px 0 rgb(0 0 0 / 3%)`,
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
        'caret-blink': 'caret-blink 1.5s ease-out infinite',
        'spin-ease': 'spin cubic-bezier(0.67, 0.33, 0.33, 0.67) 1.5s infinite',
        'appear-delayed': 'appear-delayed 0.5s ease-in-out',
      },
      transitionProperty: {
        width: 'width',
        'min-width': 'min-width',
        'stroke-dasharray': 'stroke-dasharray',
        'grid-template-rows': 'grid-template-rows',
        'border-margin': 'border, margin',
      },
      transitionDuration: {
        DEFAULT: '100ms',
        auth: 'var(--auth-transition-duration)',
        'side-panel': 'var(--side-panel-transition-duration)',
        arrow: 'var(--arrow-transition-duration)',
        'user-menu': 'var(--user-menu-transition-duration)',
        'spinner-fast': 'var(--spinner-fast-transition-duration)',
        'spinner-medium': 'var(--spinner-medium-transition-duration)',
        'spinner-slow': 'var(--spinner-slow-transition-duration)',
      },
      gridTemplateRows: {
        '0fr': '0fr',
        '1fr': '1fr',
      },
      gridTemplateColumns: {
        '0fr': '0fr',
        '1fr': '1fr',
        'fill-news-items': 'repeat(auto-fill, minmax(var(--news-items-column-width), 1fr))',
        'fill-samples': 'repeat(auto-fill, minmax(var(--samples-column-width), 1fr))',
      },
      dashArray: {
        5: '5-12',
        75: '75-12',
        100: '100-12',
      },
      keyframes: {
        'appear-delayed': {
          '0%': { opacity: '0' },
          '99%': { opacity: '0' },
          '100%': { opacity: '1' },
        },
        'caret-blink': {
          '0%,70%,100%': { opacity: '1' },
          '20%,50%': { opacity: '0' },
        },
      },
    },
  },
  plugins: [
    reactAriaComponents,
    animate,
    plugin(({ addVariant, addUtilities, matchUtilities, addComponents, theme }) => {
      addVariant('group-hover-2', ['.group:where([data-hovered]) &', '.group:where(:hover) &'])

      addUtilities({
        '.scrollbar-gutter-stable': {
          scrollbarGutter: 'stable',
        },
      })

      addUtilities(
        {
          '.container-size': {
            containerType: 'size',
          },
          '.pointer-events-none-recursive': {
            pointerEvents: 'none',
            '*': { pointerEvents: 'none' },
          },
          '.clip-path-0': {
            clipPath: 'inset(0)',
          },
          '.clip-path-top': {
            clipPath: 'polygon(0 0, 100% 0, 100% calc(50% - 1px), 0 calc(50% - 1px))',
          },
          '.clip-path-bottom': {
            clipPath: `polygon(0 calc(50% + 1px), 100% calc(50% + 1px), 100% 100%, 0 100%)`,
          },
          '.clip-path-bottom-shadow': {
            clipPath: `polygon(0 0, 100% 0, 100% calc(100% + 100vh), 0 calc(100% + 100vh))`,
          },
          '.clip-path-left-shadow': {
            clipPath: `polygon(-100vw 0, 100% 0, 100% 100%, -100vw 100%)`,
          },
          '.scroll-hidden': {
            MsOverflowStyle: 'none' /* Internet Explorer 10+ */,
            scrollbarWidth: 'none' /* Firefox */,
            '&::-webkit-scrollbar': {
              display: 'none' /* Safari and Chrome */,
            },
          },

          // === States ===

          '.focus-ring, .focus-ring:focus, .focus-ring-outset, .focus-ring-outset:focus': {
            '@apply outline outline-2 -outline-offset-2 outline-primary transition-all': '',
          },
          '.focus-ring.checkbox, .focus-ring-outset, .focus-ring-outset:focus': {
            '@apply outline-offset-0': '',
          },
          '.drop-target-after': {
            '@apply relative after:pointer-events-none after:absolute after:inset after:rounded-inherit [&[data-drop-target=true]]:after:bg-primary/10':
              '',
          },

          // === Classes affecting opacity ===

          '.selectable': {
            '@apply disabled:opacity-30 [&.disabled]:opacity-30 disabled:cursor-not-allowed [&.disabled]:cursor-not-allowed opacity-50 hover:opacity-75 transition-all':
              '',
          },
          '.active': {
            '@apply opacity-100 disabled:opacity-100 [&.disabled]:opacity-100 hover:opacity-100 disabled:cursor-default [&.disabled]:cursor-default':
              '',
          },
          '.placeholder': {
            '@apply opacity-75': '',
          },
          '.read-only': {
            '@apply opacity-75 cursor-not-allowed': '',
          },

          '.scroll-offset-edge-s': {
            '--scrollbar-offset-edge': '2px',
          },
          '.scroll-offset-edge-m': {
            '--scrollbar-offset-edge': '4px',
          },
          '.scroll-offset-edge-l': {
            '--scrollbar-offset-edge': '6px',
          },
          '.scroll-offset-edge-xl': {
            '--scrollbar-offset-edge': '8px',
          },
          '.scroll-offset-edge-2xl': {
            '--scrollbar-offset-edge': '16px',
          },
          '.scroll-offset-edge-3xl': {
            '--scrollbar-offset-edge': '24px',
          },
          '.scroll-offset-edge-4xl': {
            '--scrollbar-offset-edge': '28px',
          },
          '.scroll-offset-edge-5xl': {
            '--scrollbar-offset-edge': '32px',
          },
          '.scroll-offset-edge-6xl': {
            '--scrollbar-offset-edge': '36px',
          },
          '.scroll-offset-edge-7xl': {
            '--scrollbar-offset-edge': '40px',
          },
          '.scroll-offset-edge-8xl': {
            '--scrollbar-offset-edge': '44px',
          },
          '.scroll-offset-edge-9xl': {
            '--scrollbar-offset-edge': '48px',
          },
          '.scroll-offset-edge-10xl': {
            '--scrollbar-offset-edge': '52px',
          },

          // === Visbility classes ===

          '.visibility-visible': {},
          '.visibility-hidden': {
            '@apply hidden': '',
          },
          '.visibility-faded': {
            '@apply opacity-50 pointer-events-none-recursive': '',
          },

          // === Rounded rows ===

          '.rounded-rows': {
            [`:where(
              & > tbody > tr:nth-child(odd of .rounded-rows-child) > td:not(.rounded-rows-skip-level),
              & > tbody > tr:nth-child(odd of .rounded-rows-child) > td.rounded-rows-skip-level > *
            )`]: {
              backgroundColor: `rgb(0 0 0 / 3%)`,
            },
            [`:where(
              & > tbody > tr.rounded-rows-child.selected > td:not(.rounded-rows-skip-level),
              & > tbody > tr.rounded-rows-child.selected > td.rounded-rows-skip-level > *
            )`]: {
              backgroundColor: 'rgb(255 255 255 / 90%)',
            },
            [`:where(
              & > tbody > tr.rounded-rows-child[data-drop-target] > td:not(.rounded-rows-skip-level),
              & > tbody > tr.rounded-rows-child[data-drop-target] > td.rounded-rows-skip-level > *
            )`]: {
              backgroundColor: 'rgb(0 0 0 / 8%)',
            },
          },

          // === Custom column spans ===

          '.col-span-2-news-item.col-span-2-news-item': {
            '@media screen and (max-width: 40.5625rem)': {
              gridColumn: 'span 1 / span 1',
            },
          },
        },
        {
          respectPrefix: true,
          respectImportant: true,
        },
      )

      /** One revolution, in radians. */
      const revolution = Math.PI * 2
      matchUtilities(
        {
          // Values must be pre-computed, because FF does not support `calc()` in `stroke-dasharray`.
          // calc(12 * 0.05 * 6.2832) calc(12 * 6.2832)
          dasharray: (value) => {
            const [percentage = 0, radius = 0] = value.split('-').map((part) => Number(part) || 0)
            return {
              strokeDasharray: `${radius * (percentage / 100) * revolution} ${
                percentage === 1 ? 0 : radius * revolution
              }`,
            }
          },
        },
        {
          respectPrefix: true,
          respectImportant: true,
          values: theme('dashArray', {}),
        },
      )

      matchUtilities(
        {
          'translate-z': (value) => ({
            '--tw-translate-z': value,
            transform: ` translate3d(var(--tw-translate-x), var(--tw-translate-y), var(--tw-translate-z)) rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))`,
          }),
        },
        { values: theme('translate', {}), supportsNegativeValues: true },
      )

      addComponents(
        {
          '.button': {
            '@apply inline-block rounded-full px-4 py-1 selectable': '',
          },
          '.icon-with-text': {
            '@apply flex items-center justify-center gap-icon-with-text': '',
          },

          // === Text-related classes ===

          '.text': {
            '@apply leading-cozy h-text py-px': '',
          },
          '.text-tight': {
            '@apply leading-snug h-5 py-px': '',
          },
          '.text-header': {
            '@apply leading-snug h-text py-0.5': '',
          },
          '.text-subheading': {
            '@apply text-xl leading-snug py-0.5': '',
          },
        },
        {
          respectPrefix: true,
          respectImportant: true,
        },
      )

      addVariant('not-focus', '&:where([data-rac]):not([data-focused])')
      addVariant('not-selected', '&:where([data-rac]):not([data-selected])')
    }),
  ],
})
