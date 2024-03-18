/** @file Configuration for Tailwind. */
import plugin from 'tailwindcss/plugin.js'

// The names come from a third-party API and cannot be changed.
/* eslint-disable no-restricted-syntax, @typescript-eslint/naming-convention, @typescript-eslint/no-magic-numbers */
export default /** @satisfies {import('tailwindcss').Config} */ ({
  content: ['./src/**/*.tsx', './src/**/*.ts'],
  important: `:is(.enso-dashboard, .enso-chat)`,
  theme: {
    extend: {
      colors: {
        // While these COULD ideally be defined as CSS variables, then their opacity cannot be
        // modified.
        /** The default color of all text. */
        // This should be named "regular".
        primary: 'rgb(0 0 0 / 60%)',
        'hover-bg': 'rgb(0 0 0 / 10%)',
        frame: 'var(--frame-color)',
        'selected-frame': 'var(--selected-frame-color)',
        'not-selected': 'rgb(0 0 0 / 40%)',
        'icon-selected': 'rgb(0 0 0 / 50%)',
        'icon-not-selected': 'rgb(0 0 0 / 30%)',
        'ide-bg': '#ebeef1',
        selected: 'rgb(255 255 255 / 40%)',
        // Should be `#3e515f14`, but `bg-opacity` does not work with RGBA.
        label: '#f0f1f3',
        help: '#3f68ce',
        invite: '#0e81d4',
        cloud: '#0666be',
        share: '#64b526',
        inversed: '#ffffff',
        green: '#3e8b29',
        delete: 'rgba(243 24 10 / 87%)',
        v3: '#252423',
        youtube: '#c62421',
        discord: '#404796',
        danger: '#d33b0b',
        // Equivalent to `lch(70% 0 0 / 0.5)`
        'selection-brush': 'var(--selection-brush-color)',
        dim: 'rgba(0, 0, 0, 0.25)',
        'dim-darker': 'rgba(0, 0, 0, 0.40)',
        'tag-text': 'rgba(255, 255, 255, 0.90)',
        'tag-text-2': 'rgba(0, 0, 0, 0.60)',
        'permission-owner': 'rgba(236, 2, 2, 0.70)',
        'permission-admin': 'rgba(252, 60, 0, 0.70)',
        'permission-edit': 'rgba(255, 138, 0, 0.90)',
        'permission-read': 'rgba(152, 174, 18, 0.80)',
        'permission-docs': 'rgba(91, 8, 226, 0.64)',
        'permission-exec': 'rgba(236, 2, 2, 0.70)',
        'permission-view': 'rgba(0, 0, 0, 0.10)',
        'label-running-project': '#257fd2',
        'label-low-resources': '#ff6b18',
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
        xl: '1.1875rem',
        '4xl': '2.375rem',
        'auth-heading': 'var(--auth-heading-font-size)',
      },
      borderRadius: {
        '2.5xl': '1.25rem',
        '4xl': '2rem',
        default: 'var(--default-corner-radius)',
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
        icon: 'var(--icon-size)',
        'project-icon': 'var(--project-icon-size)',
        'profile-picture': 'var(--profile-picture-size)',
        'profile-picture-large': 'var(--profile-picture-large-size)',
        'radio-button': 'var(--radio-button-size)',
        'radio-button-dot': 'var(--radio-button-dot-size)',
        'extended-editor-menu': 'var(--extended-editor-menu-size)',
        'plus-icon': 'var(--plus-icon-size)',
        'chat-profile-picture': 'var(--chat-profile-picture-size)',
        'selection-brush-border': 'var(--selection-brush-border-width)',
        'row-h': 'var(--row-height)',
        'text-h': 'var(--text-height)',
        'top-bar-margin': 'var(--top-bar-margin)',
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
        'permission-display': 'var(--permission-display-width)',
        chat: 'var(--chat-width)',
        'chat-indicator': 'var(--chat-indicator-width)',
        'user-menu': 'var(--user-menu-width)',
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
        'upsert-secret-modal': 'var(--upsert-secret-modal-width)',

        'members-name-column': 'var(--members-name-column-width)',
        'members-email-column': 'var(--members-email-column-width)',
        'keyboard-shortcuts-icon-column': 'var(--keyboard-shortcuts-icon-column-width)',
        'keyboard-shortcuts-name-column': 'var(--keyboard-shortcuts-name-column-width)',
        'activity-log-icon-column': 'var(--activity-log-icon-column-width)',
        'activity-log-type-column': 'var(--activity-log-type-column-width)',
        'activity-log-email-column': 'var(--activity-log-email-column-width)',
        'activity-log-timestamp-column': 'var(--activity-log-timestamp-column-width)',
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
        text: 'var(--text-height)',
        heading: 'var(--heading-height)',
        'news-item': 'var(--news-item-height)',
        sample: 'var(--sample-height)',
        'sample-image': 'var(--sample-image-height)',
        'sample-info': 'var(--sample-info-height)',
        'side-panel-heading': 'var(--side-panel-heading-height)',
        'chat-thread-list': 'var(--chat-thread-list-height)',
        'payment-form': 'var(--payment-form-height)',
        'drive-table-heading': 'var(--drive-table-heading-height)',
        'paragraph-input': 'var(--paragraph-input-height)',
        'autocomplete-suggestions': 'var(--autocomplete-suggestions-height)',
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
        buttons: 'var(--buttons-gap)',
        icons: 'var(--icons-gap)',
        colors: 'var(--colors-gap)',
        'samples-icon-with-text': 'var(--samples-icon-with-text-gap)',
        'icon-with-text': 'var(--icon-with-text-gap)',
        'input-with-button': 'var(--input-with-button-gap)',
        'user-bar': 'var(--user-bar-gap)',
        'top-level': 'var(--top-level-gap)',
        'top-bar': 'var(--top-bar-gap)',
        'top-bar-right': 'var(--top-bar-right-gap)',
        pages: 'var(--pages-gap)',
        auth: 'var(--auth-gap)',
        'auth-link': 'var(--auth-link-gap)',
        'drive-sidebar': 'var(--drive-sidebar-gap)',
        home: 'var(--home-gap)',
        'drive-heading': 'var(--drive-heading-gap)',
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
        banner: 'var(--banner-gap)',
        'new-empty-project': 'var(--new-empty-project-gap)',
        modifiers: 'var(--modifiers-gap)',
        'modifiers-macos': 'var(--modifiers-macos-gap)',
        'side-panel': 'var(--side-panel-gap)',
        'side-panel-section': 'var(--side-panel-section-gap)',
        'json-schema': 'var(--json-schema-gap)',
        'asset-search-bar': 'var(--asset-search-bar-gap)',
        'drive-bar': 'var(--drive-bar-gap)',
        'column-items': 'var(--column-items-gap)',
        labels: 'var(--labels-gap)',
        'label-icons': 'var(--label-icons-gap)',
        'sidebar-section-heading': 'var(--sidebar-section-heading-gap)',
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
        'activity-log-filters': 'var(--activity-log-filters-gap)',
        'activity-log-filter': 'var(--activity-log-filter-gap)',
        'chat-buttons': 'var(--chat-buttons-gap)',
      },
      padding: {
        modal: 'var(--modal-padding)',
        'modal-wide': 'var(--modal-wide-padding)',
        auth: 'var(--auth-padding)',
        'page-x': 'var(--page-padding-x)',
        'heading-x': 'var(--heading-padding-x)',
        'heading-y': 'var(--heading-padding-y)',
        'auth-input-y': 'var(--auth-input-padding-y)',
        'auth-input-r': 'var(--auth-input-padding-right)',
        'drive-sidebar-y': 'var(--drive-sidebar-padding-y)',
        'radio-button-dot': 'var(--radio-button-dot-padding)',
        'chat-y': 'chat-padding-y',
        'cell-x': 'var(--cell-padding-x)',
        'button-x': 'var(--button-padding-x)',
        'icons-x': 'var(--icons-padding-x)',
        'drive-bar-y': 'var(--drive-bar-padding-y)',
        'drive-bar-icons-x': 'var(--drive-bar-icons-padding-x)',
        'selector-x': 'var(--selector-padding-x)',
        'selector-y': 'var(--selector-padding-y)',
        'menu-entry': 'var(--menu-entry-padding)',
        'context-menu-entry-x': 'var(--context-menu-entry-padding-x)',
        'context-menu-separator-y': 'var(--context-menu-separator-padding-y)',
        'profile-picture': 'var(--profile-picture-padding)',
        'banner-x': 'var(--banner-padding-x)',
        'banner-y': 'var(--banner-padding-y)',
        'banner-item': 'var(--banner-item-padding)',
        'profile-picture-caption-y': 'var(--profile-picture-caption-padding-y)',
        'delete-user-account-button-x': 'var(--delete-user-account-button-padding-x)',
        'context-menu': 'var(--context-menu-padding)',
        'input-x': 'var(--input-padding-x)',
        'multiline-input': 'var(--multiline-input-padding)',
        'json-schema-object-input': 'var(--json-schema-object-input-padding)',
        'new-project-button-x': 'var(--new-project-button-padding-x)',
        'name-column-x': 'var(--name-column-padding-x)',
        'name-column-y': 'var(--name-column-padding-y)',
        'home-page-b': 'var(--home-page-padding-b)',
        version: 'var(--version-padding)',
        'label-x': 'var(--label-padding-x)',
        'sidebar-section-heading-x': 'var(--sidebar-section-heading-padding-x)',
        'sidebar-section-heading-y': 'var(--sidebar-section-heading-padding-y)',
        'user-menu': 'var(--user-menu-padding)',
        'extra-columns-panel-x': 'var(--extra-columns-panel-padding-x)',
        'extra-columns-panel-y': 'var(--extra-columns-panel-padding-y)',
        'permission-type-selector': 'var(--permission-type-selector-padding)',
        'permission-type-button': 'var(--permission-type-button-padding)',
        'permission-type-y': 'var(--permission-type-padding-y)',
        'permission-mini-button-x': 'var(--permission-mini-button-padding-x)',
        'permission-mini-button-y': 'var(--permission-mini-button-padding-y)',
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
        'page-switcher-x': 'var(--page-switcher-padding-x)',
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
      },
      margin: {
        'top-bar': 'var(--top-bar-margin)',
        'name-column-icon': 'var(--name-column-icon-margin)',
        'button-px': 'var(--button-padding-x)',
        'permission-with-border': 'var(--permission-with-border-margin)',
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
      },
      lineHeight: {
        snug: 'var(--snug-line-height)',
        cozy: 'var(--cozy-line-height)',
        'chat-thread-title': 'var(--chat-thread-title-line-height)',
      },
      zIndex: {
        1: '1',
        3: '3',
      },
      backdropBlur: {
        xs: '2px',
        default: 'var(--default-backdrop-blur)',
      },
      borderWidth: {
        'selection-brush': 'var(--selection-brush-border-width)',
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
      translate: {
        'context-menu-half-x': 'var(--context-menu-half-width)',
        'context-menu-macos-half-x': 'var(--context-menu-macos-half-width)',
      },
      dashArray: {
        5: '5-12',
        75: '75-12',
        100: '100-12',
      },
    },
  },
  plugins: [
    plugin(({ addUtilities, matchUtilities, addComponents, theme }) => {
      addUtilities(
        {
          '.container-size': {
            containerType: 'size',
          },
          '.pointer-events-none-recursive': {
            pointerEvents: 'none',
            '*': { pointerEvents: 'none' },
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
          '.scroll-hidden': {
            MsOverflowStyle: 'none' /* Internet Explorer 10+ */,
            scrollbarWidth: 'none' /* Firefox */,
            '&::-webkit-scrollbar': {
              display: 'none' /* Safari and Chrome */,
            },
          },

          // === Classes affecting opacity ===

          '.selectable': {
            '@apply disabled:opacity-30 disabled:cursor-not-allowed opacity-50 hover:opacity-75 transition-all':
              '',
          },
          '.active': {
            '@apply opacity-100 disabled:opacity-100 hover:opacity-100 disabled:cursor-default': '',
          },
          '.placeholder': {
            '@apply opacity-75': '',
          },
          '.read-only': {
            '@apply opacity-75 cursor-not-allowed': '',
          },
          '.transparent': {
            '@apply opacity-0': '',
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
            & > tbody > tr:nth-child(odd) > td:not(.rounded-rows-skip-level),
            & > tbody > tr:nth-child(odd) > td.rounded-rows-skip-level > *
          )`]: {
              backgroundColor: `rgba(0 0 0 / 3%)`,
            },
            [`:where(
            & > tbody > tr.selected > td:not(.rounded-rows-skip-level),
            & > tbody > tr.selected > td.rounded-rows-skip-level > *
          )`]: {
              backgroundColor: 'rgb(255, 255, 255, 40%)',
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
        }
      )

      /** One revolution, in radians. */
      const revolution = Math.PI * 2
      matchUtilities(
        {
          // Values must be pre-computed, because FF does not support `calc()` in `stroke-dasharray`.
          // calc(12 * 0.05 * 6.2832) calc(12 * 6.2832)
          dasharray: value => {
            const [percentage = 0, radius = 0] = value.split('-').map(part => Number(part) || 0)
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
        }
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
          '.settings-subheading': {
            '@apply font-bold text-xl h-[2.375rem] py-0.5': '',
          },
          '.settings-value': {
            '@apply leading-cozy h-text py-px px-2': '',
          },
        },
        {
          respectPrefix: true,
          respectImportant: true,
        }
      )
    }),
  ],
})
