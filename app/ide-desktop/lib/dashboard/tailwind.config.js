/** @file Configuration for Tailwind. */

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
      },
      spacing: {
        DEFAULT: '0',
        // TODO: Remove unused spacings.
        0.75: '0.1875rem',
        1.25: '0.3125rem',
        1.75: '0.4375rem',
        2.25: '0.5625rem',
        3.25: '0.8125rem',
        3.75: '0.9375rem',
        3.5: '0.875rem',
        4.5: '1.125rem',
        4.75: '1.1875rem',
        5.5: '1.375rem',
        6.5: '1.625rem',
        7.5: '1.875rem',
        9.5: '2.375rem',
        13: '3.25rem',
        25: '6.25rem',
        '10lh': '10lh',
        icon: 'var(--icon-size)',
        'project-icon': 'var(--project-icon-size)',
        'profile-picture': 'var(--profile-picture-size)',
        'profile-picture-large': 'var(--profile-picture-large-size)',
        'radio-button': 'var(--radio-button-size)',
        'radio-button-dot': 'var(--radio-button-dot-size)',
        'extended-editor-menu': 'var(--extended-editor-menu-size)',
        'plus-icon': 'var(--plus-icon-size)',
        'chat-profile-picture': 'var(--chat-profile-picture-size)',
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
        'data-link-text-input': 'var(--data-link-text-input-width)',
        'data-link-object-input': 'var(--data-link-object-input-width)',
        'data-link-dropdown-title': 'var(--data-link-dropdown-title-width)',
        'asset-search-bar': 'var(--asset-search-bar-width)',
        'permission-display': 'var(--permission-display-width)',
        chat: 'var(--chat-width)',
        'chat-indicator': 'var(--chat-indicator-width)',
        'user-menu': 'var(--user-menu-width)',
        'modal-label': 'var(--modal-label-width)',
        'settings-sidebar': 'var(--settings-sidebar-width)',
        'asset-panel': 'var(--asset-panel-width)',
        'permission-type-selector': 'var(--permission-type-selector-width)',
        'drive-sidebar': 'var(--drive-sidebar-width)',

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
        'drive-name-column': 'var(--drive-name-column-width)',
        'drive-modified-column': 'var(--drive-modified-column-width)',
        'drive-shared-with-column': 'var(--drive-shared-with-column-width)',
        'drive-labels-column': 'var(--drive-labels-column-width)',
        'drive-accessed-by-projects-column': 'var(--drive-accessed-by-projects-column-width)',
        'drive-accessed-data-column': 'var(--drive-accessed-data-column-width)',
        'drive-docs-column': 'var(--drive-docs-column-width)',
      },
      minWidth: ({ theme }) => ({
        .../** @type {{}}*/ (theme('width')),
        31.5: '7.875rem',
        32: '8rem',
        61.25: '15.3125rem',
      }),
      maxWidth: ({ theme }) => ({
        .../** @type {{}}*/ (theme('width')),
        98.25: '24.5625rem',
        'screen-1/3': '33.333333vw',
      }),
      height: {
        row: 'var(--row-height)',
        text: 'var(--text-height)',
        heading: 'var(--heading-height)',
        'news-item': 'var(--news-item-height)',
        sample: 'var(--sample-height)',
        'sample-info': 'var(--sample-info-height)',
        'side-panel-heading': 'var(--side-panel-heading-height)',
        'chat-thread-list': 'var(--chat-thread-list-height)',
        'payment-form': 'var(--payment-form-height)',
        'drive-table-heading': 'var(--drive-table-heading-height)',
      },
      minHeight: ({ theme }) => ({
        .../** @type {{}}*/ (theme('height')),
        '5lh': '5lh',
      }),
      maxHeight: ({ theme }) => ({
        .../** @type {{}}*/ (theme('height')),
      }),
      opacity: {
        full: '100%',
        '1/3': '.33333333',
        disabled: 'var(--disabled-opacity)',
        'disabled-icon': 'var(--disabled-icon-opacity)',
        'disabled-category': 'var(--disabled-category-opacity)',
        placeholder: 'var(--placeholder-opacity)',
        'read-only': 'var(--read-only-opacity)',
        'new-empty-project': 'var(--new-empty-project-opacity)',
      },
      gap: {
        modal: 'var(--modal-gap)',
        subheading: 'var(--subheading-gap)',
        buttons: 'var(--buttons-gap)',
        icons: 'var(--icons-gap)',
        'samples-icon-with-text': 'var(--samples-icon-with-text-gap)',
        'icon-with-text': 'var(--icon-with-text-gap)',
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
        banner: 'var(--banner-gap)',
        'new-empty-project': 'var(--new-empty-project-gap)',
        modifiers: 'var(--modifiers-gap)',
        'modifiers-macos': 'var(--modifiers-macos-gap)',
        'side-panel': 'var(--side-panel-gap)',
        'side-panel-section': 'var(--side-panel-section-gap)',
        'data-link': 'var(--data-link-gap)',
        'asset-search-bar': 'var(--asset-search-bar-gap)',
        'drive-bar': 'var(--drive-bar-gap)',
        'column-items': 'var(--column-items-gap)',
        labels: 'var(--labels-gap)',
        'label-icons': 'var(--label-icons-gap)',
        'drive-sidebar-section-heading': 'var(--drive-sidebar-section-heading-gap)',
        'user-menu': 'var(--user-menu-gap)',
        'user-permission': 'var(--user-permission-gap)',
        'name-column-icon': 'var(--name-column-icon-gap)',
      },
      padding: {
        modal: 'var(--modal-padding)',
        auth: 'var(--auth-padding)',
        'page-x': 'var(--page-padding-x)',
        'heading-x': 'var(--heading-padding-x)',
        'heading-y': 'var(--heading-padding-y)',
        'auth-input-y': 'var(--auth-input-padding-y)',
        'drive-sidebar-y': 'var(--drive-sidebar-padding-y)',
        'radio-button-dot': 'var(--radio-button-dot-padding)',
        'cell-x': 'var(--cell-padding-x)',
        'button-x': 'var(--button-padding-x)',
        'icons-x': 'var(--icons-padding-x)',
        'drive-bar-y': 'var(--drive-bar-padding-y)',
        'drive-bar-icons-x': 'var(--drive-bar-icons-padding-x)',
        'selector-x': 'var(--selector-padding-x)',
        'selector-y': 'var(--selector-padding-y)',
        'menu-entry': 'var(--menu-entry-padding)',
        'context-menu-entry-x': 'var(--context-menu-entry-padding-x)',
        'profile-picture': 'var(--profile-picture-padding)',
        'banner-x': 'var(--banner-padding-x)',
        'banner-y': 'var(--banner-padding-y)',
        'banner-item': 'var(--banner-item-padding)',
        'profile-picture-caption-y': 'var(--profile-picture-caption-padding-y)',
        'delete-user-account-button-x': 'var(--delete-user-account-button-padding-x)',
        'context-menu': 'var(--context-menu-padding)',
        'input-x': 'var(--input-padding-x)',
        'data-link-object-input': 'var(--data-link-object-input-padding)',
        'new-project-button-x': 'var(--new-project-button-padding-x)',
        'name-column-x': 'var(--name-column-padding-x)',
        'name-column-y': 'var(--name-column-padding-y)',
        'home-page-b': 'var(--home-page-padding-b)',
        version: 'var(--version-padding)',
        'label-x': 'var(--label-padding-x)',
        'drive-sidebar-section-heading': 'var(--drive-sidebar-section-heading-padding-x)',
        'user-menu-x': 'var(--user-menu-padding-x)',
        'user-menu-y': 'var(--user-menu-padding-y)',
        'extra-columns-panel-x': 'var(--extra-columns-panel-padding-x)',
        'extra-columns-panel-y': 'var(--extra-columns-panel-padding-y)',
      },
      margin: {
        'top-bar': 'var(--top-bar-margin)',
        'name-column-icon': 'var(--name-column-icon-margin)',
      },
      lineHeight: {
        snug: 'var(--snug-line-height)',
        cozy: 'var(--cozy-line-height)',
      },
      zIndex: {
        1: '1',
        3: '3',
      },
      backdropBlur: {
        xs: '2px',
        default: 'var(--default-backdrop-blur)',
      },
      borderWidth: { 0.5: '0.5px', 6: '6px' },
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
        400: '400ms',
        5000: '5000ms',
        90000: '90000ms',
        auth: 'var(--auth-transition-duration)',
        'side-panel': 'var(--side-panel-transition-duration)',
        arrow: 'var(--arrow-transition-duration)',
        'spinner-fast': 'var(--spinner-fast-transition-duration)',
        'spinner-medium': 'var(--spinner-medium-transition-duration)',
        'spinner-slow': 'var(--spinner-slow-transition-duration)',
      },
      gridTemplateRows: {
        '0fr': '0fr',
        '1fr': '1fr',
      },
      gridTemplateColumns: {
        'fill-news-items': 'repeat(auto-fill, minmax(var(--news-items-column-width), 1fr))',
        'fill-samples': 'repeat(auto-fill, minmax(var(--samples-column-width), 1fr))',
      },
    },
  },
})
