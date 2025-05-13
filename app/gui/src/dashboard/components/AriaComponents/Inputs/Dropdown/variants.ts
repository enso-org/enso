/** @file Variants for `Dropdown`. */
import { makeRoundedStyles } from '#/components/AriaComponents/utilities'
import { tv } from '#/utilities/tailwindVariants'

export const DROPDOWN_STYLES = tv({
  base: 'group relative flex w-max cursor-pointer flex-col items-start whitespace-nowrap rounded-input leading-cozy',
  variants: {
    isFocused: {
      true: {
        container: 'z-1',
        options: 'before:shadow-soft before:bg-frame before:backdrop-blur-md',
        optionsContainer: 'grid-rows-1fr',
        input: 'z-1',
      },
      false: {
        container: 'overflow-hidden',
        options: 'before:h-full',
        optionsContainer: 'grid-rows-0fr',
      },
    },
    isReadOnly: {
      true: {
        input: 'read-only',
      },
    },
    multiple: {
      true: {
        optionsItem: 'hover:font-semibold',
      },
    },
    rounded: makeRoundedStyles('options', (classes) => `before:${classes}`),
    size: {
      medium: {
        input: 'px-4 pb-[6.5px] pt-[8.5px]',
        optionsItem: 'px-4',
        hiddenOption: 'px-4',
        icon: 'size-4',
      },
      small: {
        input: 'px-4 pb-0.5 pt-1',
        optionsItem: 'px-4',
        hiddenOption: 'px-4',
        icon: 'size-3',
      },
      custom: {},
    },
  },
  slots: {
    container: 'absolute left-0 min-h-full w-full min-w-max pb-px',
    icon: '',
    options:
      'relative before:absolute before:top-0 before:h-full before:w-full before:rounded-input before:border-0.5 before:border-primary/20 before:transition-colors',
    optionsSpacing: 'padding relative h-full',
    optionsContainer:
      'relative grid max-h-60 w-full overflow-auto rounded-input transition-grid-template-rows',
    optionsList: 'overflow-auto',
    optionsItem:
      'flex min-h-6 items-center gap-2 rounded-input transition-colors focus:cursor-default focus:bg-frame focus:font-bold focus:focus-ring not-focus:hover:bg-hover-bg not-selected:hover:bg-hover-bg',
    input: 'group relative flex items-center gap-2',
    dropdownArrow: 'rotate-90 opacity-80 group-hover:opacity-100',
    inputDisplay: 'grow select-none',
    hiddenOptions: 'flex h-0 flex-col overflow-hidden',
    hiddenOption: 'flex gap-2 font-bold',
  },
  defaultVariants: {
    rounded: 'xlarge',
    size: 'small',
  },
})
