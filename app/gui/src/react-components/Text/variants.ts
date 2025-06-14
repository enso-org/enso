/** @file Style for text component */
import { tv } from '#/utilities/tailwindVariants'

export const TEXT_STYLE = tv({
  base: '',
  variants: {
    color: {
      custom: '',
      primary: 'text-primary',
      danger: 'text-danger',
      success: 'text-accent-dark',
      accent: 'text-accent-dark',
      muted: 'text-primary/40',
      disabled: 'text-disabled',
      invert: 'text-invert',
      inherit: 'text-inherit',
      current: 'text-current',
    },
    font: {
      default: '',
      naming: 'font-naming',
    },
    // We use custom padding for the text variants to make sure the text is aligned with the grid.
    // `leading` is also adjusted to make sure the text is aligned with the grid.
    // `leading` should always be after the text size to make sure it is not stripped by `twMerge`.
    variant: {
      custom: '',
      body: 'text-xs leading-[20px] before:h-[2px] after:h-[2px] macos:before:h-[1px] macos:after:h-[3px]',
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'body-sm':
        'text-[10.5px] leading-[16px] before:h-[2px] after:h-[2px] macos:before:h-[1px] macos:after:h-[3px]',
      h1: 'text-xl leading-[29px] before:h-0.5 after:h-[5px] macos:before:h-[3px] macos:after:h-[3px]',
      subtitle:
        'text-[13.5px] leading-[20px] before:h-[2px] after:h-[2px] macos:before:h-[1px] macos:after:h-[3px]',
      caption:
        'text-[8.5px] leading-[12px] before:h-[1px] after:h-[1px] macos:before:h-[0.5px] macos:after:h-[1.5px]',
      overline:
        'text-[8.5px] leading-[16px] before:h-[1px] after:h-[1px] macos:before:h-[0.5px] macos:after:h-[1.5px] uppercase',
    },
    weight: {
      custom: '',
      default: '',
      bold: 'font-bold',
      semibold: 'font-semibold',
      extraBold: 'font-extrabold',
      medium: 'font-medium',
      normal: 'font-normal',
      thin: 'font-thin',
    },
    balance: {
      true: 'text-balance',
    },
    transform: {
      none: '',
      normal: 'normal-case',
      capitalize: 'capitalize',
      lowercase: 'lowercase',
      uppercase: 'uppercase',
    },
    align: {
      left: 'text-left',
      center: 'text-center',
      right: 'text-right',
    },
    truncate: {
      true: 'block truncate',
      /* eslint-disable @typescript-eslint/naming-convention */
      '1': 'block truncate',
      '2': 'line-clamp-2',
      '3': 'line-clamp-3',
      '4': 'line-clamp-4',
      '5': 'line-clamp-5',
      '6': 'line-clamp-6',
      '7': 'line-clamp-7',
      '8': 'line-clamp-8',
      '9': 'line-clamp-9',
      custom: 'line-clamp-[var(--line-clamp)]',
      /* eslint-enable @typescript-eslint/naming-convention */
    },
    monospace: { true: 'font-mono' },
    italic: { true: 'italic' },
    nowrap: { true: 'whitespace-nowrap', normal: 'whitespace-normal', false: '' },
    textSelection: {
      auto: '',
      none: 'select-none',
      word: 'select-text',
      all: 'select-all',
    },
    disableLineHeightCompensation: {
      true: 'before:hidden after:hidden before:w-0 after:w-0',
      false:
        'flex-col before:block after:block before:flex-none after:flex-none before:w-full after:w-full',
      top: 'flex-col before:hidden before:w-0 after:block after:flex-none after:w-full',
      bottom: 'flex-col before:block before:flex-none before:w-full after:hidden after:w-0',
    },
  },
  defaultVariants: {
    variant: 'body',
    font: 'default',
    weight: 'default',
    transform: 'none',
    color: 'primary',
    italic: false,
    nowrap: false,
    monospace: false,
    disableLineHeightCompensation: false,
    textSelection: 'auto',
  },
  compoundVariants: [
    { variant: 'body', weight: 'default', className: 'font-medium' },
    { variant: 'body-sm', weight: 'default', className: 'font-medium' },
    { variant: 'h1', weight: 'default', className: 'font-bold' },
    { variant: 'subtitle', weight: 'default', className: 'font-bold' },
    { variant: 'caption', weight: 'default', className: 'font-medium' },
    { variant: 'overline', weight: 'default', className: 'font-medium' },
  ],
})
