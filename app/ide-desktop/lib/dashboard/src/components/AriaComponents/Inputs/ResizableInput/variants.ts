/**
 * @file
 *
 * Variants for the ResizableInput component.
 */

import * as twv from '#/utilities/tailwindVariants'

import * as text from '../../Text'

export const INPUT_STYLES = twv.tv({
  base: 'w-full overflow-hidden block cursor-text rounded-md border-2 border-primary/10 bg-transparent px-1.5 pb-1 pt-1.5 focus-within:border-primary/50 transition-colors duration-200',
  variants: { isInvalid: { true: 'border-red-500/70 focus-within:border-red-500' } },
  slots: {
    inputContainer: text.TEXT_STYLE({
      className: 'block max-h-32 min-h-6 text-sm font-medium relative overflow-auto',
      variant: 'body',
    }),
    description: 'block select-none pointer-events-none opacity-80',
    error: 'block',
    textArea: 'block h-auto w-full max-h-full resize-none bg-transparent',
    resizableSpan: text.TEXT_STYLE({
      className:
        'pointer-events-none invisible absolute block max-h-32 min-h-10 overflow-y-auto break-all',
      variant: 'body',
    }),
  },
})
