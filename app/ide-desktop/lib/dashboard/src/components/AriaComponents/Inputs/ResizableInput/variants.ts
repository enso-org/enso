/**
 * @file
 *
 * Variants for the ResizableInput component.
 */

import * as twv from 'tailwind-variants'

export const INPUT_STYLES = twv.tv({
  base: 'w-full overflow-hidden block cursor-text rounded-md border-2 border-primary/10 bg-transparent px-1.5 pb-1 pt-2 focus-within:border-primary/50 transition-colors duration-200',
  variants: { isInvalid: { true: 'border-red-500/70 focus-within:border-red-500' } },
  slots: {
    inputContainer: 'block max-h-32 min-h-5 text-sm font-normal relative overflow-auto',
    description: 'mt-1 block text-xs text-primary/40 select-none pointer-events-none',
    error: 'block text-xs text-red-500',
    textArea: 'block h-auto w-full max-h-full resize-none bg-transparent',
    resizableSpan:
      'pointer-events-none invisible absolute block max-h-32 min-h-10 overflow-y-auto break-all text-sm',
  },
})
