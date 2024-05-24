/**
 * @file
 *
 * Variants for the Dialog component.
 */
import * as twv from 'tailwind-variants'

export const DIALOG_STYLES = twv.tv({
  base: 'relative flex flex-col overflow-hidden rounded-default text-left align-middle shadow-sm bg-clip-padding border border-primary/10 before:absolute before:inset before:h-full before:w-full before:rounded-xl before:bg-selected-frame before:backdrop-blur-default',
})
