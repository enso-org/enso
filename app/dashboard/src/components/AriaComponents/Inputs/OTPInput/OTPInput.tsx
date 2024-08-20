/**
 * @file
 */
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { OTPInput, type SlotProps } from 'input-otp'
import { TEXT_STYLE, Text } from '../../Text'

/**
 * Props for an {@link OtpInput}.
 */
export interface OtpInputProps extends VariantProps<typeof STYLES> {
  readonly length: number
}

const STYLES = tv({
  base: 'group flex items-center',
  slots: {},
})

const SLOT_STYLES = tv({
  base: 'flex items-center justify-center aspect-square border-border border-y border-r first:border-l first:rounded-l-md last:rounded-r-md',
  variants: {
    isActive: { true: 'bg-primary text-white' },
  },
  slots: {
    char: TEXT_STYLE({
      variant: 'body',
      weight: 'bold',
      color: 'current',
    }),
    fakeCaret:
      'absolute pointer-events-none inset-0 flex items-center justify-center animate-caret-blink before:w-px before:h-8 before:bg-white',
  },
})

/**
 *
 */
export function OtpInput(props: OtpInputProps) {
  const { length } = props

  return (
    <div>
      <OTPInput
        maxLength={length}
        render={({ slots, isFocused, isHovering }) => (
          <div className="flex">
            {slots.map((slot, idx) => (
              <Slot key={idx} {...slot} />
            ))}
          </div>
        )}
      />
    </div>
  )
}

/**
 *
 */
function Slot(props: SlotProps) {
  const { char, isActive, hasFakeCaret } = props
  const classes = SLOT_STYLES({ isActive })

  return (
    <div className={classes.base()}>
      {char != null && <div className={classes.char()}>{char}</div>}
      {hasFakeCaret && <div className={classes.fakeCaret()} />}
    </div>
  )
}
