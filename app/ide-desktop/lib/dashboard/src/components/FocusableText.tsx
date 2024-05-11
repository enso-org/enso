/** @file An {@link aria.Text} that is focusable to allow it to be a {@link aria.TooltipTrigger}
 * target. */
import * as React from 'react'

import * as aria from '#/components/aria'

// =====================
// === FocusableText ===
// =====================

/** Props for a {@link FocusableText}. */
export interface FocusableTextProps extends Readonly<aria.TextProps> {}

/** An {@link aria.Text} that is focusable to allow it to be a {@link aria.TooltipTrigger}
 * target. */
function FocusableText(props: FocusableTextProps, ref: React.ForwardedRef<HTMLElement>) {
  // @ts-expect-error This error is caused by `exactOptionalPropertyTypes`.
  const [props2, ref2] = aria.useContextProps(props, ref, aria.TextContext)
  // @ts-expect-error This error is caused by `exactOptionalPropertyTypes`.
  const { focusableProps } = aria.useFocusable(props2, ref2)
  const { elementType: ElementType = 'span', ...domProps } = props2
  return (
    <ElementType
      className="react-aria-Text"
      {...aria.mergeProps<FocusableTextProps>()(domProps, focusableProps)}
      // @ts-expect-error This is required because the dynamic element type is too complex for
      // TypeScript to typecheck.
      ref={ref2}
    />
  )
}

export default React.forwardRef(FocusableText)
