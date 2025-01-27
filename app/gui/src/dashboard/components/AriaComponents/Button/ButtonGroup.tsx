/** @file A group of buttons. */
import { forwardRef, type PropsWithChildren } from 'react'
import flattenChildren from 'react-keyed-flatten-children'

import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { IS_DEV_MODE } from 'enso-common/src/detect'
import invariant from 'tiny-invariant'
import type { TestIdProps } from '../types'
import {
  ButtonGroupProvider,
  JoinedButtonPrivateContextProvider,
  ResetButtonGroupContext,
} from './shared'
import type { ButtonGroupSharedButtonProps, PrivateJoinedButtonPosition } from './types'

const STYLES = tv({
  base: 'flex flex-1 shrink-0 max-h-max',
  variants: {
    wrap: { true: 'flex-wrap' },
    direction: { column: 'flex-col', row: 'flex-row' },
    width: { auto: 'w-auto', full: 'w-full', min: 'w-min', max: 'w-max' },
    gap: {
      custom: '',
      none: 'gap-0',
      joined: 'gap-0',
      large: 'gap-3.5',
      medium: 'gap-2',
      small: 'gap-1.5',
      xsmall: 'gap-1',
      xxsmall: 'gap-0.5',
    },
    align: {
      start: 'justify-start',
      center: 'justify-center',
      end: 'justify-end',
      between: 'justify-between',
      around: 'justify-around',
      evenly: 'justify-evenly',
    },
    verticalAlign: {
      start: 'items-start',
      center: 'items-center',
      end: 'items-end',
    },
  },
  defaultVariants: {
    direction: 'row',
    gap: 'medium',
    wrap: false,
    width: 'full',
  },
  compoundVariants: [
    { direction: 'column', align: 'start', class: 'items-start' },
    { direction: 'column', align: 'center', class: 'items-center' },
    { direction: 'column', align: 'end', class: 'items-end' },
    { direction: 'column', verticalAlign: 'start', class: 'justify-start' },
    { direction: 'column', verticalAlign: 'center', class: 'justify-center' },
    { direction: 'column', verticalAlign: 'end', class: 'justify-end' },
  ],
})

// ===================
// === ButtonGroup ===
// ===================

/** Props for a {@link ButtonGroup}. */
interface ButtonGroupProps
  extends React.PropsWithChildren,
    VariantProps<typeof STYLES>,
    TestIdProps {
  readonly className?: string | undefined
  readonly buttonVariants?: ButtonGroupSharedButtonProps
}

/** A group of buttons. */
// eslint-disable-next-line no-restricted-syntax
export const ButtonGroup = forwardRef(function ButtonGroup(
  props: ButtonGroupProps,
  ref: React.ForwardedRef<HTMLDivElement>,
) {
  const {
    children,
    className,
    gap,
    wrap,
    direction,
    width,
    align,
    variants = STYLES,
    verticalAlign,
    buttonVariants = {},
    testId = 'button-group',
    ...passthrough
  } = props

  const isJoin = gap === 'joined'

  if (IS_DEV_MODE) {
    const isColumnAndJoined = direction === 'column' && isJoin
    invariant(
      !isColumnAndJoined,
      'ButtonGroup: Joined mode is only supported for row direction, please implement column joined mode',
    )
  }

  return (
    <div
      ref={ref}
      className={variants({ gap, wrap, direction, align, verticalAlign, width, className })}
      data-testid={testId}
      {...passthrough}
    >
      <ResetButtonGroupContext>
        <ButtonGroupProvider {...buttonVariants}>
          {isJoin ?
            <JoinedButtons>{children}</JoinedButtons>
          : children}
        </ButtonGroupProvider>
      </ResetButtonGroupContext>
    </div>
  )
})

/**
 * A wrapper for a button group that joins the buttons together.
 * Adds custom styles to the buttons.
 */
function JoinedButtons(props: PropsWithChildren) {
  const { children } = props

  return flattenChildren(children).map((child, index, array) => {
    if (array.length === 1) {
      return <>{child}</>
    }

    let position: PrivateJoinedButtonPosition = 'middle'

    if (index === 0) {
      position = 'first'
    }

    if (index === array.length - 1) {
      position = 'last'
    }

    return (
      <JoinedButtonPrivateContextProvider isJoined position={position}>
        {child}
      </JoinedButtonPrivateContextProvider>
    )
  })
}

/**
 * A button group that joins the buttons together.
 */
export function ButtonGroupJoin(props: ButtonGroupProps) {
  return <ButtonGroup {...props} direction="row" gap="joined" />
}
