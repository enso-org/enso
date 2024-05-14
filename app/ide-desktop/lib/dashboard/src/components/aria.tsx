/** @file Barrel re-export of `react-aria` and `react-aria-components`. */
import * as React from 'react'

import * as aria from 'react-aria'
import * as ariaComponents from 'react-aria-components'

import * as string from '#/utilities/string'

export type * from '@react-types/shared'
export * from 'react-aria'
// @ts-expect-error The conflicting exports are props types ONLY.
export * from 'react-aria-components'

// ==================
// === mergeProps ===
// ==================

/** Merges multiple props objects together.
 * Event handlers are chained, classNames are combined, and ids are deduplicated -
 * different ids will trigger a side-effect and re-render components hooked up with `useId`.
 * For all other props, the last prop object overrides all previous ones.
 *
 * The constraint is defaulted to `never` to make an explicit constraint mandatory. */
export function mergeProps<Constraint extends object = never>() {
  // eslint-disable-next-line no-restricted-syntax
  return <T extends (Partial<Constraint> | null | undefined)[]>(...args: T) =>
    aria.mergeProps(...args)
}

// ===============================
// === childrenToDisplayGlyphs ===
// ===============================

/** Replace glyphs in strings in a React element with the non-code equivalents. */
function childrenToDisplayGlyphs(child: React.ReactNode): React.ReactNode {
  if (typeof child === 'string') {
    return string.displayGlyphs(child)
  } else if (typeof child === 'object' && child != null && Symbol.iterator in child) {
    return Array.from(child, childrenToDisplayGlyphs)
  } else {
    return child
  }
}

// ============
// === Text ===
// ============

/** An accessible `span`. */
function InternalText(props: ariaComponents.TextProps, ref: React.ForwardedRef<HTMLElement>) {
  const { children, ...textProps } = props
  return (
    <ariaComponents.Text ref={ref} {...textProps}>
      {childrenToDisplayGlyphs(children)}
    </ariaComponents.Text>
  )
}

export const Text = React.forwardRef(InternalText)

// ===============
// === Heading ===
// ===============

/** An accessible `h1` through `h6`. */
function InternalHeading(
  props: ariaComponents.HeadingProps,
  ref: React.ForwardedRef<HTMLHeadingElement>
) {
  const { children, ...HeadingProps } = props
  return (
    <ariaComponents.Heading ref={ref} {...HeadingProps}>
      {childrenToDisplayGlyphs(children)}
    </ariaComponents.Heading>
  )
}

export const Heading = React.forwardRef(InternalHeading)

// ==============
// === Header ===
// ==============

/** An accessible `h1` through `h6`. */
function InternalHeader(
  props: React.HTMLAttributes<HTMLElement>,
  ref: React.ForwardedRef<HTMLElement>
) {
  const { children, ...HeaderProps } = props
  return (
    <ariaComponents.Header ref={ref} {...HeaderProps}>
      {childrenToDisplayGlyphs(children)}
    </ariaComponents.Header>
  )
}

export const Header = React.forwardRef(InternalHeader)

// =============
// === Label ===
// =============

/** An accessible `label`. */
function InternalLabel(
  props: ariaComponents.LabelProps,
  ref: React.ForwardedRef<HTMLLabelElement>
) {
  const { children, ...LabelProps } = props
  return (
    <ariaComponents.Label ref={ref} {...LabelProps}>
      {childrenToDisplayGlyphs(children)}
    </ariaComponents.Label>
  )
}

export const Label = React.forwardRef(InternalLabel)

// =============
// === Input ===
// =============

/** An accessible `input`. */
function InternalInput(
  props: ariaComponents.InputProps,
  ref: React.ForwardedRef<HTMLInputElement>
) {
  return (
    <ariaComponents.Input
      ref={ref}
      {...props}
      {...(props.placeholder == null
        ? {}
        : { placeholder: string.displayGlyphs(props.placeholder) })}
    />
  )
}

export const Input = React.forwardRef(InternalInput)
