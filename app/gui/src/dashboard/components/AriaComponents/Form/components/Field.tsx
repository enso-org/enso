/**
 * @file
 *
 * Field component
 */
import * as React from 'react'

import * as aria from '#/components/aria'

import { forwardRef } from '#/utilities/react'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import type { Path } from 'react-hook-form'
import * as text from '../../Text'
import { Form } from '../Form'
import type * as types from './types'

/** Props for Field component */
export interface FieldComponentProps<Schema extends types.TSchema>
  extends VariantProps<typeof FIELD_STYLES>,
    types.FieldProps {
  readonly 'data-testid'?: string | undefined
  readonly name: Path<types.FieldValues<Schema>>
  readonly form?: types.FormInstance<Schema> | undefined
  readonly isInvalid?: boolean | undefined
  readonly className?: string | undefined
  readonly children?: React.ReactNode | ((props: FieldChildrenRenderProps) => React.ReactNode)
  readonly style?: React.CSSProperties | undefined
}

/** Props for Field variants */
export interface FieldVariantProps {
  readonly fieldVariants?: VariantProps<typeof FIELD_STYLES>['variants'] | undefined
}

/** Props for Field children */
export interface FieldChildrenRenderProps {
  readonly isInvalid: boolean
  readonly isDirty: boolean
  readonly isTouched: boolean
  readonly isValidating: boolean
  readonly hasError: boolean
  readonly error?: string | undefined
}

export const FIELD_STYLES = tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: {
    fullWidth: { true: 'w-full' },
    isInvalid: { true: { label: 'text-danger' } },
    isHidden: { true: { base: 'hidden' } },
  },
  slots: {
    labelContainer: 'contents',
    label: text.TEXT_STYLE({ variant: 'body', disableLineHeightCompensation: true }),
    content: 'flex flex-col items-start w-full',
    description: text.TEXT_STYLE({ variant: 'body', color: 'disabled' }),
    error: text.TEXT_STYLE({ variant: 'body', color: 'danger' }),
  },
  defaultVariants: { fullWidth: true },
})

/** Field component */
// eslint-disable-next-line no-restricted-syntax
export const Field = forwardRef(function Field<Schema extends types.TSchema>(
  props: FieldComponentProps<Schema>,
  ref: React.ForwardedRef<HTMLFieldSetElement>,
) {
  const {
    children,
    className,
    label,
    description,
    fullWidth,
    error,
    isHidden,
    isInvalid = false,
    isRequired = false,
    variants = FIELD_STYLES,
  } = props

  const labelId = React.useId()
  const descriptionId = React.useId()
  const errorId = React.useId()

  const fieldState = Form.useFieldState(props)

  const invalid = isInvalid || fieldState.hasError

  const classes = variants({ fullWidth, isInvalid: invalid, isHidden })

  const hasError = (error ?? fieldState.error) != null

  return (
    <fieldset
      ref={ref}
      className={classes.base({ className })}
      data-testid={props['data-testid']}
      aria-invalid={invalid}
      aria-label={props['aria-label']}
      aria-labelledby={labelId}
      aria-describedby={descriptionId}
      aria-details={props['aria-details']}
      aria-errormessage={hasError ? errorId : ''}
      aria-required={isRequired}
    >
      <aria.Label id={labelId} className={classes.labelContainer()}>
        {label != null && (
          <span id={labelId} className={classes.label()}>
            {label}

            {isRequired && (
              <span aria-hidden="true" className="scale-80 text-danger">
                {' *'}
              </span>
            )}
          </span>
        )}

        <div className={classes.content()}>
          {typeof children === 'function' ?
            children({
              isInvalid: invalid,
              isDirty: fieldState.isDirty,
              isTouched: fieldState.isTouched,
              isValidating: fieldState.isValidating,
              hasError: fieldState.hasError,
              error: fieldState.error,
            })
          : children}
        </div>
      </aria.Label>

      {description != null && (
        <span id={descriptionId} className={classes.description()}>
          {description}
        </span>
      )}

      {hasError && (
        <span data-testid="error" id={errorId} className={classes.error()}>
          {error ?? fieldState.error}
        </span>
      )}
    </fieldset>
  )
})
