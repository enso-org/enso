/**
 * @file
 *
 * Field component
 */

import * as React from 'react'

import * as aria from '#/components/aria'

import * as twv from '#/utilities/tailwindVariants'

import * as text from '../../Text'
import type * as types from './types'
import * as formContext from './useFormContext'

/**
 * Props for Field component
 */
export interface FieldComponentProps
  extends twv.VariantProps<typeof FIELD_STYLES>,
    types.FieldProps {
  readonly name: string
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: types.FormInstance<any, any, any>
  readonly isInvalid?: boolean | undefined
  readonly className?: string | undefined
  readonly children?: React.ReactNode | ((props: FieldChildrenRenderProps) => React.ReactNode)
  readonly style?: React.CSSProperties | undefined
}

/**
 * Props for Field children
 */
export interface FieldChildrenRenderProps {
  readonly isInvalid: boolean
  readonly isDirty: boolean
  readonly isTouched: boolean
  readonly isValidating: boolean
  readonly error?: string | undefined
}

export const FIELD_STYLES = twv.tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: {
    fullWidth: { true: 'w-full' },
    isInvalid: { true: { label: 'text-danger' } },
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

/**
 * Field component
 */
export const Field = React.forwardRef(function Field(
  props: FieldComponentProps,
  ref: React.ForwardedRef<HTMLFieldSetElement>
) {
  const {
    form = formContext.useFormContext(),
    isInvalid,
    children,
    className,
    label,
    description,
    fullWidth,
    error,
    name,
    isRequired = false,
  } = props

  const fieldState = form.getFieldState(name)

  const labelId = React.useId()
  const descriptionId = React.useId()
  const errorId = React.useId()

  const invalid = isInvalid === true || fieldState.invalid

  const classes = FIELD_STYLES({
    fullWidth,
    isInvalid: invalid,
  })

  const hasError = (error ?? fieldState.error?.message) != null

  return (
    <fieldset
      ref={ref}
      className={classes.base({ className })}
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
              /* eslint-disable-next-line no-restricted-syntax */
              <span aria-hidden="true" className="scale-80 text-danger">
                {' *'}
              </span>
            )}
          </span>
        )}

        <div className={classes.content()}>
          {typeof children === 'function'
            ? children({
                isInvalid: invalid,
                isDirty: fieldState.isDirty,
                isTouched: fieldState.isTouched,
                isValidating: fieldState.isValidating,
                error: fieldState.error?.message,
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
        <span id={errorId} className={classes.error()}>
          {error ?? fieldState.error?.message}
        </span>
      )}
    </fieldset>
  )
})
