/** @file Field component */
import * as aria from '#/components/aria'
import type { Path } from '#/utilities/objectPath'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import * as React from 'react'
import { ContextualHelp } from '../../ContextualHelp'
import * as text from '../../Text'
import { Form } from '../Form'
import type * as types from './types'

/** Props for Field component */
export interface FieldComponentProps<Schema extends types.TSchema>
  extends VariantProps<typeof FIELD_STYLES>,
    types.FieldProps {
  readonly 'data-testid'?: string | undefined
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly name: Path<types.FieldValues<Schema>, any>
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
  readonly error?: string | null | undefined
}

// eslint-disable-next-line react-refresh/only-export-components
export const FIELD_STYLES = tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: {
    fullWidth: { true: 'w-full' },
    isInvalid: { true: { label: 'text-danger' } },
    isHidden: { true: { base: 'hidden' } },
  },
  slots: {
    fieldContent: 'contents',
    contextualHelp: '',
    labelContainer: 'flex gap-1 items-center',
    label: text.TEXT_STYLE({ variant: 'body' }),
    content: 'flex flex-col items-start w-full',
    description: text.TEXT_STYLE({ variant: 'body', color: 'disabled' }),
  },
  defaultVariants: { fullWidth: true },
})

/** Field component */
export const Field = React.forwardRef(function Field<Schema extends types.TSchema>(
  props: FieldComponentProps<Schema>,
  ref: React.ForwardedRef<HTMLDivElement>,
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
    preventLabelFocus = false,
    contextualHelp,
    variants = FIELD_STYLES,
  } = props

  const labelId = React.useId()
  const descriptionId = React.useId()
  const errorId = React.useId()

  // This is SAFE, we are just using a type with added constraint.
  // eslint-disable-next-line no-restricted-syntax
  const fieldState = Form.useFieldState(props as never)

  const invalid = isInvalid || fieldState.hasError

  const classes = variants({ fullWidth, isInvalid: invalid, isHidden })

  const hasError = (error !== undefined ? error : fieldState.error) != null

  return (
    <div
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
      <aria.Label
        id={labelId}
        className={classes.fieldContent()}
        onClickCapture={(event) => {
          if (preventLabelFocus) {
            event.preventDefault()
          }
        }}
      >
        <div className={classes.labelContainer()}>
          {label != null && (
            <span id={labelId} className={classes.label()}>
              {label}

              {isRequired && (
                <span aria-hidden="true" className="text-primary" data-testid="required-mark">
                  {' *'}
                </span>
              )}
            </span>
          )}

          <div className={classes.contextualHelp()}>
            {contextualHelp != null && (
              <ContextualHelp placement="top" variant="help">
                {contextualHelp}
              </ContextualHelp>
            )}
          </div>
        </div>

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
        <span id={descriptionId} className={classes.description()} data-testid="description">
          {description}
        </span>
      )}

      <FieldError
        error={error}
        id={errorId}
        /* This is SAFE, we are just using a type with added constraint. */
        /* eslint-disable-next-line no-restricted-syntax */
        name={props.name as types.FieldPath<Schema, string>}
        form={props.form}
      />
    </div>
  )
})

// eslint-disable-next-line react-refresh/only-export-components
export const FIELD_ERROR_STYLES = tv({
  base: text.TEXT_STYLE({ variant: 'body', color: 'danger', className: 'block' }),
  variants: { fullWidth: { true: 'w-full' } },
  defaultVariants: { fullWidth: true },
})

/**
 * Props for the {@link FieldError} component.
 */
export interface FieldErrorProps<
  Schema extends types.TSchema,
  TFieldName extends types.FieldPath<Schema, string>,
> extends React.HTMLAttributes<HTMLSpanElement>,
    VariantProps<typeof FIELD_ERROR_STYLES> {
  readonly error?: React.ReactNode | string | null | undefined
  readonly id?: string | undefined
  readonly form?: types.FormInstance<Schema> | undefined
  readonly name: TFieldName
}

/**
 * Component for displaying an error message for a field.
 */
export function FieldError<
  Schema extends types.TSchema,
  TFieldName extends types.FieldPath<Schema, string>,
>(props: FieldErrorProps<Schema, TFieldName>) {
  const { error, className, id, variants = FIELD_ERROR_STYLES, fullWidth, ...rest } = props

  // This is SAFE, we are just using a type with added constraint.
  // eslint-disable-next-line no-restricted-syntax
  const fieldState = Form.useFieldState(props as never)

  const hasError = (error !== undefined ? error : fieldState.error) != null

  if (!hasError) {
    return null
  }

  return (
    <span data-testid="error" id={id} className={variants({ className, fullWidth })} {...rest}>
      {error ?? fieldState.error}
    </span>
  )
}
