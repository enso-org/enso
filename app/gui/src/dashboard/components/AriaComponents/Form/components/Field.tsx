/** @file Field component. */
import * as aria from '#/components/aria'
import type { Path } from '#/utilities/objectPath'
import type { VariantProps } from '#/utilities/tailwindVariants'
import * as React from 'react'
import { ContextualHelp } from '../../ContextualHelp'
import { FIELD_ERROR_STYLES, FIELD_STYLES } from '../variants'
import type { FieldPath, FieldProps, FieldValues, FormInstance, TSchema } from './types'
import { useFieldState } from './useFieldState'

/** Props for Field component */
export interface FieldComponentProps<Schema extends TSchema>
  extends VariantProps<typeof FIELD_STYLES>,
    FieldProps {
  readonly 'data-testid'?: string | undefined
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly name: Path<FieldValues<Schema>, any>
  readonly form?: FormInstance<Schema> | undefined
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

/** Field component */
export const Field = React.forwardRef(function Field<Schema extends TSchema>(
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
    contextualHelp,
    variants = FIELD_STYLES,
  } = props

  const labelId = React.useId()
  const descriptionId = React.useId()
  const errorId = React.useId()

  // This is SAFE, we are just using a type with added constraint.
  // eslint-disable-next-line no-restricted-syntax
  const fieldState = useFieldState(props as never)

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
      <aria.Label id={labelId} className={classes.fieldContent()}>
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
        name={props.name as FieldPath<Schema, string>}
        form={props.form}
      />
    </div>
  )
})

/**
 * Props for the {@link FieldError} component.
 */
export interface FieldErrorProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, string>,
> extends React.HTMLAttributes<HTMLSpanElement>,
    VariantProps<typeof FIELD_ERROR_STYLES> {
  readonly error?: React.ReactNode | string | null | undefined
  readonly id?: string | undefined
  readonly form?: FormInstance<Schema> | undefined
  readonly name: TFieldName
}

/**
 * Component for displaying an error message for a field.
 */
export function FieldError<Schema extends TSchema, TFieldName extends FieldPath<Schema, string>>(
  props: FieldErrorProps<Schema, TFieldName>,
) {
  const { error, className, id, variants = FIELD_ERROR_STYLES, fullWidth, ...rest } = props

  // This is SAFE, we are just using a type with added constraint.
  // eslint-disable-next-line no-restricted-syntax
  const fieldState = useFieldState(props as never)

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
