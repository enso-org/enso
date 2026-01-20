/** @file A date picker. */
import CrossIcon from '#/assets/cross.svg'
import {
  TimeField as AriaTimeField,
  DateInput,
  DateSegment,
  Group,
  Label,
  TimeFieldStateContext,
  type TimeFieldProps as AriaTimeFieldProps,
  type TimeValue,
} from '#/components/aria'
import { Button } from '#/components/Button'
import {
  Form,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type TSchema,
} from '#/components/Form'
import { Text } from '#/components/Text'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { useText } from '$/providers/react'
import { forwardRef, useContext, type ForwardedRef } from 'react'
import type { DateSegment as DateSegmentType } from 'react-stately'

const DATE_PICKER_STYLES = tv({
  base: '',
  variants: {
    size: {
      small: {
        inputGroup: 'h-6 px-2',
      },
      medium: {
        inputGroup: 'h-8 px-4',
      },
    },
  },
  slots: {
    inputGroup: 'flex items-center gap-2 rounded-full border-0.5 border-primary/20',
    dateInput: 'flex justify-center grow',
    dateSegment: 'rounded placeholder-shown:text-primary/30 focus:bg-primary/10 px-[0.5px]',
    resetButton: '',
    calendarPopover: '',
    calendarDialog: 'text-primary text-xs mx-2',
    calendarContainer: '',
    calendarHeader: 'flex items-center mb-2',
    calendarHeading: 'grow text-center',
    calendarGrid: '',
    calendarGridHeader: 'flex',
    calendarGridHeaderCell: '',
    calendarGridBody: '',
    calendarGridCell:
      'text-center px-1 rounded border border-transparent hover:bg-primary/10 outside-visible-range:text-primary/30 disabled:text-primary/30 selected:border-primary/40',
  },
  defaultVariants: {
    size: 'medium',
  },
})

/** Props for a {@link TimeField}. */
export interface TimeFieldProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, TimeValue>,
> extends Pick<AriaTimeFieldProps<TimeValue>, 'granularity'>,
    FieldStateProps<
      Omit<
        AriaTimeFieldProps<Extract<FieldValues<Schema>[TFieldName], TimeValue>>,
        'children' | 'className' | 'style'
      >,
      Schema,
      TFieldName,
      TimeValue
    >,
    FieldProps,
    Pick<FieldComponentProps<Schema>, 'className' | 'style'>,
    VariantProps<typeof DATE_PICKER_STYLES> {
  readonly noResetButton?: boolean
  readonly segments?: Partial<Record<DateSegmentType['type'], boolean>>
}

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
const useTimeValueField = Form.makeUseField<TimeValue>()

/** A date picker. */
export const TimeField = forwardRef(function TimeField<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, TimeValue>,
>(props: TimeFieldProps<Schema, TFieldName>, ref: ForwardedRef<HTMLDivElement>) {
  const {
    isRequired = false,
    noResetButton = isRequired,
    segments = {},
    name,
    isDisabled,
    form,
    defaultValue,
    label,
    className,
    size,
    variants = DATE_PICKER_STYLES,
    granularity,
    style,
    isInvalid,
    contextualHelp,
    ...rest
  } = props

  const { fieldState, formInstance } = useTimeValueField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const styles = variants({ size })

  return (
    <Form.Field
      preventLabelFocus
      form={formInstance}
      name={name}
      fullWidth
      label={label}
      aria-label={props['aria-label']}
      aria-labelledby={props['aria-labelledby']}
      aria-describedby={props['aria-describedby']}
      isRequired={isRequired}
      isInvalid={fieldState.invalid}
      aria-details={props['aria-details']}
      ref={ref}
      style={style}
      contextualHelp={contextualHelp}
    >
      <Form.Controller
        control={formInstance.control}
        name={name}
        render={(renderProps) => (
          <AriaTimeField
            {...rest}
            isInvalid={isInvalid ?? false}
            className={styles.base({ className })}
            {...(granularity != null ? { granularity } : {})}
            {...renderProps.field}
          >
            <Label />
            <Group className={styles.inputGroup()}>
              <DateInput className={styles.dateInput()}>
                {(segment) =>
                  segments[segment.type] === false ?
                    <></>
                  : <DateSegment segment={segment} className={styles.dateSegment()} />
                }
              </DateInput>
              {!noResetButton && <TimeFieldResetButton className={styles.resetButton()} />}
            </Group>
            {props.description != null && <Text slot="description" />}
          </AriaTimeField>
        )}
      />
    </Form.Field>
  )
})

/** Props for a {@link TimeFieldResetButton}. */
interface TimeFieldResetButtonProps {
  readonly className?: string
}

/** A reset button for a {@link TimeField}. */
function TimeFieldResetButton(props: TimeFieldResetButtonProps) {
  const { className } = props
  const state = useContext(TimeFieldStateContext)
  const { getText } = useText()

  return (
    <Button
      // Do not inherit default Button behavior from TimeField.
      slot={null}
      variant="icon"
      aria-label={getText('reset')}
      icon={CrossIcon}
      className={className ?? ''}
      onPress={() => {
        state?.setValue(null)
      }}
    />
  )
}
