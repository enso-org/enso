/** @file A date picker. */
import { useContext, type ForwardedRef } from 'react'

import type { DateSegment as DateSegmentType } from 'react-stately'

import CrossIcon from '#/assets/cross.svg'
import ArrowIcon from '#/assets/folder_arrow.svg'
import {
  DatePicker as AriaDatePicker,
  Calendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  DateInput,
  DatePickerStateContext,
  DateSegment,
  Dialog,
  Group,
  Heading,
  I18nProvider,
  Label,
  type DatePickerProps as AriaDatePickerProps,
  type DateValue,
} from '#/components/aria'
import { useText } from '#/providers/TextProvider'
import { forwardRef } from '#/utilities/react'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import {
  Button,
  Form,
  Popover,
  Text,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type TSchema,
} from '../..'
// This cannot be added to the import above or else it is `undefined` due to a circular import.
import { makeRoundedStyles } from '../../utilities'

const DATE_PICKER_STYLES = tv({
  base: '',
  variants: {
    rounded: makeRoundedStyles('inputContainer'),
    size: {
      small: {
        inputContainer: 'h-6 px-2',
      },
      medium: {
        inputContainer: 'h-8 px-4',
      },
    },
  },
  slots: {
    inputContainer: 'flex items-center gap-2 rounded-full border-0.5 border-primary/20',
    dateInput: 'flex justify-start grow order-2',
    dateSegment: 'rounded placeholder-shown:text-primary/30 focus:bg-primary/10 px-[0.5px]',
    calendarButton: 'order-1 rotate-90',
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
    rounded: 'xlarge',
  },
})

/** Props for a {@link DatePicker}. */
export interface DatePickerProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, DateValue>,
> extends Pick<AriaDatePickerProps<DateValue>, 'granularity'>,
    FieldStateProps<
      Omit<
        AriaDatePickerProps<Extract<FieldValues<Schema>[TFieldName], DateValue>>,
        'children' | 'className' | 'style'
      >,
      Schema,
      TFieldName,
      DateValue
    >,
    FieldProps,
    Pick<FieldComponentProps<Schema>, 'className' | 'style'>,
    VariantProps<typeof DATE_PICKER_STYLES> {
  readonly noResetButton?: boolean
  readonly noCalendarHeader?: boolean
  readonly segments?: Partial<Record<DateSegmentType['type'], boolean>>
}

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
const useDateValueField = Form.makeUseField<DateValue>()

/** A date picker. */
export const DatePicker = forwardRef(function DatePicker<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, DateValue>,
>(props: DatePickerProps<Schema, TFieldName>, ref: ForwardedRef<HTMLDivElement>) {
  const {
    isRequired = false,
    noResetButton = isRequired,
    noCalendarHeader = false,
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
    isInvalid,
    style,
    rounded,
    ...rest
  } = props

  const { fieldState, formInstance } = useDateValueField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const styles = variants({ size, rounded })

  return (
    <Form.Field
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
    >
      <Form.Controller
        control={formInstance.control}
        name={name}
        render={(renderProps) => (
          <AriaDatePicker
            {...rest}
            isInvalid={isInvalid ?? false}
            className={styles.base({ className })}
            {...(granularity != null ? { granularity } : {})}
            {...renderProps.field}
          >
            <Label />
            <Group className={styles.inputContainer()}>
              {/* Use Swedish locale (`sv`) because it uses ISO dates. */}
              <I18nProvider locale="sv">
                <DateInput className={styles.dateInput()}>
                  {(segment) =>
                    segments[segment.type] === false ?
                      <></>
                    : <DateSegment
                        segment={segment}
                        className={styles.dateSegment({
                          className:
                            segment.type === 'literal' && segment.text === ' ' ? 'w-1.5' : '',
                        })}
                      />
                  }
                </DateInput>
              </I18nProvider>
              <Button variant="icon" icon={ArrowIcon} className={styles.calendarButton()} />
              {!noResetButton && <DatePickerResetButton className={styles.resetButton()} />}
            </Group>
            {props.description != null && <Text slot="description" />}
            <Popover size="auto" className={styles.calendarPopover()}>
              <Dialog className={styles.calendarDialog()}>
                <Calendar className={styles.calendarContainer()}>
                  <header className={styles.calendarHeader()}>
                    <Button
                      variant="icon"
                      slot="previous"
                      icon={ArrowIcon}
                      className="rotate-180"
                    />
                    <Heading className={styles.calendarHeading()} />
                    <Button variant="icon" slot="next" icon={ArrowIcon} />
                  </header>
                  <CalendarGrid className={styles.calendarGrid()}>
                    {noCalendarHeader ?
                      <></>
                    : <CalendarGridHeader className={styles.calendarGridHeader()}>
                        {() => <CalendarHeaderCell className={styles.calendarGridHeaderCell()} />}
                      </CalendarGridHeader>
                    }
                    <CalendarGridBody className={styles.calendarGridBody()}>
                      {(date) => <CalendarCell date={date} className={styles.calendarGridCell()} />}
                    </CalendarGridBody>
                  </CalendarGrid>
                  <Text slot="errorMessage" />
                </Calendar>
              </Dialog>
            </Popover>
          </AriaDatePicker>
        )}
      />
    </Form.Field>
  )
})

/** Props for a {@link DatePickerResetButton}. */
interface DatePickerResetButtonProps {
  readonly className?: string
}

/** A reset button for a {@link DatePicker}. */
function DatePickerResetButton(props: DatePickerResetButtonProps) {
  const { className } = props
  const state = useContext(DatePickerStateContext)
  const { getText } = useText()

  return (
    <Button
      // Do not inherit default Button behavior from DatePicker.
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
