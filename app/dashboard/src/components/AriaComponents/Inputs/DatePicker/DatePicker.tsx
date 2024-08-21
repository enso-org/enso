/** @file A date picker. */
import type { ForwardedRef } from 'react'

import type { DateSegment as DateSegmentType } from 'react-stately'

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
  DateSegment,
  Dialog,
  Group,
  Heading,
  Label,
  type DatePickerProps as AriaDatePickerProps,
  type DateValue,
} from '#/components/aria'
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
} from '#/components/AriaComponents'
import { forwardRef } from '#/utilities/react'
import { Controller } from 'react-hook-form'
import { tv, VariantProps } from 'tailwind-variants'

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
    calendarPopover: 'w-0',
    calendarDialog: 'text-primary text-xs',
    calendarContainer: '',
    calendarHeader: 'flex items-center mb-2',
    calendarHeading: 'grow text-center',
    calendarGrid: '',
    calendarGridHeader: 'flex',
    calendarGridHeaderCell: '',
    calendarGridBody: '',
    calendarGridCell:
      'text-center px-1 rounded hover:bg-primary/10 outside-visible-range:text-primary/30 disabled:text-primary/30',
  },
  defaultVariants: {
    size: 'medium',
  },
})

/** Props for a {@link DatePicker}. */
export interface DatePickerProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<
      Omit<
        AriaDatePickerProps<Extract<FieldValues<Schema>[TFieldName], DateValue>>,
        'children' | 'className' | 'style'
      >,
      Schema,
      TFieldName
    >,
    FieldProps,
    Pick<FieldComponentProps<Schema>, 'className' | 'style'>,
    VariantProps<typeof DATE_PICKER_STYLES> {
  readonly noCalendarHeader?: boolean
  readonly segments?: Partial<Record<DateSegmentType['type'], boolean>>
}

/** A date picker. */
export const DatePicker = forwardRef(function DatePicker<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: DatePickerProps<Schema, TFieldName>, ref: ForwardedRef<HTMLFieldSetElement>) {
  const {
    noCalendarHeader = false,
    segments = {},
    name,
    isDisabled,
    form,
    defaultValue,
    label,
    isRequired,
    className,
    size,
  } = props

  const { fieldState, formInstance } = Form.useField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const styles = DATE_PICKER_STYLES({ size })
  console.log(className, styles.base({ className }))

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
      style={props.style}
    >
      <Controller
        control={formInstance.control}
        name={name}
        render={(renderProps) => {
          return (
            <AriaDatePicker className={styles.base({ className })} {...renderProps.field}>
              <Label />
              <Group className={styles.inputGroup()}>
                <DateInput className={styles.dateInput()}>
                  {(segment) =>
                    segments[segment.type] === false ?
                      <></>
                    : <DateSegment segment={segment} className={styles.dateSegment()} />
                  }
                </DateInput>
                <Button variant="icon" icon={ArrowIcon} className="rotate-90" />
              </Group>
              {props.description && <Text slot="description" />}
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
                        {(date) => (
                          <CalendarCell date={date} className={styles.calendarGridCell()} />
                        )}
                      </CalendarGridBody>
                    </CalendarGrid>
                    <Text slot="errorMessage" />
                  </Calendar>
                </Dialog>
              </Popover>
            </AriaDatePicker>
          )
        }}
      />
    </Form.Field>
  )
})
