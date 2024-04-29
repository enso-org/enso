/** @file An input that outputs a {@link Date}. */
import * as React from 'react'

import CrossIcon from 'enso-assets/cross.svg'
import FolderArrowDoubleIcon from 'enso-assets/folder_arrow_double.svg'
import FolderArrowIcon from 'enso-assets/folder_arrow.svg'

import * as focusHooks from '#/hooks/focusHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

import * as dateTime from '#/utilities/dateTime'

// =================
// === Constants ===
// =================

/** The number of days in a week. */
const DAYS_IN_WEEK = 7
/** The month index of the last month i the year (December). */
const LAST_MONTH_INDEX = 11

// =============
// === Types ===
// =============

/** Information required to render a specific day. */
interface DayInfo {
  readonly monthOffset: number
  readonly date: number
}

// ======================
// === DateInputProps ===
// ======================

/** Props for a {@link DateInput}. */
export interface DateInputProps {
  readonly date: Date | null
  readonly onInput: (date: Date | null) => void
}

/** An input that outputs a {@link Date}. */
export default function DateInput(props: DateInputProps) {
  const { date, onInput } = props
  const { getText } = textProvider.useText()
  const focusChildProps = focusHooks.useFocusChild()
  const year = date?.getFullYear() ?? new Date().getFullYear()
  const monthIndex = date?.getMonth() ?? new Date().getMonth()
  const [isPickerVisible, setIsPickerVisible] = React.useState(false)
  const [selectedYear, setSelectedYear] = React.useState(year)
  const [selectedMonthIndex, setSelectedMonthIndex] = React.useState(monthIndex)
  const month = React.useMemo<readonly (readonly DayInfo[])[]>(() => {
    const currentDay = new Date(selectedYear, selectedMonthIndex, 1)
    const dayOfWeek = (currentDay.getDay() + DAYS_IN_WEEK - 1) % DAYS_IN_WEEK
    currentDay.setDate(currentDay.getDate() - dayOfWeek)
    const result: DayInfo[][] = []
    let lastMonthIndex = currentDay.getMonth()
    let monthOffset = currentDay.getMonth() === selectedMonthIndex ? 0 : -1
    do {
      const week: DayInfo[] = []
      for (let i = 0; i < DAYS_IN_WEEK; i += 1) {
        week.push({ monthOffset, date: currentDay.getDate() })
        currentDay.setDate(currentDay.getDate() + 1)
        if (currentDay.getMonth() !== lastMonthIndex) {
          monthOffset += 1
          lastMonthIndex = currentDay.getMonth()
        }
      }
      result.push(week)
      // This MUST be a `do ... while ...` loop since the first day may not be in the same month.
    } while (currentDay.getMonth() === selectedMonthIndex)
    return result
  }, [selectedYear, selectedMonthIndex])

  React.useEffect(() => {
    setSelectedYear(year)
    setSelectedMonthIndex(monthIndex)
  }, [year, monthIndex])

  React.useEffect(() => {
    const onClick = () => {
      setIsPickerVisible(false)
    }
    document.addEventListener('click', onClick)
    return () => {
      document.removeEventListener('click', onClick)
    }
  })

  return (
    <div
      className="relative flex flex-col"
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <FocusRing>
        <div
          {...aria.mergeProps<JSX.IntrinsicElements['div']>()(focusChildProps, {
            role: 'button',
            tabIndex: 0,
            className: `flex h-text w-date-picker items-center rounded-full border border-primary/10 px-date-input transition-colors hover:[&:not(:has(button:hover))]:bg-hover-bg ${date == null ? 'placeholder' : ''}`,
            onClick: event => {
              event.stopPropagation()
              setIsPickerVisible(!isPickerVisible)
            },
            onKeyDown: event => {
              if (event.key === 'Enter' || event.key === 'Space') {
                event.stopPropagation()
                setIsPickerVisible(!isPickerVisible)
              }
            },
          })}
        >
          <div className="flex grow flex-col items-center">
            {date != null ? dateTime.formatDate(date) : getText('noDateSelected')}
          </div>
          {date != null && (
            <UnstyledButton
              className="flex rounded-full transition-colors hover:bg-hover-bg"
              onPress={() => {
                onInput(null)
              }}
            >
              <SvgMask src={CrossIcon} className="size-icon" />
            </UnstyledButton>
          )}
        </div>
      </FocusRing>
      {isPickerVisible && (
        <div className="absolute left-1/2 top-text-h mt-date-input-gap">
          <div className="relative -translate-x-1/2 rounded-2xl border border-primary/10 p-date-input shadow-soft before:absolute before:inset-0 before:rounded-2xl before:backdrop-blur-3xl">
            <div className="relative mb-date-input-gap">
              <div className="flex items-center">
                <UnstyledButton
                  className="inline-flex rounded-small-rectangle-button hover:bg-hover-bg"
                  onPress={() => {
                    setSelectedYear(selectedYear - 1)
                  }}
                >
                  <SvgMask src={FolderArrowDoubleIcon} className="rotate-180" />
                </UnstyledButton>
                <UnstyledButton
                  className="inline-flex rounded-small-rectangle-button hover:bg-black/10"
                  onPress={() => {
                    if (selectedMonthIndex === 0) {
                      setSelectedYear(selectedYear - 1)
                      setSelectedMonthIndex(LAST_MONTH_INDEX)
                    } else {
                      setSelectedMonthIndex(selectedMonthIndex - 1)
                    }
                  }}
                >
                  <SvgMask src={FolderArrowIcon} className="rotate-180" />
                </UnstyledButton>
                <aria.Text className="grow text-center">
                  {dateTime.MONTH_NAMES[selectedMonthIndex]} {selectedYear}
                </aria.Text>
                <UnstyledButton
                  className="inline-flex rounded-small-rectangle-button hover:bg-black/10"
                  onPress={() => {
                    if (selectedMonthIndex === LAST_MONTH_INDEX) {
                      setSelectedYear(selectedYear + 1)
                      setSelectedMonthIndex(0)
                    } else {
                      setSelectedMonthIndex(selectedMonthIndex + 1)
                    }
                  }}
                >
                  <SvgMask src={FolderArrowIcon} />
                </UnstyledButton>
                <UnstyledButton
                  className="inline-flex rounded-small-rectangle-button hover:bg-black/10"
                  onPress={() => {
                    setSelectedYear(selectedYear + 1)
                  }}
                >
                  <SvgMask src={FolderArrowDoubleIcon} />
                </UnstyledButton>
              </div>
            </div>
            <table className="relative w-full">
              <thead>
                <tr>
                  <th className="text-tight min-w-date-cell p">{getText('mondayAbbr')}</th>
                  <th className="text-tight min-w-date-cell p">{getText('tuesdayAbbr')}</th>
                  <th className="text-tight min-w-date-cell p">{getText('wednesdayAbbr')}</th>
                  <th className="text-tight min-w-date-cell p">{getText('thursdayAbbr')}</th>
                  <th className="text-tight min-w-date-cell p">{getText('fridayAbbr')}</th>
                  <th className="text-tight min-w-date-cell p">{getText('saturdayAbbr')}</th>
                  <th className="text-tight min-w-date-cell p">{getText('sundayAbbr')}</th>
                </tr>
              </thead>
              <tbody>
                {month.map((week, i) => (
                  <tr key={i}>
                    {week.map((day, j) => {
                      const currentDate = new Date(
                        selectedYear,
                        selectedMonthIndex + day.monthOffset,
                        day.date
                      )
                      const isSelectedDate =
                        date != null &&
                        currentDate.getFullYear() === year &&
                        currentDate.getMonth() === monthIndex &&
                        currentDate.getDate() === date.getDate()
                      return (
                        <td key={j} className="text-tight p">
                          <UnstyledButton
                            isDisabled={isSelectedDate}
                            className={`w-full rounded-small-rectangle-button text-center hover:bg-primary/10 disabled:bg-frame disabled:font-bold ${day.monthOffset === 0 ? '' : 'opacity-unimportant'}`}
                            onPress={() => {
                              setIsPickerVisible(false)
                              onInput(currentDate)
                            }}
                          >
                            {day.date}
                          </UnstyledButton>
                        </td>
                      )
                    })}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}
    </div>
  )
}
