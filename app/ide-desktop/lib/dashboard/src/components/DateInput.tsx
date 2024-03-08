/** @file An input that outputs a {@link Date}. */
import * as React from 'react'

import FolderArrowDoubleIcon from 'enso-assets/folder_arrow_double.svg'
import FolderArrowIcon from 'enso-assets/folder_arrow.svg'

import SvgMask from '#/components/SvgMask'

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
      <button
        className={`border border-primary/10 rounded-full px-2 h-6 w-30 ${date == null ? 'opacity-75' : ''}`}
        onClick={() => {
          setIsPickerVisible(!isPickerVisible)
        }}
      >
        {date != null ? dateTime.formatDate(date) : 'No date selected'}
      </button>
      {isPickerVisible && (
        <div className="absolute top-6 mt-2 left-1/2">
          <div className="relative rounded-2xl -translate-x-1/2 p-2 shadow-soft border border-primary/10 before:absolute before:inset-0 before:rounded-2xl before:backdrop-blur-3xl">
            <table className="relative w-full">
              <caption className="caption-top mb-2">
                <div className="flex items-center">
                  <button
                    className="inline-flex rounded-md hover:bg-black/10"
                    onClick={() => {
                      setSelectedYear(selectedYear - 1)
                    }}
                  >
                    <SvgMask src={FolderArrowDoubleIcon} className="rotate-180" />
                  </button>
                  <button
                    className="inline-flex rounded-md hover:bg-black/10"
                    onClick={() => {
                      if (selectedMonthIndex === 0) {
                        setSelectedYear(selectedYear - 1)
                        setSelectedMonthIndex(LAST_MONTH_INDEX)
                      } else {
                        setSelectedMonthIndex(selectedMonthIndex - 1)
                      }
                    }}
                  >
                    <SvgMask src={FolderArrowIcon} className="rotate-180" />
                  </button>
                  <span className="grow">
                    {dateTime.MONTH_NAMES[selectedMonthIndex]} {selectedYear}
                  </span>
                  <button
                    className="inline-flex rounded-md hover:bg-black/10"
                    onClick={() => {
                      if (selectedMonthIndex === LAST_MONTH_INDEX) {
                        setSelectedYear(selectedYear + 1)
                        setSelectedMonthIndex(0)
                      } else {
                        setSelectedMonthIndex(selectedMonthIndex + 1)
                      }
                    }}
                  >
                    <SvgMask src={FolderArrowIcon} />
                  </button>
                  <button
                    className="inline-flex rounded-md hover:bg-black/10"
                    onClick={() => {
                      setSelectedYear(selectedYear + 1)
                    }}
                  >
                    <SvgMask src={FolderArrowDoubleIcon} />
                  </button>
                </div>
              </caption>
              <thead>
                <tr>
                  <th className="p-0 min-w-6 h-5 leading-144.5 py-px">M</th>
                  <th className="p-0 min-w-6 h-5 leading-144.5 py-px">Tu</th>
                  <th className="p-0 min-w-6 h-5 leading-144.5 py-px">W</th>
                  <th className="p-0 min-w-6 h-5 leading-144.5 py-px">Th</th>
                  <th className="p-0 min-w-6 h-5 leading-144.5 py-px">F</th>
                  <th className="p-0 min-w-6 h-5 leading-144.5 py-px">Sa</th>
                  <th className="p-0 min-w-6 h-5 leading-144.5 py-px">Su</th>
                </tr>
              </thead>
              <tbody>
                {month.map((week, i) => (
                  <tr key={i}>
                    {week.map((day, j) => (
                      <td
                        key={j}
                        className="p-0 h-5 leading-144.5 py-px"
                        onClick={() => {
                          setIsPickerVisible(false)
                          onInput(
                            new Date(selectedYear, selectedMonthIndex + day.monthOffset, day.date)
                          )
                        }}
                      >
                        <button
                          className={`w-full text-center rounded-md hover:bg-black/10 ${day.monthOffset === 0 ? '' : 'opacity-50'}`}
                        >
                          {day.date}
                        </button>
                      </td>
                    ))}
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
