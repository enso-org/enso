/** @file An input that outputs a {@link Date}. */
import * as React from 'react'

import FolderArrowIcon from 'enso-assets/folder_arrow.svg'

import SvgMask from '#/components/SvgMask'

import * as dateTime from '#/utilities/dateTime'

// =================
// === Constants ===
// =================

// The month index of the last month i the year (December).
const LAST_MONTH_INDEX = 11

// ======================
// === DateInputProps ===
// ======================

/** Props for a {@link DateInput}. */
export interface DateInputProps {
  readonly date: Date | null
  readonly onInput: (date: Date) => void
}

/** An input that outputs a {@link Date}. */
export default function DateInput(props: DateInputProps) {
  const { date, onInput } = props
  const year = date?.getUTCFullYear() ?? new Date().getUTCFullYear()
  const monthIndex = date?.getUTCMonth() ?? new Date().getUTCMonth()
  const [selectedYear, setSelectedYear] = React.useState(year)
  const [selectedMonthIndex, setSelectedMonthIndex] = React.useState(monthIndex)
  const month = React.useMemo(() => {
    const firstOfMonth = new Date(selectedYear, selectedMonthIndex, 1)
    return [[{ monthOffset: 0, date: 1 }]]
  }, [selectedYear, selectedMonthIndex])

  React.useEffect(() => {
    setSelectedYear(year)
    setSelectedMonthIndex(monthIndex)
  }, [year, monthIndex])

  return (
    <div className="flex flex-col">
      <div>{date != null ? dateTime.toRfc3339(date) : ''}</div>
      <table>
        <caption className="flex">
          <button
            onClick={() => {
              if (selectedMonthIndex === 0) {
                setSelectedYear(selectedYear - 1)
                setSelectedMonthIndex(LAST_MONTH_INDEX)
              } else {
                setSelectedMonthIndex(selectedMonthIndex - 1)
              }
            }}
          >
            <SvgMask src={FolderArrowIcon} />
          </button>
          {dateTime.MONTH_NAMES[selectedMonthIndex]} {selectedYear}
          <button
            onClick={() => {
              if (selectedMonthIndex === LAST_MONTH_INDEX) {
                setSelectedYear(selectedYear + 1)
                setSelectedMonthIndex(0)
              } else {
                setSelectedMonthIndex(selectedMonthIndex + 1)
              }
            }}
          >
            <SvgMask src={FolderArrowIcon} className="rotate-180" />
          </button>
        </caption>
        <thead>
          <tr>
            <th>M</th>
            <th>Tu</th>
            <th>W</th>
            <th>Th</th>
            <th>F</th>
            <th>Sa</th>
            <th>Su</th>
          </tr>
        </thead>
        <tbody>
          {month.map((week, i) => (
            <tr key={i}>
              {week.map((day, j) => (
                <td
                  key={j}
                  onClick={() => {
                    onInput(new Date(selectedYear, selectedMonthIndex + day.monthOffset, day.date))
                  }}
                >
                  <button className={day.monthOffset === 0 ? '' : 'opacity-75'}>{day.date}</button>
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}
