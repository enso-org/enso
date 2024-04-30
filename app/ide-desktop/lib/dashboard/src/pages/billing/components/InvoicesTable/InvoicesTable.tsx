/**
 * @file
 *
 * InvoicesTable component
 */

import * as React from 'react'

import * as aria from '#/components/aria'

import * as invoicesCell from './InvoicesCell'
import * as invocesColumn from './InvoicesColumn'
import * as invoicesRow from './InvoicesRow'

/**
 *
 */
export interface InvoicesTableProps {
  readonly titleId: string
  readonly items: ReadonlyArray<{
    readonly id: string
    readonly date: string
    readonly amount: number
    readonly status: string
    readonly description: string
  }>
}

/**
 *
 */
export function InvoicesTable(props: InvoicesTableProps) {
  const { titleId, items } = props

  return (
    <aria.ResizableTableContainer>
      <aria.Table aria-labelledby={titleId} className="text-left text-sm font-bold text-black-a50">
        <aria.TableHeader>
          <invocesColumn.InvoicesColumn
            id="symbol"
            allowsSorting
            className="h-full min-w-drive-name-column border-2 border-y border-l-0 border-transparent bg-clip-padding last:w-full last:rounded-r-full last:border-r-0"
          >
            Date
          </invocesColumn.InvoicesColumn>
          <invocesColumn.InvoicesColumn
            id="name"
            isRowHeader
            allowsSorting
            defaultWidth="3fr"
            className="h-full min-w-drive-name-column border-2 border-y border-l-0 border-transparent bg-clip-padding rounded-rows-skip-level last:w-full last:rounded-r-full last:border-r-0"
          >
            Amount
          </invocesColumn.InvoicesColumn>
          <invocesColumn.InvoicesColumn
            id="marketCap"
            allowsSorting
            className="h-full min-w-drive-name-column border-2 border-y border-l-0 border-transparent bg-clip-padding last:w-full last:rounded-r-full last:border-r-0"
          >
            Status
          </invocesColumn.InvoicesColumn>
          <invocesColumn.InvoicesColumn
            id="sector"
            allowsSorting
            className="h-full min-w-drive-name-column border-2 border-y border-l-0 border-transparent bg-clip-padding rounded-rows-skip-level last:w-full last:rounded-r-full last:border-r-0"
          >
            Description
          </invocesColumn.InvoicesColumn>
        </aria.TableHeader>

        <aria.TableBody items={items}>
          {item => (
            <invoicesRow.InvoicesRow key={item.id} className="rounded-rows-skip-level">
              <invoicesCell.InvoicesCell>{item.date}</invoicesCell.InvoicesCell>
              <invoicesCell.InvoicesCell className="font-semibold">
                {item.amount}
              </invoicesCell.InvoicesCell>
              <invoicesCell.InvoicesCell>{item.status}</invoicesCell.InvoicesCell>
              <invoicesCell.InvoicesCell>{item.description}</invoicesCell.InvoicesCell>
            </invoicesRow.InvoicesRow>
          )}
        </aria.TableBody>
      </aria.Table>
    </aria.ResizableTableContainer>
  )
}
