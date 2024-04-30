/**
 * @file
 *
 * InvoicesTable component
 */

import * as React from 'react'

import OpenInNewIcon from 'enso-assets/open.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

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

  const { locale, getText } = textProvider.useText()

  return (
    <aria.ResizableTableContainer>
      <aria.Table aria-labelledby={titleId} className="-mx-2 text-left rounded-rows">
        <aria.TableHeader>
          <invocesColumn.InvoicesColumn
            id="symbol"
            allowsSorting
            className="h-full min-w-drive-name-column border-2 border-y border-l-0 border-transparent bg-clip-padding last:w-full last:rounded-r-full last:border-r-0"
          >
            <ariaComponents.Text weight="semibold" variant="subtitle">
              Date
            </ariaComponents.Text>
          </invocesColumn.InvoicesColumn>
          <invocesColumn.InvoicesColumn
            id="name"
            isRowHeader
            allowsSorting
            className="h-full min-w-drive-name-column border-2 border-y border-l-0 border-transparent bg-clip-padding rounded-rows-skip-level last:w-full last:rounded-r-full last:border-r-0"
          >
            <ariaComponents.Text weight="semibold" variant="subtitle">
              Amount
            </ariaComponents.Text>
          </invocesColumn.InvoicesColumn>
          <invocesColumn.InvoicesColumn
            id="sector"
            allowsSorting
            defaultWidth="2fr"
            className="h-full min-w-drive-name-column border-2 border-y border-l-0 border-transparent bg-clip-padding rounded-rows-skip-level last:w-full last:rounded-r-full last:border-r-0"
          >
            <ariaComponents.Text weight="semibold" variant="subtitle">
              Description
            </ariaComponents.Text>
          </invocesColumn.InvoicesColumn>
        </aria.TableHeader>

        <aria.TableBody items={items}>
          {item => (
            <invoicesRow.InvoicesRow key={item.id}>
              <invoicesCell.InvoicesCell>
                <div className="flex items-center gap-1">
                  <ariaComponents.Text>
                    {new Date(item.date).toLocaleString(locale, {
                      year: 'numeric',
                      month: 'long',
                      day: 'numeric',
                    })}
                  </ariaComponents.Text>
                  <ariaComponents.Button
                    variant="icon"
                    icon={OpenInNewIcon}
                    aria-label={getText('downloadInvoice')}
                    target="_blank"
                    size="small"
                  />
                </div>
              </invoicesCell.InvoicesCell>
              <invoicesCell.InvoicesCell>
                <div className="flex items-center gap-2">
                  <ariaComponents.Text weight="semibold">
                    {item.amount.toLocaleString(locale, {
                      style: 'currency',
                      currency: 'USD',
                    })}
                  </ariaComponents.Text>
                  <ariaComponents.Text
                    variant="body"
                    className="rounded-full border border-share px-2 pb-[1px]"
                    color="success"
                    disableLineHeightCompensation
                  >
                    {item.status}
                  </ariaComponents.Text>
                </div>
              </invoicesCell.InvoicesCell>
              <invoicesCell.InvoicesCell>
                <ariaComponents.Text truncate="3">{item.description}</ariaComponents.Text>
              </invoicesCell.InvoicesCell>
            </invoicesRow.InvoicesRow>
          )}
        </aria.TableBody>
      </aria.Table>
    </aria.ResizableTableContainer>
  )
}
