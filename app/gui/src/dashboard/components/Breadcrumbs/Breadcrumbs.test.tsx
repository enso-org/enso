/**
 * @file Tests for the Breadcrumbs component.
 */

import '@testing-library/jest-dom'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { describe, expect, it, vi } from 'vitest'
import { BreadcrumbItem, Breadcrumbs } from '.'

describe('Breadcrumbs', () => {
  it('renders all items when there is enough space', () => {
    render(
      <Breadcrumbs>
        <BreadcrumbItem onPress={() => {}}>Home</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Projects</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}} isDisabled>
          Current Project
        </BreadcrumbItem>
      </Breadcrumbs>,
    )

    expect(screen.getByText('Home')).toBeInTheDocument()
    expect(screen.getByText('Projects')).toBeInTheDocument()
    expect(screen.getByText('Current Project')).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: 'Show more breadcrumbs' })).not.toBeInTheDocument()
  })

  it('shows a menu trigger when items overflow', () => {
    render(
      <Breadcrumbs>
        <BreadcrumbItem onPress={() => {}}>Home</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Projects</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Team</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Documents</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Reports</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}} isDisabled>
          Current Report
        </BreadcrumbItem>
      </Breadcrumbs>,
    )

    expect(screen.getByText('Home')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Show more breadcrumbs' })).toBeInTheDocument()
    expect(screen.getByText('Current Report')).toBeInTheDocument()
  })

  it('shows hidden items in a menu when menu trigger is clicked', async () => {
    const user = userEvent.setup()

    render(
      <Breadcrumbs>
        <BreadcrumbItem onPress={() => {}}>Home</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Projects</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Team</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Documents</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Reports</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}} isDisabled>
          Current Report
        </BreadcrumbItem>
      </Breadcrumbs>,
    )

    await user.click(screen.getByRole('button', { name: 'Show more breadcrumbs' }))

    expect(screen.getByText('Team')).toBeInTheDocument()
    expect(screen.getByText('Documents')).toBeInTheDocument()
    expect(screen.getByText('Reports')).toBeInTheDocument()
  })

  it('calls onPress when a breadcrumb item is clicked', async () => {
    const user = userEvent.setup()
    const onPress = vi.fn()

    render(
      <Breadcrumbs>
        <BreadcrumbItem onPress={onPress}>Home</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Projects</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}} isDisabled>
          Current Project
        </BreadcrumbItem>
      </Breadcrumbs>,
    )

    await user.click(screen.getByText('Home'))
    expect(onPress).toHaveBeenCalledTimes(1)
  })

  it('disables interaction when isDisabled is true', async () => {
    const user = userEvent.setup()
    const onPress = vi.fn()

    render(
      <Breadcrumbs>
        <BreadcrumbItem onPress={() => {}}>Home</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Projects</BreadcrumbItem>
        <BreadcrumbItem onPress={onPress} isDisabled>
          Current Project
        </BreadcrumbItem>
      </Breadcrumbs>,
    )

    await user.click(screen.getByText('Current Project'))
    expect(onPress).not.toHaveBeenCalled()
  })

  it('renders a suffix element when provided', () => {
    render(
      <Breadcrumbs>
        <BreadcrumbItem onPress={() => {}}>Home</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}}>Projects</BreadcrumbItem>
        <BreadcrumbItem onPress={() => {}} suffix={<span data-testid="suffix">Suffix</span>}>
          Current Project
        </BreadcrumbItem>
      </Breadcrumbs>,
    )

    expect(screen.getByTestId('suffix')).toBeInTheDocument()
  })
})
