import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { useEffect, useState } from 'react'
import { describe } from 'vitest'
import { Activity } from '../Activity'

describe('Activity', (it) => {
  it('should render the children', ({ expect }) => {
    render(
      <Activity mode="active">
        <div>Hello</div>
      </Activity>,
    )

    expect(screen.getByText('Hello')).toBeInTheDocument()
  })

  it('should render children when inactive', ({ expect }) => {
    render(
      <Activity mode="inactive">
        <div>Hello</div>
      </Activity>,
    )

    expect(screen.getByText('Hello')).toBeInTheDocument()
  })

  it('should render children when inactive-hidden', ({ expect }) => {
    render(
      <Activity mode="inactive-hidden">
        <div>Hello</div>
      </Activity>,
    )

    expect(screen.getByText('Hello')).toBeInTheDocument()
  })

  it('should display children when inactive', ({ expect }) => {
    render(
      <Activity mode="inactive">
        <div>Hello</div>
      </Activity>,
    )

    expect(screen.getByText('Hello')).toBeVisible()
  })

  it('should not unmount children when inactive', async ({ expect }) => {
    let count = 0

    const Component = () => {
      useEffect(
        () => () => {
          count++
        },
        [],
      )

      return <div>{count}</div>
    }

    function Container() {
      const [mode, setMode] = useState<'active' | 'inactive'>('active')

      return (
        <div>
          <button
            onClick={() => {
              setMode('inactive')
            }}
          >
            Inactive
          </button>
          <button
            onClick={() => {
              setMode('active')
            }}
          >
            Active
          </button>
          <Activity mode={mode}>
            <Component />
          </Activity>
        </div>
      )
    }

    render(<Container />)

    await userEvent.click(screen.getByText('Inactive'))
    await userEvent.click(screen.getByText('Active'))

    expect(count).toBe(0)
  })
})
