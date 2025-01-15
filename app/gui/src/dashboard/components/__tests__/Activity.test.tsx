import { render, screen } from '@testing-library/react'
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

  it('should render the when inactive', ({ expect }) => {
    render(
      <Activity mode="inactive">
        <div>Hello</div>
      </Activity>,
    )

    expect(screen.getByText('Hello')).toBeInTheDocument()
  })

  it('should render the when inactive-hidden', ({ expect }) => {
    render(
      <Activity mode="inactive-hidden">
        <div>Hello</div>
      </Activity>,
    )

    expect(screen.getByText('Hello')).toBeInTheDocument()
  })

  it('should display the children when inactive', ({ expect }) => {
    render(
      <Activity mode="inactive">
        <div>Hello</div>
      </Activity>,
    )

    expect(screen.getByText('Hello')).toBeVisible()
  })

  it('should not unmount the children when inactive', ({ expect }) => {
    const Component = () => {
      const [count, setCount] = useState(0)

      useEffect(() => {
        return () => {
          setCount(count + 1)
        }
      }, [])

      return <div>{count}</div>
    }

    const { rerender } = render(
      <Activity mode="active">
        <Component />
      </Activity>,
    )

    rerender(
      <Activity mode="inactive">
        <Component />
      </Activity>,
    )

    expect(screen.getByText('0')).toBeInTheDocument()
  })
})
