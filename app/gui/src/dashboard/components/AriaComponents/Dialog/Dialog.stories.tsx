import type { Meta, StoryObj } from '@storybook/react'
import { useSuspenseQuery } from '@tanstack/react-query'
import { useLayoutEffect, useRef, useState } from 'react'
import { Button } from '../Button'
import { Text } from '../Text'
import { Dialog, type DialogProps } from './Dialog'
import { DialogTrigger } from './DialogTrigger'

type Story = StoryObj<DialogProps>

export default {
  title: 'Components/Dialog',
  component: Dialog,
  render: (args) => (
    <DialogTrigger defaultOpen>
      <Button>Open Dialog</Button>

      <Dialog {...args} />
    </DialogTrigger>
  ),
  args: {
    type: 'modal',
    title: 'Dialog Title',
    children: 'Dialog Content',
  },
} as Meta<DialogProps>

export const Default = {}

// Use a random query key to avoid caching
const QUERY_KEY = Math.random().toString()

function SuspenseContent({ delay = 10_000 }: { delay?: number }): React.ReactNode {
  useSuspenseQuery({
    queryKey: [QUERY_KEY],
    gcTime: 0,
    initialDataUpdatedAt: 0,
    queryFn: () =>
      new Promise((resolve) => {
        setTimeout(() => {
          resolve('resolved')
        }, delay)
      }),
  })

  return (
    <div className="flex h-[250px] flex-col items-center justify-center text-center">
      Unsuspended content
    </div>
  )
}

export const Suspened = {
  args: {
    children: <SuspenseContent delay={10_000_000_000} />,
  },
}

function BrokenContent(): React.ReactNode {
  throw new Error('💣')
}

export const Broken = {
  args: {
    children: <BrokenContent />,
  },
}

const sizes = [600, 300, 150, 450]
function ResizableContent() {
  const [sizeIndex, setSizeIndex] = useState(0)
  const divRef = useRef<HTMLDivElement>(null)

  useLayoutEffect(() => {
    const interval = setTimeout(() => {
      const nextSizeIndex = sizeIndex + 1

      if (nextSizeIndex < sizes.length) {
        setSizeIndex(nextSizeIndex)
      }
    }, 150)

    return () => {
      clearTimeout(interval)
    }
  }, [sizeIndex])

  return (
    <div
      ref={divRef}
      style={{ height: sizes[sizeIndex] }}
      className="flex flex-none items-center justify-center text-center"
    >
      This dialog should resize with animation, and the content should be centered. Height:{' '}
      {sizes[sizeIndex]}
    </div>
  )
}

export const AnimateSize: Story = {
  args: {
    children: <ResizableContent />,
  },
  parameters: {
    chromatic: { disableSnapshot: true },
  },
}

export const Fullscreen = {
  args: {
    type: 'fullscreen',
  },
}

export const FullscreenWithStretchChildren: Story = {
  args: {
    type: 'fullscreen',
    children: () => {
      return (
        <div className="flex h-full w-full flex-col items-center justify-center rounded-3xl bg-primary text-center">
          <Text color="invert" variant="h1">
            This dialog should stretch to fit the screen.
          </Text>
        </div>
      )
    },
  },
}
