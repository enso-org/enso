import type { Meta, StoryObj } from '@storybook/react'
import { expect, userEvent, waitFor, within } from '@storybook/test'
import { Button } from '../Button'
import { DialogTrigger } from './DialogTrigger'
import { Popover, type PopoverProps } from './Popover'

type Story = StoryObj<PopoverProps>

export default {
  title: 'Components/Popover',
  component: Popover,
  args: {
    children: 'Popover content',
  },
  render: (props: PopoverProps) => (
    <DialogTrigger>
      <Button>Open Dialog</Button>
      <Popover {...props} />
    </DialogTrigger>
  ),
} satisfies Meta<PopoverProps>

export const Default: Story = {
  args: {
    isOpen: true,
  },
}

export const Dismissible: Story = {
  play: async ({ canvasElement }) => {
    const { getByRole, queryByRole } = within(canvasElement)
    await userEvent.click(getByRole('button'))

    await expect(getByRole('dialog')).toBeInTheDocument()

    await userEvent.click(document.body)

    await waitFor(() => expect(queryByRole('dialog')).not.toBeInTheDocument())
  },
}

export const NonDidmissible: Story = {
  args: {
    isDismissable: false,
  },
  play: async ({ canvasElement }) => {
    const { getByRole } = within(canvasElement)
    await userEvent.click(getByRole('button'))

    await expect(getByRole('dialog')).toBeInTheDocument()

    await userEvent.click(document.body)

    await expect(getByRole('dialog')).toBeInTheDocument()
  },
}
