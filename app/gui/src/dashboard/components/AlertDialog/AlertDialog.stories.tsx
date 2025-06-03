import type { Meta, StoryObj } from '@storybook/react'
import { expect, fn, userEvent, within } from '@storybook/test'
import { Alert } from '../Alert'
import { Button } from '../Button'
import { Text } from '../Text'
import type { AlertDialogProps } from './AlertDialog'
import { AlertDialog } from './AlertDialog'

const onConfirm = fn() as AlertDialogProps['onConfirm']
const onCancel = fn() as AlertDialogProps['onCancel']

const meta: Meta<AlertDialogProps> = {
  title: 'Components/AlertDialog',
  component: AlertDialog,
  parameters: {
    layout: 'centered',
  },
  args: {
    title: 'This is an alert dialog',
    children: 'Are you sure you want to proceed?',
    onConfirm,
    onCancel,
  },
  render: (args) => (
    <AlertDialog.Trigger>
      <Button>Open</Button>

      <AlertDialog {...args} />
    </AlertDialog.Trigger>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)

    await userEvent.click(canvas.getByRole('button', { name: 'Open' }))

    await expect(canvas.getByRole('alertdialog')).toBeInTheDocument()
  },
} satisfies Meta<AlertDialogProps>

export default meta

type Story = StoryObj<AlertDialogProps>

export const Default: Story = {
  args: {},
}

export const Destructive: Story = {
  args: {
    isDestructive: true,
    children: 'Are you sure you want to delete this item?',
  },
}

export const CutAndPaste: Story = {
  args: {
    children: (
      <>
        <Text>Are you sure you want to move this item?</Text>

        <Alert variant="outline" icon="copy2">
          <Text>You can copy instead</Text>
        </Alert>
      </>
    ),
  },
}

export const Cancel: Story = {
  args: {
    onCancel,
  },
  play: async (args) => {
    await meta.play?.(args)

    const canvas = within(args.canvasElement)

    await userEvent.click(canvas.getByRole('button', { name: 'Cancel' }))

    await expect(onCancel).toHaveBeenCalledOnce()
    await expect(onConfirm).not.toHaveBeenCalled()
  },
}

export const Confirm: Story = {
  args: {
    onConfirm,
  },
  play: async (args) => {
    await meta.play?.(args)

    const canvas = within(args.canvasElement)

    await userEvent.click(canvas.getByRole('button', { name: 'Confirm' }))

    await expect(onConfirm).toHaveBeenCalledOnce()
    await expect(onCancel).not.toHaveBeenCalled()
  },
}
