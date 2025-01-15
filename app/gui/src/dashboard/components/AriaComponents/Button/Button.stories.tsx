import Enso from '#/assets/enso_logo.svg'
import type * as aria from '#/components/aria'
import { Text } from '#/components/AriaComponents'
import type { Meta, StoryObj } from '@storybook/react'
import { expect, userEvent, within } from '@storybook/test'
import { Badge } from '../../Badge'
import type { BaseButtonProps } from './Button'
import { Button } from './Button'

type Story = StoryObj<BaseButtonProps<aria.ButtonRenderProps>>

const variants = [
  'primary',
  'accent',
  'delete',
  'ghost-fading',
  'ghost',
  'link',
  'submit',
  'outline',
] as const
const sizes = ['hero', 'large', 'medium', 'small', 'xsmall', 'xxsmall'] as const

export default {
  title: 'Components/AriaComponents/Button',
  component: Button,
  render: (props) => <Button {...props} />,
  argTypes: {
    variant: {
      control: 'radio',
      options: variants,
    },
    size: {
      control: 'radio',
      options: sizes,
    },
    addonStart: { control: false },
    addonEnd: { control: false },
  },
} as Meta<BaseButtonProps<aria.ButtonRenderProps>>

export const Variants: Story = {
  render: () => (
    <div className="flex flex-col gap-4">
      <Text.Heading>Variants</Text.Heading>
      <div className="grid grid-cols-4 place-content-start place-items-start gap-3">
        {variants.map((variant) => (
          <Button key={variant} variant={variant}>
            {variant}
          </Button>
        ))}
      </div>

      <Text.Heading>Sizes</Text.Heading>
      <div className="grid grid-cols-4 place-content-center place-items-start gap-3">
        {sizes.map((size) => (
          <Button key={size} size={size}>
            {size}
          </Button>
        ))}
      </div>

      <Text.Heading>Icons</Text.Heading>
      <div className="grid grid-cols-4 place-content-center place-items-start gap-3">
        <Button icon={Enso}>Icon start</Button>
        <Button icon={Enso} iconPosition="end">
          Icon end
        </Button>
        <Button icon={Enso} aria-label="Only icon" />
      </div>

      <Text.Heading>States</Text.Heading>
      <div className="grid grid-cols-4 place-content-center place-items-start gap-3">
        <Button isDisabled>Disabled</Button>
        <Button loading>Loading</Button>
        <Button loaderPosition="icon" loading>
          Loading
        </Button>
        <Button isActive>Active</Button>
      </div>
    </div>
  ),
}

export const Tooltips: Story = {
  render: () => (
    <div className="flex flex-col gap-4">
      <Text.Heading>Tooltip</Text.Heading>
      <div className="grid grid-cols-4 place-content-center place-items-start gap-3">
        <Button tooltip="This is a tooltip">Tooltip</Button>
        <Button
          aria-label="Tooltip uses aria-label for icon buttons"
          icon={Enso}
          testId="icon-button"
        />
        <Button icon={Enso} tooltip={false} testId="icon-button-no-tooltip" />
      </div>
    </div>
  ),
}

export const LoadingOnPress: Story = {
  render: () => {
    return (
      <Button
        onPress={() => {
          return new Promise((resolve) => setTimeout(resolve, 1000))
        }}
      >
        Click me to trigger loading
      </Button>
    )
  },
  play: async ({ canvasElement }) => {
    const { getByRole, findByTestId } = within(canvasElement)

    const button = getByRole('button', { name: 'Click me to trigger loading' })
    await userEvent.click(button)
    await expect(button).toHaveAttribute('disabled')
    // then the spinner appears after some delay
    await expect(await findByTestId('spinner')).toBeInTheDocument()
  },
}

export const Addons: Story = {
  args: {
    addonStart: (
      <Badge color="error" variant="solid">
        Test
      </Badge>
    ),
    addonEnd: (
      <Badge color="error" variant="solid">
        Test
      </Badge>
    ),
  },
  render: (args) => (
    <>
      <div className="mb-8 grid grid-cols-[repeat(4,minmax(0,min-content))] items-center justify-items-center gap-4">
        {sizes.map((size) => (
          <Button key={size} size={size} {...args}>
            {size}
          </Button>
        ))}

        {variants.map((variant) => (
          <Button key={variant} variant={variant} {...args}>
            {variant}
          </Button>
        ))}
      </div>

      <div className="grid grid-cols-[repeat(4,minmax(0,min-content))] items-center justify-items-center gap-4">
        {sizes.map((size) => (
          <Button key={size} size={size} {...args}>
            {size}
          </Button>
        ))}

        {variants.map((variant) => (
          <Button key={variant} variant={variant} {...args}>
            {variant}
          </Button>
        ))}
      </div>
    </>
  ),
}
