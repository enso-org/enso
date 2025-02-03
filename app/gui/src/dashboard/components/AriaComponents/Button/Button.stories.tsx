import Enso from '#/assets/enso_logo.svg'
import ArrowDownIcon from '#/assets/expand_arrow_down.svg'
import Plus from '#/assets/plus.svg'

import type * as aria from '#/components/aria'
import { Popover, Separator, Text } from '#/components/AriaComponents'
import type { Meta, StoryObj } from '@storybook/react'
import { expect, userEvent, within } from '@storybook/test'
import { Button, type BaseButtonProps } from '.'
import { Badge } from '../../Badge'

type Story = StoryObj<BaseButtonProps<string, aria.ButtonRenderProps>>

const variants = [
  'primary',
  'accent',
  'delete',
  'ghost-fading',
  'ghost',
  'link',
  'submit',
  'outline',
  'icon',
] as const
const sizes = ['hero', 'large', 'medium', 'small', 'xsmall', 'xxsmall'] as const

export default {
  title: 'Components/Button',
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
} satisfies Meta<BaseButtonProps<string, aria.ButtonRenderProps>>

export const Variants: Story = {
  render: () => (
    <div className="flex flex-col gap-4">
      <Text.Heading>Variants</Text.Heading>
      {variants.map((variant) => (
        <div className="grid grid-cols-4 place-items-center gap-3">
          {sizes.map((size) => (
            <Button key={variant} variant={variant} size={size}>
              {variant}
            </Button>
          ))}
        </div>
      ))}

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

export const ButtonGroup: Story = {
  render: () => (
    <div className="flex flex-col gap-4">
      <div className="flex flex-col gap-2">
        <Text.Heading>Separate</Text.Heading>
        <Button.Group>
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>
      </div>

      <div className="flex flex-col gap-2">
        <Text.Heading>Joined</Text.Heading>

        {variants.map((variant) => (
          <Button.GroupJoin key={variant} buttonVariants={{ variant }}>
            <Button>Button 1</Button>
            <Button>Button 2</Button>
            <Button icon={ArrowDownIcon} />
          </Button.GroupJoin>
        ))}

        <Button.GroupJoin buttonVariants={{ variant: 'primary' }}>
          <Button icon={Plus}>New Project</Button>

          <Popover.Trigger>
            <Button icon={ArrowDownIcon} />

            <Popover>
              <Text>Lorem ipsum dolor sit amet consectetur adipisicing elit. Quisquam, quos.</Text>
            </Popover>
          </Popover.Trigger>
        </Button.GroupJoin>
      </div>

      {/* Column */}
      <div className="flex flex-col gap-2">
        <Text.Heading>Column</Text.Heading>
        <Button.Group direction="column">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" align="center">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" align="end">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" align="between">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" align="around">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" align="evenly">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" align="start">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" verticalAlign="end">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" verticalAlign="center">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" verticalAlign="start">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="column" verticalAlign="end">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />
      </div>
      {/* End Column */}

      {/* Row */}
      <div className="flex flex-col gap-4">
        <Text.Heading>Row</Text.Heading>
        <Button.Group direction="row">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" align="center">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" align="end">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" align="between">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" align="around">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" align="evenly">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" align="start">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" verticalAlign="end">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" verticalAlign="center">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>

        <Separator />

        <Button.Group direction="row" verticalAlign="start">
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>
      </div>
      {/* End Row */}

      <div className="flex flex-col gap-2">
        <Text.Heading>Button Styles</Text.Heading>
        <Button.Group buttonVariants={{ isDisabled: true, variant: 'outline' }}>
          <Button>Button 1</Button>
          <Button>Button 2</Button>
          <Button>Button 3</Button>
        </Button.Group>
      </div>
    </div>
  ),
}
