import ArrowDownIcon from '#/assets/expand_arrow_down.svg'
import Plus from '#/assets/plus.svg'

import type * as aria from '#/components/aria'
import { Popover } from '#/components/Dialog'
import { Separator } from '#/components/Separator'
import { Text } from '#/components/Text'
import { StoryVariants } from '#/utilities/StoryVariants'
import type { Meta, StoryObj } from '@storybook/react'
import { expect, userEvent, within } from '@storybook/test'
import { omit } from 'enso-common/src/utilities/data/object'
import { Button, type BaseButtonProps, type ButtonProps } from '.'
import { Badge } from '../Badge'

type Story = StoryObj<BaseButtonProps<string, aria.ButtonRenderProps>>

const variants = [
  'primary',
  'accent',
  'delete',
  'delete-outline',
  'ghost-fading',
  'ghost',
  'link',
  'submit',
  'outline',
  'icon',
] as const satisfies readonly ButtonProps['variant'][]
const sizes = ['hero', 'large', 'medium', 'small', 'xsmall', 'xxsmall'] as const

export default {
  title: 'Components/Button',
  component: Button,
  argTypes: {
    variant: {
      options: variants,
      control: { type: 'radio' },
    },
    size: {
      options: sizes,
      control: { type: 'radio' },
    },
    addonStart: { control: false },
    addonEnd: { control: false },
  },
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<BaseButtonProps<string, aria.ButtonRenderProps>>

export const Variants: Story = {
  render: () => (
    <StoryVariants
      columns="6"
      render={Button}
      toLabel={(props) => omit(props, 'children')}
      variants={variants.flatMap((variant) =>
        sizes.map((size) => ({ variant, size, children: variant })),
      )}
    />
  ),
}

export const Icons: Story = {
  render: () => (
    <StoryVariants
      render={Button}
      toProps={(props) => ({ ...props, icon: 'enso_logo' })}
      toLabel={(props) => omit(props, 'children')}
      variants={[
        { children: 'Icon start' },
        { iconPosition: 'end', children: 'Icon end' },
        { 'aria-label': 'Only Icon' },
      ]}
    />
  ),
}

export const States: Story = {
  render: () => (
    <StoryVariants
      render={Button}
      toLabel={(props) => omit(props, 'children')}
      variants={[
        { isDisabled: true, children: 'Disabled' },
        { loading: true, children: 'Loading' },
        { loaderPosition: 'icon', loading: true, children: 'Loading' },
        { isActive: true, children: 'Active' },
      ]}
    />
  ),
}

export const Tooltips: Story = {
  render: () => (
    <StoryVariants
      render={Button}
      toLabel={(props) =>
        omit(
          'icon' in props ? { ...props, icon: { toString: () => 'Enso' } } : props,
          'testId',
          'children',
        )
      }
      variants={[
        { tooltip: 'This is a tooltip', children: 'Tooltip' },
        {
          icon: 'enso_logo',
          'aria-label': 'Tooltip uses aria-label for icon buttons',
          testId: 'icon-button',
        },
        { icon: 'enso_logo', tooltip: false, testId: 'icon-button-no-tooltip' },
      ]}
    />
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
