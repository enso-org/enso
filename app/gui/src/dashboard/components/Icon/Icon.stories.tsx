import { StatusBadge } from '#/components/AriaComponents'
import { StoryVariants } from '#/utilities/StoryVariants'
import { iconNames } from '@/util/iconMetadata/iconName'
import type { Meta, StoryObj } from '@storybook/react'
import { ICON_COLORS, Icon, type IconProps } from './Icon'

const meta = {
  title: 'Components/Icon',
  component: Icon,
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof Icon>

const sizes = [
  'xsmall',
  'small',
  'medium',
  'large',
  'xlarge',
  'xxlarge',
] satisfies IconProps['size'][]

export default meta
type Story = StoryObj<typeof meta>

const CustomSvgIcon = () => (
  <svg viewBox="0 0 16 16" fill="none">
    <circle cx="8" cy="8" r="7" stroke="currentColor" strokeWidth="2" />
    <path
      d="M5 8L7 10L11 6"
      stroke="currentColor"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  </svg>
)

interface RenderProps {
  color: string | undefined
}

const RenderPropIcon = ({ color = '#000' }: RenderProps) => (
  <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
    <circle cx="8" cy="8" r="7" stroke={color} strokeWidth="2" />
    <path d="M8 4V12M4 8H12" stroke={color} strokeWidth="2" strokeLinecap="round" />
  </svg>
)

export const Default = {
  args: {},
  argTypes: {
    icon: { options: iconNames, control: { type: 'select' } },
    size: { options: sizes, control: { type: 'select' } },
    color: { options: ICON_COLORS, control: { type: 'select' } },
  },
}

export const Custom: Story = {
  render: () => (
    <StoryVariants
      render={Icon}
      variants={[
        ...sizes.map((size) => ({ size, icon: CustomSvgIcon })),
        ...ICON_COLORS.map((color) => ({ color, icon: CustomSvgIcon })),
        {
          renderProps: { color: 'blue' },
          icon: (props: { color: string }) => <RenderPropIcon color={props.color} />,
        },
        {
          color: 'accent',
          renderProps: { color: 'red' },
          icon: (props: { color: string }) => <RenderPropIcon color={props.color} />,
        },
      ]}
    />
  ),
}

export const Sizes: Story = {
  render: () => (
    <StoryVariants
      render={Icon}
      toProps={(size) => ({ icon: 'close' as const, size })}
      variants={sizes}
    />
  ),
}

export const Colors: Story = {
  render: () => (
    <StoryVariants
      render={Icon}
      toProps={(color) => ({ icon: 'close' as const, color })}
      variants={ICON_COLORS}
    />
  ),
}

export const AvailableIcons: Story = {
  render: () => (
    <StoryVariants columns="9" render={Icon} toProps={(icon) => ({ icon })} variants={iconNames} />
  ),
}

export const AvailableIconsWithStatusBadge: Story = {
  render: () => (
    <StoryVariants
      columns="9"
      render={(props: IconProps) => (
        <StatusBadge color="danger">
          <Icon {...props} />
        </StatusBadge>
      )}
      toProps={(icon) => ({ icon })}
      variants={iconNames}
    />
  ),
}
