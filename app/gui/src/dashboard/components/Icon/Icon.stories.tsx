import { iconNames } from '@/util/iconMetadata/iconName'
import type { Meta, StoryObj } from '@storybook/react'
import { Text } from '../AriaComponents/Text'
import { Icon, type IconProps } from './Icon'

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
const colors = [
  'primary',
  'danger',
  'success',
  'accent',
  'muted',
  'disabled',
  'invert',
  'inherit',
  'current',
] satisfies IconProps['color'][]

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

export const Icons: Story = {
  render: () => (
    <div className="flex flex-col gap-8">
      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Custom Icons</div>
        <div className="flex items-center gap-4">
          {sizes.map((size) => (
            <Icon key={size} size={size} icon={CustomSvgIcon} />
          ))}
          {colors.map((color) => (
            <Icon key={color} color={color} icon={CustomSvgIcon} />
          ))}
          <Icon
            renderProps={{ color: 'blue' }}
            icon={(props) => <RenderPropIcon color={props.color} />}
          />
          <Icon
            color="accent"
            renderProps={{ color: 'red' }}
            icon={(props) => <RenderPropIcon color={props.color} />}
          />
        </div>
      </div>

      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Sizes</div>
        <div className="flex items-center gap-4">
          {sizes.map((size) => (
            <Icon key={size} size={size} icon="close" />
          ))}
        </div>
      </div>

      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Colors</div>
        <div className="flex items-center gap-4">
          {colors.map((color) => (
            <Icon key={color} color={color} icon="close" />
          ))}
        </div>
      </div>
    </div>
  ),
}

export const AvailableIcons: Story = {
  render: () => (
    <div className="flex flex-col gap-2 pb-24">
      <Text.Heading className="mb-3">Available Icons</Text.Heading>

      <div className="grid grid-cols-9 items-center gap-4">
        {iconNames.map((icon) => (
          <div key={icon} className="flex flex-col items-center gap-2">
            <Icon key={icon} icon={icon} />
            <Text variant="caption">{icon}</Text>
          </div>
        ))}
      </div>
    </div>
  ),
}
