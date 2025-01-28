import closeIcon from '#/assets/close.svg'
import closeTabIcon from '#/assets/close_tab.svg'
import cloudIcon from '#/assets/cloud.svg'
import cloudToIcon from '#/assets/cloud_to.svg'
import type { Meta, StoryObj } from '@storybook/react'
import { Icon, type IconProps } from './Icon'

const meta = {
  title: 'Components/IconComponent',
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
        <div className="text-sm font-medium">Available Icons</div>
        <div className="flex items-center gap-4">
          <Icon>{closeIcon}</Icon>
          <Icon>{cloudIcon}</Icon>
          <Icon>{cloudToIcon}</Icon>
          <Icon>{closeTabIcon}</Icon>
        </div>
      </div>

      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Custom Icons</div>
        <div className="flex items-center gap-4">
          {sizes.map((size) => (
            <Icon key={size} size={size}>
              {CustomSvgIcon}
            </Icon>
          ))}
          {colors.map((color) => (
            <Icon key={color} color={color}>
              {CustomSvgIcon}
            </Icon>
          ))}
          <Icon renderProps={{ color: 'blue' }}>
            {(props) => <RenderPropIcon color={props.color} />}
          </Icon>
          <Icon color="accent" renderProps={{ color: 'red' }}>
            {(props) => <RenderPropIcon color={props.color} />}
          </Icon>
        </div>
      </div>

      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Sizes</div>
        <div className="flex items-center gap-4">
          {sizes.map((size) => (
            <Icon key={size} size={size}>
              {closeIcon}
            </Icon>
          ))}
        </div>
      </div>

      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Colors</div>
        <div className="flex items-center gap-4">
          <Icon color="primary">{closeIcon}</Icon>
          <Icon color="danger">{closeIcon}</Icon>
          <Icon color="success">{closeIcon}</Icon>
          <Icon color="accent">{closeIcon}</Icon>
          <Icon color="muted">{closeIcon}</Icon>
          <Icon color="disabled">{closeIcon}</Icon>
          <Icon color="invert">{closeIcon}</Icon>
          <Icon color="inherit">{closeIcon}</Icon>
          <Icon color="current">{closeIcon}</Icon>
        </div>
      </div>
    </div>
  ),
}
