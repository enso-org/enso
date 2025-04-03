import type { Meta, StoryObj } from '@storybook/react'
import { Text } from '../AriaComponents/Text'
import { ProfilePicture, type ProfilePictureProps } from './ProfilePicture'

const meta = {
  title: 'Components/ProfilePicture',
  component: ProfilePicture,
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof ProfilePicture>

const PFP = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAApgAAAKYB3X3/OAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAANCSURBVEiJtZZPbBtFFMZ/M7ubXdtdb1xSFyeilBapySVU8h8OoFaooFSqiihIVIpQBKci6KEg9Q6H9kovIHoCIVQJJCKE1ENFjnAgcaSGC6rEnxBwA04Tx43t2FnvDAfjkNibxgHxnWb2e/u992bee7tCa00YFsffekFY+nUzFtjW0LrvjRXrCDIAaPLlW0nHL0SsZtVoaF98mLrx3pdhOqLtYPHChahZcYYO7KvPFxvRl5XPp1sN3adWiD1ZAqD6XYK1b/dvE5IWryTt2udLFedwc1+9kLp+vbbpoDh+6TklxBeAi9TL0taeWpdmZzQDry0AcO+jQ12RyohqqoYoo8RDwJrU+qXkjWtfi8Xxt58BdQuwQs9qC/afLwCw8tnQbqYAPsgxE1S6F3EAIXux2oQFKm0ihMsOF71dHYx+f3NND68ghCu1YIoePPQN1pGRABkJ6Bus96CutRZMydTl+TvuiRW1m3n0eDl0vRPcEysqdXn+jsQPsrHMquGeXEaY4Yk4wxWcY5V/9scqOMOVUFthatyTy8QyqwZ+kDURKoMWxNKr2EeqVKcTNOajqKoBgOE28U4tdQl5p5bwCw7BWquaZSzAPlwjlithJtp3pTImSqQRrb2Z8PHGigD4RZuNX6JYj6wj7O4TFLbCO/Mn/m8R+h6rYSUb3ekokRY6f/YukArN979jcW+V/S8g0eT/N3VN3kTqWbQ428m9/8k0P/1aIhF36PccEl6EhOcAUCrXKZXXWS3XKd2vc/TRBG9O5ELC17MmWubD2nKhUKZa26Ba2+D3P+4/MNCFwg59oWVeYhkzgN/JDR8deKBoD7Y+ljEjGZ0sosXVTvbc6RHirr2reNy1OXd6pJsQ+gqjk8VWFYmHrwBzW/n+uMPFiRwHB2I7ih8ciHFxIkd/3Omk5tCDV1t+2nNu5sxxpDFNx+huNhVT3/zMDz8usXC3ddaHBj1GHj/As08fwTS7Kt1HBTmyN29vdwAw+/wbwLVOJ3uAD1wi/dUH7Qei66PfyuRj4Ik9is+hglfbkbfR3cnZm7chlUWLdwmprtCohX4HUtlOcQjLYCu+fzGJH2QRKvP3UNz8bWk1qMxjGTOMThZ3kvgLI5AzFfo379UAAAAASUVORK5CYII=`

const sizes = ['small', 'medium', 'large'] satisfies ProfilePictureProps['size'][]
const rounded = ['full', 'none'] satisfies ProfilePictureProps['rounded'][]

export default meta
type Story = StoryObj<typeof meta>

export const Icons: Story = {
  render: () => (
    <div className="flex flex-col gap-8">
      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Sizes</div>
        <div className="flex items-center gap-4">
          {sizes.map((size) => (
            <ProfilePicture key={size} size={size} picture={null} name="Default User" />
          ))}
          {sizes.map((size) => (
            <ProfilePicture key={size} size={size} picture={PFP} name="Custom User" />
          ))}
        </div>
      </div>

      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Rounded</div>
        <div className="flex items-center gap-4">
          {rounded.map((r) => (
            <ProfilePicture key={r} rounded={r} size="medium" picture={null} name="Default User" />
          ))}
          {rounded.map((r) => (
            <ProfilePicture key={r} rounded={r} size="medium" picture={PFP} name="Custom User" />
          ))}
        </div>
      </div>

      <div className="flex flex-col gap-2">
        <div className="text-sm font-medium">Within flex container</div>
        <div className="flex items-center gap-4">
          {sizes.map((size) => (
            <div key={size} className="flex w-32 items-center justify-center gap-2">
              <ProfilePicture size={size} rounded="full" picture={PFP} name="Custom User" />
              <Text truncate>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</Text>
            </div>
          ))}
        </div>
      </div>
    </div>
  ),
}
