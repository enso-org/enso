/** @file Display multiple variants of a component, labeled by their props. */
import { Text } from '#/components/Text'
import { identity } from '#/utilities/functions'
import { stringifyJsx } from '#/utilities/jsx'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import type { JSX, ReactNode } from 'react'

const STORY_LAYOUT_STYLES = tv({
  base: 'grid gap-4 place-items-center text-primary',
  slots: {
    item: 'flex flex-col items-center gap-1',
  },
  variants: {
    columns: {
      /* eslint-disable @typescript-eslint/naming-convention */
      '1': 'grid-cols-1',
      '2': 'grid-cols-2',
      '3': 'grid-cols-3',
      '4': 'grid-cols-4',
      '5': 'grid-cols-5',
      '6': 'grid-cols-6',
      '7': 'grid-cols-7',
      '8': 'grid-cols-8',
      '9': 'grid-cols-9',
      /* eslint-enable @typescript-eslint/naming-convention */
    },
  },
  defaultVariants: {
    columns: '1',
  },
})

/** Props for a {@link StoryVariants}. */
export interface StoryVariantsProps<
    Component extends (props: Props) => ReactNode,
    Props = Parameters<Component>[0],
    T = Props,
  >
  // `variants` is intentionally omitted
  extends Omit<VariantProps<typeof STORY_LAYOUT_STYLES>, 'variants'> {
  readonly render: Component
  readonly variants: readonly T[]
  readonly toProps?: (
    variant: T,
  ) => JSX.IntrinsicAttributes & JSX.LibraryManagedAttributes<Component, NoInfer<Props>>
  readonly toLabel?: (variant: T) => unknown
}

/** Display multiple variants of a component, labeled by their props. */
export function StoryVariants<
  const Component extends (props: Props) => ReactNode,
  Props = Parameters<Component>[0],
  const T = Props,
>(props: StoryVariantsProps<Component, Props, T>) {
  const {
    render: Render,
    columns,
    variants,
    // UNSAFE when `JSX.LibraryManagedAttributes` requires extra fields.
    // eslint-disable-next-line no-restricted-syntax
    toProps = identity as NonNullable<typeof props.toProps>,
    toLabel = identity,
  } = props

  const styles = STORY_LAYOUT_STYLES({ columns })

  return (
    <section className={styles.base()}>
      {variants.map((variant, i) => (
        <div key={i} className={styles.item()}>
          <Render {...toProps(variant)} />
          <Text variant="caption">{stringifyJsx(toLabel(variant))}</Text>
        </div>
      ))}
    </section>
  )
}
