/** @file Display multiple variants of a component, labeled by their props. */
import { Text } from '#/components/AriaComponents'
import { identity } from '#/utilities/functions'
import { stringifyJsx } from '#/utilities/jsx'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import type { JSX, ReactNode } from 'react'

const STORY_LAYOUT_STYLES = tv({
  base: 'flex flex-col gap-4',
  slots: {
    item: 'flex flex-col items-center gap-1',
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
    variants,
    // UNSAFE when `JSX.LibraryManagedAttributes` requires extra fields.
    // eslint-disable-next-line no-restricted-syntax
    toProps = identity as NonNullable<typeof props.toProps>,
    toLabel = identity,
  } = props

  const styles = STORY_LAYOUT_STYLES()

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
