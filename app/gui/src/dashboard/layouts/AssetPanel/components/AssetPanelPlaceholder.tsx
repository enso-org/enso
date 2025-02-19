/** @file A {@link Result} for an `AssetPanel`. */
import { Result } from '#/components/Result'

/** Props for a {@link AssetPanelPlaceholder}. */
export interface AssetPanelPlaceholderProps {
  readonly title: string
}

/** A {@link Result} for an `AssetPanel`. */
export function AssetPanelPlaceholder(props: AssetPanelPlaceholderProps) {
  const { title } = props

  return <Result status="info" centered title={title} />
}
