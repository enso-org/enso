/** @file A component for testing the `useRefresh` hook. */
import * as React from 'react'

import * as useRefresh from '../../../../src/hooks/refreshHooks'

// ===============
// === Refresh ===
// ===============

/** The type of the state returned by {@link useRefresh.useRefresh}. */
export type RefreshState = useRefresh.RefreshState

/** Props for a {@link Refresh}. */
interface InternalRefreshProps {
  onRefresh: (refreshState: RefreshState) => void
}

/** A component for testing the `useRefresh` hook. */
export default function Refresh(props: InternalRefreshProps) {
  const { onRefresh } = props
  const [refresh, doRefresh] = useRefresh.useRefresh()
  React.useEffect(() => {
    onRefresh(refresh)
  }, [refresh, /* should never change */ onRefresh])

  return <div onClick={doRefresh}>.</div>
}
