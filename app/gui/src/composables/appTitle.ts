import type { UserSession } from '$/providers/auth'
import { useText } from '$/providers/text'
import type { ToValue } from '@/util/reactivity'
import { Plan } from 'enso-common/src/services/Backend'
import type { Opt } from 'enso-common/src/utilities/data/opt'
import { computed, toValue, watch } from 'vue'

/**
 * Composable for managing the app title based on user authentication and plan.
 *
 * Appends "Not for commercial use" to the document title when:
 * - User is logged in (session exists)
 * - User is on the Free plan
 * @param userSession - Reactive reference to the user session data
 */
export function useAppTitle(userSession: ToValue<Opt<UserSession>>) {
  const originalTitle = document.title
  const text = useText()
  const commercialUseNotice = text.getText('commercialUseNotice', originalTitle)

  const shouldShowCommercialNotice = computed(() => {
    const session = toValue(userSession)
    return session?.user?.plan === Plan.free
  })

  watch(shouldShowCommercialNotice, (shouldShow) => {
    if (shouldShow) {
      if (!document.title.includes(commercialUseNotice)) {
        document.title = commercialUseNotice
      }
    } else {
      document.title = originalTitle
    }
  })

  return {
    shouldShowCommercialNotice,
  }
}
