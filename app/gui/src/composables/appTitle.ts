import { Plan } from '#/services/Backend'
import type { UserSession } from '$/providers/auth'
import { useText } from '$/providers/text'
import { ToValue } from '@/util/reactivity'
import { computed, toValue, watch } from 'vue'
import { Opt } from 'ydoc-shared/util/data/opt'

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
