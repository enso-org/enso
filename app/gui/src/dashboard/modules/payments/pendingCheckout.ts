/** @file A module for managing the pending checkout target plan in local storage. */
import LocalStorage from '#/utilities/LocalStorage'
import { Plan } from 'enso-common/src/services/Backend'
import { z } from 'zod'

declare module '#/utilities/LocalStorage' {
  /**
   * Stores the user's intended subscription plan during checkout process so that the app can
   * track the user's intended subscription plan while navigating between pages.
   */
  interface LocalStorageData {
    readonly pendingCheckoutTargetPlan: Plan
  }
}

LocalStorage.registerKey('pendingCheckoutTargetPlan', {
  isUserSpecific: true,
  schema: z.nativeEnum(Plan),
})

/** Returns the stored pending checkout target plan. */
export function getPendingCheckoutTargetPlan() {
  return LocalStorage.getInstance().get('pendingCheckoutTargetPlan')
}

/** Sets the pending checkout target plan. */
export function setPendingCheckoutTargetPlan(plan: Plan) {
  LocalStorage.getInstance().set('pendingCheckoutTargetPlan', plan)
}

/** Clears the pending checkout target plan. */
export function clearPendingCheckoutTargetPlan() {
  LocalStorage.getInstance().delete('pendingCheckoutTargetPlan')
}
