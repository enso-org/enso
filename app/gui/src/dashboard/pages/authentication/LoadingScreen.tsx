/** @file A loading screen, displayed while the user is logging in. */
import { Text } from '#/components/AriaComponents'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import { useText } from '#/providers/TextProvider'

// =================
// === Constants ===
// =================

/** The diameter of the spinner. */
const SPINNER_SIZE_PX = 64

// =====================
// === LoadingScreen ===
// =====================

/** A loading screen. */
export default function LoadingScreen() {
  const { getText } = useText()

  return (
    <div
      className="grid h-screen w-screen place-items-center text-primary"
      data-testid="loading-screen"
    >
      <div className="flex flex-col items-center gap-8 text-center">
        <StatelessSpinner state="loading-fast" size={SPINNER_SIZE_PX} />

        <Text.Heading variant="h1" color="inherit">
          {getText('loadingAppMessage')}
        </Text.Heading>
      </div>
    </div>
  )
}
