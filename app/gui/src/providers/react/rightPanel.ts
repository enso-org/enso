import { RightPanelData, useRightPanelData as useRightPanelDataVue } from '$/providers/rightPanel'
import { createCrossingProviderForPureReactInVue } from 'veaury'
import { toRefs } from 'vue'

const [useRightPanelDataUntyped, RightPanelDataProviderForReact] =
  createCrossingProviderForPureReactInVue(() => toRefs(useRightPanelDataVue()))

export { RightPanelDataProviderForReact }
export const useRightPanelData = useRightPanelDataUntyped as () => RightPanelData
