import { useNavigator } from '@/util/navigator'
import { createProvidable } from '.'

export type GraphSelection = ReturnType<typeof useNavigator>
const { provideFn, useFn } = createProvidable(useNavigator)
export { provideFn as provideGraphNavigator, useFn as useGraphNavigator }
