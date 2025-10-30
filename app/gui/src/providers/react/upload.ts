import { useInReactFunction } from '$/providers/react/common'
import type { UploadsToCloudStore } from '$/providers/upload'
import * as react from 'react'

export const UploadsToCloudStoreContext = react.createContext<UploadsToCloudStore | null>(null)
export const useUploadsToCloudStore = useInReactFunction(UploadsToCloudStoreContext)
