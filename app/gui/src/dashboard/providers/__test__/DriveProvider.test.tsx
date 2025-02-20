import { DirectoryId } from '#/services/Backend'
import { act, renderHook, type RenderHookOptions, type RenderHookResult } from '#/test'
import { useStore } from '#/utilities/zustand'
import { useState } from 'react'
import { describe, expect, it } from 'vitest'
import type { CategoryId } from '../../layouts/CategorySwitcher/Category'
import DriveProvider, { useDriveStore } from '../DriveProvider'

function renderDriveProviderHook<Result, Props>(
  hook: (props: Props) => Result,
  options?: Omit<RenderHookOptions<Props>, 'wrapper'>,
): RenderHookResult<Result, Props> {
  let currentCategoryId: CategoryId = 'cloud'
  let setCategoryId: (categoryId: CategoryId) => void
  let doResetAssetTableState: () => void

  return renderHook(
    (props) => {
      const result = hook(props)
      return { ...result, setCategoryId }
    },
    {
      wrapper: ({ children }) => {
        // eslint-disable-next-line react-hooks/rules-of-hooks
        const [category, setCategory] = useState(() => currentCategoryId)
        currentCategoryId = category
        setCategoryId = (nextCategoryId) => {
          setCategory(nextCategoryId)
          doResetAssetTableState()
        }

        return (
          <DriveProvider>
            {({ resetAssetTableState }) => {
              doResetAssetTableState = resetAssetTableState
              return children
            }}
          </DriveProvider>
        )
      },
      ...options,
    },
  )
}

describe('<DriveProvider />', () => {
  it('Should reset expanded directory ids when category changes', () => {
    const driveAPI = renderDriveProviderHook((setCategoryId: (categoryId: string) => void) => {
      const store = useDriveStore()
      return useStore(
        store,
        ({
          setExpandedDirectoryIds,
          expandedDirectoryIds,
          selectedKeys,
          visuallySelectedKeys,
        }) => ({
          expandedDirectoryIds,
          setExpandedDirectoryIds,
          setCategoryId,
          selectedKeys,
          visuallySelectedKeys,
        }),
      )
    })

    act(() => {
      driveAPI.result.current.setExpandedDirectoryIds([DirectoryId('directory-test-123')])
    })

    expect(driveAPI.result.current.expandedDirectoryIds).toEqual([
      DirectoryId('directory-test-123'),
    ])

    act(() => {
      driveAPI.result.current.setCategoryId('recent')
    })

    expect(driveAPI.result.current.expandedDirectoryIds).toEqual([])
    expect(driveAPI.result.current.selectedKeys).toEqual(new Set())
    expect(driveAPI.result.current.visuallySelectedKeys).toEqual(null)
  })
})
