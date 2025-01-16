import { Suspense } from '#/components/Suspense'
import { DirectoryId } from '#/services/Backend'
import { act, renderHook, waitFor, type RenderHookOptions, type RenderHookResult } from '#/test'
import { useStore } from '#/utilities/zustand'
import { useState } from 'react'
import invariant from 'tiny-invariant'
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
      ...options,
      wrapper: ({ children }) => {
        // eslint-disable-next-line react-hooks/rules-of-hooks
        const [category, setCategory] = useState(() => currentCategoryId)
        currentCategoryId = category
        setCategoryId = (nextCategoryId) => {
          setCategory(nextCategoryId)
          doResetAssetTableState()
        }

        return (
          <Suspense>
            <DriveProvider
              launchedProjects={[]}
              cloudCategories={[]}
              localCategories={[]}
              // UNSAFE, but fine in this case as this value is not accessed as
              // the categories lists above are empty.
              // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
              remoteBackend={null!}
              localBackend={null}
            >
              {({ resetAssetTableState }) => {
                doResetAssetTableState = resetAssetTableState
                return children
              }}
            </DriveProvider>
          </Suspense>
        )
      },
    },
  )
}

describe('<DriveProvider />', async () => {
  const driveAPI = renderDriveProviderHook((setCategoryId: (categoryId: CategoryId) => void) => {
    const store = useDriveStore()
    return useStore(
      store,
      ({ toggleDirectoryExpansion, expandedDirectories, selectedKeys, visuallySelectedKeys }) => ({
        expandedDirectories,
        toggleDirectoryExpansion,
        setCategoryId,
        selectedKeys,
        visuallySelectedKeys,
      }),
    )
  })

  await waitFor(() => {
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    invariant(driveAPI.result.current != null)
  })

  it('Should reset expanded directory ids when category changes', () => {
    act(() => {
      driveAPI.result.current.toggleDirectoryExpansion(
        [DirectoryId('directory-test-123')],
        true,
        'cloud',
      )
    })

    expect(driveAPI.result.current.expandedDirectories.cloud).toEqual(
      new Set(['directory-test-123']),
    )

    act(() => {
      driveAPI.result.current.setCategoryId('local')
    })

    expect(driveAPI.result.current.expandedDirectories.local).toEqual(new Set())
    expect(driveAPI.result.current.selectedKeys).toEqual(new Set())
    expect(driveAPI.result.current.visuallySelectedKeys).toEqual(null)

    act(() => {
      driveAPI.result.current.toggleDirectoryExpansion(
        [DirectoryId('directory-test-124')],
        true,
        'local',
      )
    })
    expect(driveAPI.result.current.expandedDirectories.local).toEqual(
      new Set(['directory-test-124']),
    )

    act(() => {
      // Set the category back to the default category (`cloud`).
      driveAPI.result.current.setCategoryId('cloud')
    })
    // The original expanded directories should be retained.
    expect(driveAPI.result.current.expandedDirectories.cloud).toEqual(
      new Set(['directory-test-123']),
    )
    act(() => {
      driveAPI.result.current.toggleDirectoryExpansion(
        // Allow removing extra directories
        [DirectoryId('directory-test-123'), DirectoryId('directory-test-124')],
        false,
        'cloud',
      )
    })

    act(() => {
      driveAPI.result.current.setCategoryId('local')
    })
    expect(driveAPI.result.current.expandedDirectories.local).toEqual(
      new Set(['directory-test-124']),
    )

    act(() => {
      driveAPI.result.current.setCategoryId('cloud')
    })
    expect(driveAPI.result.current.expandedDirectories.cloud).toEqual(new Set())
  })
})
