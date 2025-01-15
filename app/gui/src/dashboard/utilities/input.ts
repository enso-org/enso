/** @file Functions related to inputs. */

/**
 * Trigger a file input.
 */
export function inputFiles() {
  return new Promise<FileList>((resolve, reject) => {
    const input = document.createElement('input')
    input.type = 'file'
    input.style.display = 'none'
    document.body.appendChild(input)
    input.addEventListener('input', () => {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      resolve(input.files!)
    })
    input.addEventListener('cancel', () => {
      reject(new Error('File selection was cancelled.'))
    })
    input.click()
    input.remove()
  })
}
