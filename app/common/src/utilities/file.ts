/** @file Functions related to files. */

export type FileExtension = `.${string}`
export type MimeType = `${string}/${string}`

export interface InputFilesOptions {
  accept?: (FileExtension | MimeType)[]
}

/**
 * Open a file-selection dialog and read the file selected by the user.
 */
export function readUserSelectedFile(options: InputFilesOptions = {}) {
  return new Promise<FileList>((resolve, reject) => {
    const input = document.createElement('input')
    input.type = 'file'
    input.style.display = 'none'
    if (options.accept) input.accept = options.accept.join(',')
    document.body.appendChild(input)
    input.addEventListener('input', () => {
      resolve(input.files!)
    })
    input.addEventListener('cancel', () => {
      reject(new Error('File selection was cancelled.'))
    })
    input.click()
    input.remove()
  })
}
