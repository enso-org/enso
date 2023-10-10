/** @file Temporary file to print base64 of a png file. */
import * as fs from 'node:fs/promises'

const BUF = await fs.readFile('./test-results/driveView-drive-view/drive-view-2-actual.png')
console.log(BUF.toString('base64'))
const BUF2 = await fs.readFile('./test-results/driveView-drive-view/drive-view-2-diff.png')
console.log(BUF2.toString('base64'))
const BUF3 = await fs.readFile(
    './test-results/changePasswordModal-change-password-modal/change-password-modal-2-actual.png'
)
console.log(BUF3.toString('base64'))
const BUF4 = await fs.readFile(
    './test-results/changePasswordModal-change-password-modal/change-password-modal-2-diff.png'
)
console.log(BUF4.toString('base64'))
const BUF5 = await fs.readFile(
    './test-results/loginLogout-login-and-logout/login-and-logout-2-actual.png'
)
console.log(BUF5.toString('base64'))
const BUF6 = await fs.readFile(
    './test-results/loginLogout-login-and-logout/login-and-logout-2-diff.png'
)
console.log(BUF6.toString('base64'))
