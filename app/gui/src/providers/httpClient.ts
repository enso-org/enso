import HttpClient from '#/utilities/HttpClient'
import { createContextStore } from '@/providers'

function generateSessionID() {
  const sessionID = sessionStorage.getItem('sessionID')
  if (sessionID) {
    return sessionID
  }

  const newSessionID = crypto.randomUUID()
  sessionStorage.setItem('sessionID', newSessionID)
  return newSessionID
}

export const [provideHttpClient, useHttpClient] = createContextStore('http-client', () => {
  const sessionID = generateSessionID()
  const httpClient = new HttpClient({
    'x-enso-ide-version': $config.VERSION ?? '',
    'x-enso-session-id': sessionID,
  })
  return httpClient
})
