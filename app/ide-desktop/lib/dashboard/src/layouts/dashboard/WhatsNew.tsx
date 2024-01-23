/** @file Community updates for the app. */
import * as React from 'react'

import DiscordIcon from 'enso-assets/discord.svg'
import IntegrationsImage from 'enso-assets/integrations.png'
import YoutubeIcon from 'enso-assets/youtube.svg'

import * as textProvider from '#/providers/TextProvider'

// ================
// === WhatsNew ===
// ================

/** Community updates for the app. */
export default function WhatsNew() {
  const { getText } = textProvider.useText()

  return (
    <div className="flex flex-col gap-4 px-4.75">
      <h2 className="text-xl leading-144.5 py-0.5">{getText('discoverWhatsNew')}</h2>
      <div className="grid gap-3 grid-cols-fill-75">
        <a
          className="relative whatsnew-span-2 col-span-1 sm:col-span-2 bg-v3 text-tag-text rounded-2xl h-45"
          rel="noreferrer"
          target="_blank"
          href="https://enso.org/"
          style={{
            background: `url(${IntegrationsImage}) top -85px right -390px / 1055px`,
          }}
        >
          <div className="absolute flex flex-col bottom-0 p-4">
            <span className="text-xl font-bold leading-144.5 py-0.5">
              {getText('newsItem3Beta')}
            </span>
            <span className="text-sm leading-144.5 py-0.5">
              {getText('newsItem3BetaDescription')}
            </span>
          </div>
        </a>
        <a
          className="relative bg-youtube text-tag-text rounded-2xl h-45"
          rel="noreferrer"
          target="_blank"
          href="https://www.youtube.com/c/Enso_org"
        >
          <img className="absolute top-6 left-1/2 -translate-x-1/2 mx-auto" src={YoutubeIcon} />
          <div className="absolute flex flex-col bottom-0 p-4">
            <span className="text-xl font-bold leading-144.5 py-0.5">
              {getText('newsItemWeeklyTutorials')}
            </span>
            <span className="text-sm leading-144.5 py-0.5">
              {getText('newsItemWeeklyTutorialsDescription')}
            </span>
          </div>
        </a>
        <a
          className="relative bg-discord text-tag-text rounded-2xl h-45"
          rel="noreferrer"
          target="_blank"
          href="https://discord.gg/enso"
        >
          <img className="absolute top-7 left-1/2 -translate-x-1/2 mx-auto" src={DiscordIcon} />
          <div className="absolute flex flex-col bottom-0 p-4">
            <span className="text-xl font-bold leading-144.5 py-0.5">
              {getText('newsItemCommunityServer')}
            </span>
            <span className="text-sm leading-144.5 py-0.5">
              {getText('newsItemCommunityServerDescription')}
            </span>
          </div>
        </a>
      </div>
    </div>
  )
}
