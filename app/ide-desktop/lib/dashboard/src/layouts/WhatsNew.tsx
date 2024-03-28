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
    <div className="flex flex-col gap-subheading px-home-section-x">
      <h2 className="text-subheading">{getText('discoverWhatsNew')}</h2>
      <div className="grid grid-cols-fill-news-items gap-news-items">
        <a
          className="relative col-span-1 h-news-item rounded-default bg-v3 text-tag-text col-span-2-news-item sm:col-span-2"
          rel="noreferrer"
          target="_blank"
          href="https://enso.org/"
          style={{ background: `url(${IntegrationsImage}) top -85px right -390px / 1055px` }}
        >
          <div className="absolute bottom flex w-full flex-col p-news-item-description">
            <span className="text-subheading font-bold">{getText('newsItem3Beta')}</span>
            <span className="py-news-item-subtitle-y text-sm leading-snug">
              {getText('newsItem3BetaDescription')}
            </span>
          </div>
        </a>
        <a
          className="relative h-news-item rounded-default bg-youtube text-tag-text"
          rel="noreferrer"
          target="_blank"
          href="https://www.youtube.com/c/Enso_org"
        >
          <img className="absolute left-1/2 top-6 mx-auto -translate-x-1/2" src={YoutubeIcon} />
          <div className="absolute bottom flex w-full flex-col p-news-item-description">
            <span className="text-subheading font-bold">{getText('newsItemWeeklyTutorials')}</span>
            <span className="py-news-item-subtitle-y text-sm leading-snug">
              {getText('newsItemWeeklyTutorialsDescription')}
            </span>
          </div>
        </a>
        <a
          className="relative h-news-item rounded-default bg-discord text-tag-text"
          rel="noreferrer"
          target="_blank"
          href="https://discord.gg/enso"
        >
          <img className="absolute left-1/2 top-7 mx-auto -translate-x-1/2" src={DiscordIcon} />
          <div className="absolute bottom flex w-full flex-col p-news-item-description">
            <span className="text-subheading font-bold">{getText('newsItemCommunityServer')}</span>
            <span className="py-news-item-subtitle-y text-sm leading-snug">
              {getText('newsItemCommunityServerDescription')}
            </span>
          </div>
        </a>
      </div>
    </div>
  )
}
