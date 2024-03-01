/** @file Community updates for the app. */
import * as React from 'react'

import DiscordIcon from 'enso-assets/discord.svg'
import IntegrationsImage from 'enso-assets/integrations.png'
import YoutubeIcon from 'enso-assets/youtube.svg'

// ================
// === WhatsNew ===
// ================

/** Community updates for the app. */
export default function WhatsNew() {
  return (
    <div className="flex flex-col gap-subheading px-home-section-x">
      <h2 className="text-subheading">Discover what&rsquo;s new</h2>
      <div className="grid gap-news-items grid-cols-fill-news-items">
        <a
          className="relative whatsnew-span-2 col-span-1 sm:col-span-2 bg-v3 text-tag-text rounded-default h-news-item"
          rel="noreferrer"
          target="_blank"
          href="https://enso.org/"
          style={{ background: `url(${IntegrationsImage}) top -85px right -390px / 1055px` }}
        >
          <div className="absolute flex flex-col bottom p-news-item-description w-full">
            <span className="text-subheading font-bold">
              Read what&rsquo;s new in Enso 3.0 Beta
            </span>
            <span className="text-sm leading-snug py-news-item-subtitle-y">
              Learn about Enso Cloud, new data libraries, and Enso AI.
            </span>
          </div>
        </a>
        <a
          className="relative bg-youtube text-tag-text rounded-default h-news-item"
          rel="noreferrer"
          target="_blank"
          href="https://www.youtube.com/c/Enso_org"
        >
          <img className="absolute top-6 left-1/2 -translate-x-1/2 mx-auto" src={YoutubeIcon} />
          <div className="absolute flex flex-col bottom p-news-item-description w-full">
            <span className="text-subheading font-bold">Watch weekly Enso tutorials</span>
            <span className="text-sm leading-snug py-news-item-subtitle-y">
              Subscribe not to miss new weekly tutorials.
            </span>
          </div>
        </a>
        <a
          className="relative bg-discord text-tag-text rounded-default h-news-item"
          rel="noreferrer"
          target="_blank"
          href="https://discord.gg/enso"
        >
          <img className="absolute top-7 left-1/2 -translate-x-1/2 mx-auto" src={DiscordIcon} />
          <div className="absolute flex flex-col bottom p-news-item-description w-full">
            <span className="text-subheading font-bold">Join our community server</span>
            <span className="text-sm leading-snug py-news-item-subtitle-y">
              Chat with our team and other Enso users.
            </span>
          </div>
        </a>
      </div>
    </div>
  )
}
