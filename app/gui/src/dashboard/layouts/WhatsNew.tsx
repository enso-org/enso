/** @file Community updates for the app. */
import * as React from 'react'

import BookImage from '#/assets/book.png'
import Logo from '#/assets/enso_logo_large.svg'
import IntegrationsImage from '#/assets/integrations.png'
import YoutubeIcon from '#/assets/youtube.svg'
import { Button, Text } from '#/components/AriaComponents'

import * as textProvider from '#/providers/TextProvider'

import SvgMask from '#/components/SvgMask'

// ================
// === WhatsNew ===
// ================

/** Community updates for the app. */
export default function WhatsNew() {
  const { getText } = textProvider.useText()

  return (
    <div className="flex w-full flex-col">
      <Text.Heading level={2}>{getText('discoverWhatsNew')}</Text.Heading>

      <div className="-mx-12 inline-flex snap-x snap-mandatory gap-4 overflow-x-auto px-12 py-2 scroll-offset-edge-9xl">
        <Button
          variant="custom"
          size="custom"
          rounded="xxxlarge"
          className="flex-none snap-center snap-always overflow-hidden bg-accent"
          rel="noreferrer"
          target="_blank"
          href="https://help.enso.org/"
        >
          <SvgMask
            src={BookImage}
            className="absolute left-1/2 top-5 mx-auto -translate-x-1/2"
            color="white"
          />

          <div className="flex aspect-[7/4] h-40 w-full flex-col justify-end bg-gradient-to-t from-accent-dark to-transparent">
            <div className="flex w-full flex-col items-start px-4 pb-3 text-start">
              <Text variant="subtitle" color="invert" nowrap="normal">
                {getText('newsItemHelpServer')}
              </Text>
              <Text variant="body-sm" color="invert" nowrap="normal">
                {getText('newsItemHelpServerDescription')}
              </Text>
            </div>
          </div>
        </Button>
        <Button
          variant="custom"
          size="custom"
          rounded="xxxlarge"
          className="flex-none snap-center snap-always overflow-hidden bg-youtube"
          rel="noreferrer"
          target="_blank"
          href="https://www.youtube.com/@Enso_Analytics/videos"
        >
          <SvgMask
            src={YoutubeIcon}
            className="absolute left-1/2 top-6 mx-auto -translate-x-1/2"
            color="white"
          />

          <div className="from-youtube-transparent flex aspect-[7/4] h-40 w-full flex-col justify-end bg-gradient-to-t">
            <div className="flex w-full flex-col items-start px-4 pb-3 text-start">
              <Text variant="subtitle" color="invert" nowrap="normal">
                {getText('newsItemWeeklyTutorials')}
              </Text>
              <Text variant="body-sm" color="invert" nowrap="normal">
                {getText('newsItemWeeklyTutorialsDescription')}
              </Text>
            </div>
          </div>
        </Button>
        <Button
          variant="custom"
          size="custom"
          rounded="xxxlarge"
          className="flex-none snap-center snap-always overflow-hidden bg-green"
          rel="noreferrer"
          target="_blank"
          href="https://community.ensoanalytics.com/"
        >
          <SvgMask
            src={Logo}
            className="absolute left-1/2 top-4 mx-auto -translate-x-1/2"
            color="white"
          />
          <div className="flex aspect-[7/4] h-40 w-full flex-col justify-end bg-gradient-to-t from-accent-dark to-transparent">
            <div className="flex w-full flex-col items-start px-4 pb-3 text-start">
              <Text variant="subtitle" color="invert" nowrap="normal">
                {getText('newsItemCommunityServer')}
              </Text>
              <Text variant="body-sm" color="invert" nowrap="normal">
                {getText('newsItemCommunityServerDescription')}
              </Text>
            </div>
          </div>
        </Button>
        <Button
          variant="custom"
          size="custom"
          rounded="xxxlarge"
          className="flex-none snap-center snap-always overflow-hidden"
          target="_blank"
          href="https://community.ensoanalytics.com/c/what-is-new-in-enso/"
          style={{ background: `url(${IntegrationsImage}) top -85px right -390px / 1055px` }}
        >
          <div className="flex aspect-[7/4] h-40 w-full flex-col justify-end bg-gradient-to-t from-primary to-transparent">
            <div className="absolute bottom left right flex flex-col items-start px-4 pb-3 text-start">
              <Text variant="subtitle" color="invert" nowrap="normal">
                {getText('newsItem3Beta')}
              </Text>

              <Text variant="body-sm" color="invert" nowrap="normal">
                {getText('newsItem3BetaDescription')}
              </Text>
            </div>
          </div>
        </Button>
      </div>
    </div>
  )
}
