/** @file Community updates for the app. */
import * as React from 'react'

import Logo from '#/assets/enso_logo_large.svg'
import BookImage from '#/assets/book.png'
import IntegrationsImage from '#/assets/integrations.png'
import YoutubeIcon from '#/assets/youtube.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

// ================
// === WhatsNew ===
// ================

/** Community updates for the app. */
export default function WhatsNew() {
  const { getText } = textProvider.useText()

  return (
    <div className="flex flex-col gap-subheading px-[5px]">
      <aria.Heading level={2} className="text-subheading font-normal">
        {getText('discoverWhatsNew')}
      </aria.Heading>
      <div className="grid grid-cols-fill-news-items gap-news-items">
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <FocusRing>
              <a
                className="focus-child relative h-news-item rounded-default bg-green text-tag-text"
                rel="noreferrer"
                target="_blank"
                href="https://help.enso.org/"
                {...innerProps}
              >
                <img
                  className="absolute left-1/2 top-7 mx-auto -translate-x-1/2"
                  src={BookImage}
                />
                <div className="absolute bottom flex w-full flex-col p-news-item-description">
                  <aria.Text className="text-subheading font-bold">
                    {getText('newsItemHelpServer')}
                  </aria.Text>
                  <aria.Text className="py-news-item-subtitle-y text-sm leading-snug">
                    {getText('newsItemHelpServerDescription')}
                  </aria.Text>
                </div>
              </a>
            </FocusRing>
          )}
        </FocusArea>
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <FocusRing>
              <a
                className="focus-child relative h-news-item rounded-default bg-youtube text-tag-text"
                rel="noreferrer"
                target="_blank"
                href="https://www.youtube.com/@Enso_Analytics/videos"
                {...innerProps}
              >
                <img
                  className="absolute left-1/2 top-6 mx-auto -translate-x-1/2"
                  src={YoutubeIcon}
                />
                <div className="absolute bottom flex w-full flex-col p-news-item-description">
                  <aria.Text className="text-subheading font-bold">
                    {getText('newsItemWeeklyTutorials')}
                  </aria.Text>
                  <aria.Text className="py-news-item-subtitle-y text-sm leading-snug">
                    {getText('newsItemWeeklyTutorialsDescription')}
                  </aria.Text>
                </div>
              </a>
            </FocusRing>
          )}
        </FocusArea>
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <FocusRing>
              <a
                className="focus-child relative h-news-item rounded-default bg-green text-tag-text"
                rel="noreferrer"
                target="_blank"
                href="https://community.ensoanlytics.com/"
                {...innerProps}
              >
                <img
                  className="absolute left-1/2 top-7 mx-auto -translate-x-1/2"
                  src={Logo}
                />
                <div className="absolute bottom flex w-full flex-col p-news-item-description">
                  <aria.Text className="text-subheading font-bold">
                    {getText('newsItemCommunityServer')}
                  </aria.Text>
                  <aria.Text className="py-news-item-subtitle-y text-sm leading-snug">
                    {getText('newsItemCommunityServerDescription')}
                  </aria.Text>
                </div>
              </a>
            </FocusRing>
          )}
        </FocusArea>
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <FocusRing>
              <a
                className="focus-child relative col-span-1 h-news-item rounded-default bg-v3 text-tag-text"
                rel="noreferrer"
                target="_blank"
                href="https://community.ensoanalytics.com/c/what-is-new-in-enso/"
                style={{ background: `url(${IntegrationsImage}) top -85px right -390px / 1055px` }}
                {...innerProps}
              >
                <div className="absolute bottom flex w-full flex-col p-news-item-description">
                  <aria.Text className="text-subheading font-bold">
                    {getText('newsItem3Beta')}
                  </aria.Text>
                  <aria.Text className="py-news-item-subtitle-y text-sm leading-snug">
                    {getText('newsItem3BetaDescription')}
                  </aria.Text>
                </div>
              </a>
            </FocusRing>
          )}
        </FocusArea>
      </div>
    </div>
  )
}
