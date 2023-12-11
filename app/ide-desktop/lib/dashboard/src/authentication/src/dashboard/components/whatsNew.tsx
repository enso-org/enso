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
        <div className="flex flex-col gap-4 px-4.75">
            <h2 className="text-xl leading-144.5 py-0.5">Discover what&rsquo;s new</h2>
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
                            Read what&rsquo;s new in Enso 3.0 Beta
                        </span>
                        <span className="text-sm leading-144.5 py-0.5">
                            Learn about Enso Cloud, new data libraries, and Enso AI.
                        </span>
                    </div>
                </a>
                <a
                    className="relative bg-youtube text-tag-text rounded-2xl h-45"
                    rel="noreferrer"
                    target="_blank"
                    href="https://www.youtube.com/c/Enso_org"
                >
                    <img
                        className="absolute top-6 left-1/2 -translate-x-1/2 mx-auto"
                        src={YoutubeIcon}
                    />
                    <div className="absolute flex flex-col bottom-0 p-4">
                        <span className="text-xl font-bold leading-144.5 py-0.5">
                            Watch weekly Enso tutorials
                        </span>
                        <span className="text-sm leading-144.5 py-0.5">
                            Subscribe not to miss new weekly tutorials.
                        </span>
                    </div>
                </a>
                <a
                    className="relative bg-discord text-tag-text rounded-2xl h-45"
                    rel="noreferrer"
                    target="_blank"
                    href="https://discord.gg/enso"
                >
                    <img
                        className="absolute top-7 left-1/2 -translate-x-1/2 mx-auto"
                        src={DiscordIcon}
                    />
                    <div className="absolute flex flex-col bottom-0 p-4">
                        <span className="text-xl font-bold leading-144.5 py-0.5">
                            Join our community server
                        </span>
                        <span className="text-sm leading-144.5 py-0.5">
                            Chat with our team and other Enso users.
                        </span>
                    </div>
                </a>
            </div>
        </div>
    )
}
