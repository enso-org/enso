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
            <h2 className="py-0.5 text-xl leading-144.5">Discover what&rsquo;s new</h2>
            <div className="grid grid-cols-fill-75 gap-3">
                <a
                    className="whatsnew-span-2 relative col-span-2 h-45 rounded-2xl bg-v3 text-tag-text"
                    rel="noreferrer"
                    target="_blank"
                    href="https://enso.org/"
                    style={{
                        background: `url(${IntegrationsImage}) top -85px right -390px / 1055px`,
                    }}
                >
                    <div className="absolute bottom-0 flex flex-col p-4">
                        <span className="py-0.5 text-xl font-bold leading-144.5">
                            Read what&rsquo;s new in Enso 3.0 Beta
                        </span>
                        <span className="py-0.5 text-sm leading-144.5">
                            Learn about Enso Cloud, new data libraries, and Enso AI.
                        </span>
                    </div>
                </a>
                <a
                    className="relative h-45 rounded-2xl bg-youtube text-tag-text"
                    rel="noreferrer"
                    target="_blank"
                    href="https://www.youtube.com/c/Enso_org"
                >
                    <img
                        className="absolute left-1/2 top-6 mx-auto -translate-x-1/2"
                        src={YoutubeIcon}
                    />
                    <div className="absolute bottom-0 flex flex-col p-4">
                        <span className="py-0.5 text-xl font-bold leading-144.5">
                            Watch weekly Enso tutorials
                        </span>
                        <span className="py-0.5 text-sm leading-144.5">
                            Subscribe not to miss new weekly tutorials.
                        </span>
                    </div>
                </a>
                <a
                    className="relative h-45 rounded-2xl bg-discord text-tag-text"
                    rel="noreferrer"
                    target="_blank"
                    href="https://discord.gg/enso"
                >
                    <img
                        className="absolute left-1/2 top-7 mx-auto -translate-x-1/2"
                        src={DiscordIcon}
                    />
                    <div className="absolute bottom-0 flex flex-col p-4">
                        <span className="py-0.5 text-xl font-bold leading-144.5">
                            Join our community server
                        </span>
                        <span className="py-0.5 text-sm leading-144.5">
                            Chat with our team and other Enso users.
                        </span>
                    </div>
                </a>
            </div>
        </div>
    )
}
