"""
SBT toolchain implementation.
"""

SbtInfo = provider(
    "Sbt toolchain info, contains the path to the sbt executable.",
    fields = [
        "sbt_bin",
    ],
)

def _impl(ctx):
    sbt_bin = ctx.attr.sbt_binary
    template_variables = platform_common.TemplateVariableInfo({
        "SBT_BIN": sbt_bin,
    })
    sbt_info = SbtInfo(
        sbt_bin = sbt_bin,
    )

    return [
        platform_common.ToolchainInfo(
            sbt_info = sbt_info,
            template_variables = template_variables,
        ),
        template_variables,
    ]

sbt_toolchain = rule(
    implementation = _impl,
    attrs = {
        "sbt_binary": attr.string(mandatory = True),
    },
)
