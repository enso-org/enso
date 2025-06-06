"""
Runtime configuration for sbt path.
"""

def _impl(ctx):
    sbt_bin = ctx.build_setting_value
    if not sbt_bin:
        fail(str(ctx.label) + " is missing sbt_path build setting")
    return [
        platform_common.TemplateVariableInfo({
            "SBT_BIN": sbt_bin,
        }),
    ]

sbt_path = rule(
    implementation = _impl,
    build_setting = config.string(flag = True),
)
