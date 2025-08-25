"""
Flatc toolchain implementation. 
"""

FlatcInfo = provider(
    "Flatbuffers compiler info, contains the name of the flatc executable and the path to the flatc binary directory.",
    fields = [
        "flatc_bin",
        "flatc_path",
    ],
)

def _impl(ctx):
    flatc_bin = ctx.attr.flatc_binary
    flatc_path = ctx.attr.flatc_path
    template_variables = platform_common.TemplateVariableInfo({
        "FLATC_BIN": flatc_bin,
        "FLATC_PATH": flatc_path,
    })
    flatc_info = FlatcInfo(
        flatc_bin = flatc_bin,
        flatc_path = flatc_path,
    )

    return [
        platform_common.ToolchainInfo(
            flatc_info = flatc_info,
            template_variables = template_variables,
        ),
        template_variables,
    ]

flatc_toolchain = rule(
    implementation = _impl,
    attrs = {
        "flatc_binary": attr.string(mandatory = True),
        "flatc_path": attr.string(mandatory = True),
    },
)

def _resolve_toolchain_impl(ctx):
    toolchain_info = ctx.toolchains["@//toolchains/flatc:toolchain_type"]
    return [
        toolchain_info,
        toolchain_info.template_variables,
    ]

resolve_toolchain = rule(
    toolchains = ["@//toolchains/flatc:toolchain_type"],
    implementation = _resolve_toolchain_impl,
)
