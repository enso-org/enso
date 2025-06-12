"""
Runtime configuration for sbt path.
"""

DOWNLOAD_LINK = "https://github.com/sbt/sbt/releases/download/v1.11.2/sbt-1.11.2.zip"

LAUNCH_JAR = "sbt/bin/sbt-launch.jar"

# TODO: Read from .jvmopts
DEFAULT_SBT_OPTS = [
]

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

def _sbt_repo(rctx):
    rctx.download(
        url = DOWNLOAD_LINK,
        output = "sbt.zip",
    )
    rctx.extract("sbt.zip")
    rctx.file("BUILD.bazel", """
load("@rules_java//java:defs.bzl", "java_binary", "java_runtime")

filegroup(
    name = "sbt_launch_jar",
    srcs = ["sbt/bin/sbt-launch.jar"],
    visibility = ["//visibility:public"],
)
""")

sbt_repo = repository_rule(
    implementation = _sbt_repo,
)

def _sbt_ext(rctx):
    sbt_repo(name = "sbt_repo")

sbt_ext = module_extension(
    implementation = _sbt_ext,
)
