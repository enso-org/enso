"""
Repository definitions for sbt toolchain. Custom repository rule to download and extract the sbt binary.
"""

DOWNLOAD_LINK = "https://github.com/sbt/sbt/releases/download/v1.11.2/sbt-1.11.2.zip"
LAUNCH_JAR = "sbt/bin/sbt-launch.jar"

def _sbt_repo(rctx):
    rctx.download_and_extract(
        url = DOWNLOAD_LINK,
    )

    rctx.template("BUILD.bazel", rctx.attr._build_tpl, substitutions = {
        "{sbt_binary}": str(rctx.path(LAUNCH_JAR)),
    })

sbt_repo = repository_rule(
    implementation = _sbt_repo,
    attrs = {
        "_build_tpl": attr.label(
            default = "@//toolchains/sbt:BUILD.bazel.tpl",
        ),
    },
)

def _sbt_ext(_rctx):
    sbt_repo(name = "sbt_repo")

sbt_ext = module_extension(
    implementation = _sbt_ext,
)
