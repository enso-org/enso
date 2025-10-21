"""
Runs a script with a path to the stable status file as an argument.
"""

load("@aspect_bazel_lib//lib:stamping.bzl", "STAMP_ATTRS", "maybe_stamp")

def _generate_build_info_impl(ctx):
    args = ctx.actions.args()
    args.add(ctx.outputs.out.path)
    inputs = []
    outputs = [ctx.outputs.out]
    stamp = maybe_stamp(ctx)
    if stamp:
        args.add(stamp.stable_status_file.path)
        inputs = [stamp.stable_status_file]

    ctx.actions.run(
        inputs = inputs,
        outputs = outputs,
        arguments = [args],
        env = {
            "BAZEL_BINDIR": ctx.bin_dir.path,
        },
        executable = ctx.executable._stamp_exec,
    )
    return [DefaultInfo(files = depset(outputs))]

generate_build_info = rule(
    implementation = _generate_build_info_impl,
    attrs = dict({
        "out": attr.output(mandatory = True),
        "_stamp_exec": attr.label(executable = True, default = Label("//app/electron-client:create_build_info_script"), cfg = "exec"),
    }, **STAMP_ATTRS),
)
