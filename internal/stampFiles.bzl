"""
Runs a node.js script with the following arguments:
- input path (file or directory), where the files to be stamped are located
- output path (file or directory), where the stamped files must be placed
- whether content-hash filenames should be recalculated (`true` / `false`)
- status file path (optional)

If the stamping is globally disabled, the status file path is not provided, and the script should just copy the files to the output path.
"""

load("@aspect_bazel_lib//lib:expand_make_vars.bzl", "expand_variables")
load("@aspect_bazel_lib//lib:stamping.bzl", "STAMP_ATTRS", "maybe_stamp")

def _stamp_files_impl(ctx):
    if ctx.attr.input_dir and ctx.attr.input_path:
        fail("Only one of input_dir and input_path may be set.")

    input_path_attr = ctx.attr.input_path or ctx.attr.input_dir
    if not input_path_attr:
        fail("One of input_dir or input_path must be set.")

    if ctx.attr.output_is_directory:
        output = ctx.actions.declare_directory(ctx.attr.out)
    else:
        output = ctx.actions.declare_file(ctx.attr.out)

    outputs = [output]
    inputs = [] + ctx.files.srcs
    input_path = expand_variables(ctx, ctx.expand_location(input_path_attr, targets = ctx.attr.srcs), outs = outputs)
    args = ctx.actions.args()
    args.add(input_path)
    args.add(output.path)
    args.add("true" if ctx.attr.recalculate_hashes else "false")
    stamp = maybe_stamp(ctx)
    if stamp:
        args.add(stamp.stable_status_file.path)
        inputs.append(stamp.stable_status_file)

    ctx.actions.run(
        inputs = inputs,
        outputs = outputs,
        arguments = [args],
        env = {
            "BAZEL_BINDIR": ctx.bin_dir.path,
        },
        executable = ctx.executable._stamp_exec,
    )
    return [DefaultInfo(files = depset(outputs), runfiles = ctx.runfiles(files = outputs))]

stamp_files = rule(
    implementation = _stamp_files_impl,
    attrs = dict({
        "srcs": attr.label_list(allow_files = True, mandatory = True, doc = "The files to be stamped."),
        "input_dir": attr.string(default = "", doc = "Deprecated alias for input_path. The path to the input directory, relative to exec root."),
        "input_path": attr.string(default = "", doc = "The path to the input file or directory, relative to exec root."),
        "out": attr.string(mandatory = True, doc = "The path to the output file or directory, relative to exec root."),
        "output_is_directory": attr.bool(default = True, doc = "Whether `out` is a directory. Set to False for a single output file."),
        "recalculate_hashes": attr.bool(default = False, doc = "Whether to recalculate content hashes encoded in filenames and rewrite references to renamed files."),
        "_stamp_exec": attr.label(executable = True, default = Label("//internal:script_env_replacer"), cfg = "exec"),
    }, **STAMP_ATTRS),
)
