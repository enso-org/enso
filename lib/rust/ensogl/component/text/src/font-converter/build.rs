use std::process::Command;


const URL: &str = "git@github.com:enso-org/fonts.git";

fn main() {
    // println!("cargo:rerun-if-changed=build.rs");
    // let out = env::var("OUT_DIR").unwrap();

    let spawn_err = "Failed to execute 'git'. Make sure it is available in your PATH.";
    let git_clone = Command::new("git").arg("clone");
    let out = git_clone.arg(URL).arg("dist/fonts/src").output().expect(spawn_err);
    if !out.status.success() {
        let err = std::str::from_utf8(&out.stderr).unwrap();
        println!(
            "cargo:warning=Cannot download fonts from {URL}. The app will be bundled with default \
            fonts instead. Make sure that you have access to the repository and your SSH \
            configuration is set up correctly. Due to legal constraints, the fonts can be \
            accessed by Enso team only."
        );
    }

    // let out_dir = path::Path::new(&out);
}
