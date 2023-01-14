use ensogl_pack::prelude::*;
use ensogl_pack::workspace_dir;
use ensogl_pack::WasmPackOutputs;

fn main() {
    todo!()
}
//
// fn modify_arg_1(
//     args: &mut Vec<String>,
//     arg: &str,
//     f: impl Fn(&str) -> Option<String>,
// ) -> Option<String> {
//     let mut old_value = None;
//     let eq_arg = format!("{}=", arg);
//     let index = args.iter().position(|a| a == arg);
//     let eq_index = args.iter().position(|a| a.starts_with(&eq_arg));
//     if let Some(index) = index {
//         if index + 1 < args.len() {
//             old_value = Some(args[index + 1].clone());
//             match f(&args[index + 1]) {
//                 Some(new_value) => args[index + 1] = new_value,
//                 None => {
//                     args.remove(index + 1);
//                 }
//             }
//         } else {
//             old_value = Some("".to_string());
//             if let Some(new_value) = f("") {
//                 args.push(new_value);
//             }
//         }
//     } else if let Some(index) = eq_index {
//         let value = args[index].split_at(eq_arg.len()).1;
//         old_value = Some(value.to_string());
//         match f(value) {
//             Some(new_value) => args[index] = format!("{}{}", eq_arg, new_value),
//             None => {
//                 args.remove(index);
//             }
//         }
//     }
//     old_value
// }
//
// #[tokio::main]
// async fn main() -> Result {
//     setup_logging()?;
//     let original_args = std::env::args().skip(1).collect_vec();
//     let mut args =
//     let is_build = args.contains(&"build".to_string());
//     println!("{:?}", workspace_dir());
//     if is_build {
//         let workspace_dir = workspace_dir();
//         let target_dir = workspace_dir.join("target").join("ensogl-pack");
//         let target_dist_dir = target_dir.join("dist");
//
//         let original_out_dir =
//             modify_arg_1(&mut args, "--out-dir", |_| Some(target_dir.display().to_string()))
//                 .or_else(|| modify_arg_1(&mut args, "-d", |_|
// Some(target_dir.display().to_string())));         let original_out_name = modify_arg_1(&mut args,
// "--out-name", |_| Some("pkg".to_string()));
//
//         ensogl_pack::build(
//             ReplacedArgs { out_dir: out_dir.unwrap().into(), out_name: out_name.unwrap() },
//             |args| {},
//         )
//     } else {
//         ide_ci::programs::WasmPack.cmd()?.args(&args).run_ok().await?;
//     }
//     ensogl_pack::main_lib(std::env::args().skip(1).collect()).await
// }
