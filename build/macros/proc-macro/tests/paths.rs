use enso_build_base::prelude::*;



mod paths {
    use enso_build_macros::make_paths;

    make_paths! {
        r#"
<root>/:
    item:
    item-<root>.bar:"#
    }
}

#[test]
fn test_path_generation() -> Result {
    let paths = paths::Root::new("name");
    assert_eq!(paths.path, PathBuf::from("name"));
    assert_eq!(paths.item.path, PathBuf::from_iter(["name", "item"]));
    assert_eq!(paths.item_root_bar.path, PathBuf::from_iter(["name", "item-name.bar"]));

    Ok(())
}
