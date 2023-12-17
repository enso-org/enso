fn main() {
    let icon_rc_line =
        r#"ICON_ID ICON "C:/Users/mwurb/AppData/Local/Temp/.tmpsLdSoZ/icon.ico""#.to_string();
    let rc_file_contents = [icon_rc_line].join("\n");
    std::fs::write("archive.rc", rc_file_contents).unwrap();
    embed_resource::compile("archive.rc", embed_resource::NONE);
}
