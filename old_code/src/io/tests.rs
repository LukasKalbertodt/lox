use super::{
    FileFormat,
};

#[test]
fn from_extension() {
    macro_rules! check_for_format {
        ($lower:literal $upper:literal $mixed:literal => $variant:ident) => {{
            let exp = Some(FileFormat::$variant);
            assert_eq!(FileFormat::from_extension(concat!("foo.", $lower)), exp);
            assert_eq!(FileFormat::from_extension(concat!("foo.", $upper)), exp);
            assert_eq!(FileFormat::from_extension(concat!("foo.", $mixed)), exp);
            assert_eq!(FileFormat::from_extension(concat!("föö.", $lower)), exp);
            assert_eq!(FileFormat::from_extension(concat!("/bar/foo.", $lower)), exp);
        }}
    }

    check_for_format!("ply" "PLY" "pLy" => Ply);
    check_for_format!("stl" "STL" "sTl" => Stl);
}

#[test]
fn from_extension_none() {
    assert_eq!(FileFormat::from_extension("foo/bar/"), None);
    assert_eq!(FileFormat::from_extension("foo/bar"), None);
    assert_eq!(FileFormat::from_extension("foo/bar."), None);
    assert_eq!(FileFormat::from_extension("foo/.bar"), None);
    assert_eq!(FileFormat::from_extension("foo/bröther"), None);
}

#[test]
fn extensions() {
    assert_eq!(FileFormat::Ply.extensions(), ["ply"]);
    assert_eq!(FileFormat::Stl.extensions(), ["stl"]);
}

#[test]
fn from_file_start() {
    assert_eq!(FileFormat::from_file_start(b"ply\n"), Some(FileFormat::Ply));
    assert_eq!(FileFormat::from_file_start(b"solid"), Some(FileFormat::Stl));
    assert_eq!(FileFormat::from_file_start(&[0; 90]), Some(FileFormat::Stl));

    assert_eq!(FileFormat::from_file_start(b""), None);
    assert_eq!(FileFormat::from_file_start(b"peter"), None);
}
