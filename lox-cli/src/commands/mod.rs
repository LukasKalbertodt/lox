use std::{
    fs::File,
    io::{Read, Seek, SeekFrom},
};

use failure::{err_msg,Error};
use lox::{
    io::{
        FileFormat,
    },
};


pub mod convert;
pub mod info;


/// Tries to find the file format of the given file.
///
/// The explicitly specified format `file_format` is preferred. If that's not
/// set, the format is guessed from the filename. If that won't work, it's
/// guess from the file start.
///
/// The seek position of `file` has to be at the very start. It is not changed
/// by this function.
fn guess_file_format(
    file_format: Option<FileFormat>,
    filename: &str,
    file: &mut File,
) -> Result<FileFormat, Error> {
    // If the file format is explicitly specified or can be guessed via
    // extension, return that.
    if let Some(format) = file_format.or_else(|| FileFormat::from_extension(filename)) {
        return Ok(format);
    }

    // Otherwise try to guess from the start of the file
    let mut start = Vec::new();
    file.by_ref().take(1024).read_to_end(&mut start)?;
    file.seek(SeekFrom::Start(0))?;

    FileFormat::from_file_start(&start).ok_or_else(|| err_msg(
        "couldn't determine source file format, please specify it explicitly using \
            '--source-format'"
    ))
}
