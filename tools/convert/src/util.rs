use std::{
    convert::{TryInto},
    str::FromStr,
};
use lox::{
    io::{
        ply, stl, FileFormat, FileEncoding,
    },
};


pub fn encoding_str(encoding: FileEncoding) -> &'static str {
    match encoding {
        FileEncoding::Ascii => "ASCII",
        FileEncoding::BinaryLittleEndian => "little endian binary",
        FileEncoding::BinaryBigEndian => "big endian binary",
    }
}

/// The encoding the user requested.
#[derive(Debug, Clone, Copy)]
pub enum EncodingRequest {
    Specific(FileEncoding),

    /// Specific encoding: native endian binary
    BinaryNative,

    /// Broad request: some binary encoding, native endianess is preferred
    Binary,
}

impl EncodingRequest {
    pub fn encoding_for(&self, file_format: FileFormat) -> Option<FileEncoding> {
        match file_format {
            FileFormat::Ply => TryInto::<ply::Encoding>::try_into(*self).ok().map(Into::into),
            FileFormat::Stl => TryInto::<stl::Encoding>::try_into(*self).ok().map(Into::into),
            other => panic!("File format {} not supported", other),
        }
    }
}

impl FromStr for EncodingRequest {
    type Err = String;
    fn from_str(src: &str) -> Result<Self, Self::Err> {
        match src {
            "binary" => Ok(EncodingRequest::Binary),
            "bne" => Ok(EncodingRequest::BinaryNative),
            "bbe" => Ok(EncodingRequest::Specific(FileEncoding::BinaryBigEndian)),
            "ble" => Ok(EncodingRequest::Specific(FileEncoding::BinaryLittleEndian)),
            "ascii" => Ok(EncodingRequest::Specific(FileEncoding::Ascii)),
            other => Err(format!(
                "'{}' is not a valid file encoding (possible values: 'binary' (endianess not \
                    specified, but prefer native), 'bne' (binary native endian), \
                    'bbe' (binary big endian), 'ble' (binary little endian) and 'ascii')",
                other,
            )),
        }
    }
}

impl TryInto<ply::Encoding> for EncodingRequest {
    type Error = ();
    fn try_into(self) -> Result<ply::Encoding, Self::Error> {
        match self {
            EncodingRequest::Binary => Ok(ply::Encoding::binary_native()),
            EncodingRequest::BinaryNative => Ok(ply::Encoding::binary_native()),
            EncodingRequest::Specific(FileEncoding::BinaryBigEndian)
                => Ok(ply::Encoding::BinaryBigEndian),
            EncodingRequest::Specific(FileEncoding::BinaryLittleEndian)
                => Ok(ply::Encoding::BinaryLittleEndian),
            EncodingRequest::Specific(FileEncoding::Ascii) => Ok(ply::Encoding::Ascii),
        }
    }
}

impl TryInto<stl::Encoding> for EncodingRequest {
    type Error = ();
    fn try_into(self) -> Result<stl::Encoding, Self::Error> {
        match self {
            EncodingRequest::Binary => Ok(stl::Encoding::Binary),
            EncodingRequest::BinaryNative => {
                if cfg!(target_endian = "little") {
                    Ok(stl::Encoding::Binary)
                } else {
                    Err(())
                }
            }
            EncodingRequest::Specific(FileEncoding::BinaryBigEndian)
                => Err(()),
            EncodingRequest::Specific(FileEncoding::BinaryLittleEndian)
                => Ok(stl::Encoding::Binary),
            EncodingRequest::Specific(FileEncoding::Ascii)
                => Ok(stl::Encoding::Ascii),
        }
    }
}
