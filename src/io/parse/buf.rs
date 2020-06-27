use std::{
    cmp::{max, min},
    fmt,
    io::{self, Read},
};

use crate::io::Error;
use super::{ParseBuf, ParseError};


/// The initial size of the buffer in bytes.
const START_BUFFER_SIZE: usize = 8 * 1024;

/// The maximum size the internal buffer can grow to.
///
/// This is just a security mechanism: usually, parsers won't parse a huge
/// chunk of data at once. Each "piece" of information in a file is usually
/// very small, almost always below 1KB. The internal buffer is only used to
/// offer a byte slice for parsing one such entity. However, if a parser has a
/// bug, it might try to request very large chunk of data. To avoid dying from
/// OOM, we limit the buffer size.
pub(crate) const MAX_BUFFER_SIZE: usize = 4 * 1024 * 1024;

// The correctness of some code depends on this.
static_assertions::const_assert!(MAX_BUFFER_SIZE <= u32::max_value() as usize);

#[derive(Clone)]
pub(crate) struct Buffer<R: Read> {
    reader: R,

    buf: Vec<u8>,

    /// Points to the first byte in `buf` that is real data. Invariants:
    /// - `0 <= start < buf.len()`
    /// - `start <= end`
    start: usize,

    /// Points to the byte after the last byte of real data. Invariants:
    /// - `0 <= end <= buf.len()`
    /// - `start <= end`
    end: usize,

    consumed_total: usize,
}

impl<R: Read> fmt::Debug for Buffer<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Buffer {{ consumed_total: {}, .. }}", self.consumed_total)
    }
}

impl<R: Read> Buffer<R> {
    pub(crate) fn new(reader: R) -> Result<Self, Error> {
        let mut out = Self {
            buf: vec![0; START_BUFFER_SIZE],
            reader,
            start: 0,
            end: 0,
            consumed_total: 0,
        };

        // Read once to prefill the buffer.
        out.fill_buf()?;

        Ok(out)
    }

    // =======================================================================
    // ===== Internal methods
    // =======================================================================

    fn len(&self) -> usize {
        self.end - self.start
    }

    fn cap(&self) -> usize {
        self.buf.len()
    }

    /// Tries to fill the buffer with some new data, starting at `self.end`.
    ///
    /// `self.end` must not be equal to `self.cap()`! This function doesn't
    /// grow the buffer, it simply reads some data to the back of the buffer.
    fn fill_buf(&mut self) -> Result<usize, io::Error> {
        let n = self.reader.read(&mut self.buf[self.end..])?;
        self.end += n;

        Ok(n)
    }

    /// Resizes the internal buffer to have space for at least `additional`
    /// more bytes.
    ///
    /// This means that this function might do nothing, even if `additional` is
    /// not 0. If the buffer is already big enough, nothing happens.
    #[inline(never)]
    fn grow_buf(&mut self, additional: usize) {
        let space_after = self.cap() - self.end;
        let space_before = self.start;

        // If we still have enough buffer space left at the end, we don't have
        // to do anything. If that's not the case, we have to do some work to
        // get more space at the end.
        if space_after < additional {
            // Now we need to decide if we want to move the data to the
            // beginning of the buffer to get more space at the end or if we
            // want to grow the buffer.
            //
            // A prerequisite for moving data to the beginning is that the
            // space in the beginning combined with the space in the end can
            // hold `additional` bytes. If that's not the case, we have to
            // increase the buffer anyway.
            //
            // But even if we could make enough room by just moving data, we
            // don't always want to do that. In particular, if the data we need
            // to move is half as big as or bigger than the whole buffer, we
            // don't move it. For once, it would be costly, but more
            // importantly: if we don't increase the buffer in this situation,
            // we might have to copy a lot of data again and again. For
            // example, if there are alternating reads of sizes `2` and
            // `bufsize - 1`, we it might lead to moving `bufsize - 2` bytes
            // every second read. That's bad.
            if space_after + space_before >= additional && self.len() < self.cap() / 2 {
                // We simply move the data via `for` loop. We could use
                // `ptr::copy`, but we don't want to use `unsafe` code for
                // this.
                for i in self.start..self.end {
                    self.buf[i - self.start] = self.buf[i];
                }
            } else {
                // We don't have enough space left, so we need to grow the
                // buffer. If we reached the maximum buffer size, we panic.
                if self.cap() == MAX_BUFFER_SIZE {
                    panic!(
                        "Temporary buffer grew larger than {} while parsing mesh format. This \
                         is most likely a bug in the parser.",
                        MAX_BUFFER_SIZE,
                    );
                }

                // The new buffer size will be at least our current length +
                // `additional`, but no less than twice the current buffer size
                // (otherwise, reallocations might be too frequent).
                let new_len = min(
                    max(self.len() + additional, self.buf.len() * 2),
                    MAX_BUFFER_SIZE,
                );

                // We could just use `Vec::resize` but we can do better: since
                // `Vec::resize` will allocate and copy bytes around anyway, we
                // use this to move our data to the beginning of our buffer.
                let mut new = Vec::with_capacity(new_len);
                new.extend_from_slice(self.raw_buf());
                new.resize(new_len, 0);
                self.buf = new;

            }

            // In both cases, the data starts at the very beginning now.
            self.end -= self.start;
            self.start = 0;
        }
    }

    #[inline(never)]
    fn fill_buf_by(&mut self, additional: usize) -> Result<usize, Error> {
        self.grow_buf(additional);

        // Read new data until we have read `additional` many bytes. We ignore
        // `Interrupted` errors and just continue.
        let mut bytes_read = 0;
        while bytes_read < additional {
            match self.reader.read(&mut self.buf[self.end + bytes_read..]) {
                Ok(0) => {
                    // We know that `self.buf[self.end..]` is not an empty
                    // slice (we made sure of that above by growing the buffer
                    // or moving data). This means that the reader is
                    // exhausted.
                    break;
                }
                Ok(n) => bytes_read += n,
                Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e.into()),
            }
        }

        self.end += bytes_read;
        Ok(bytes_read)
    }
}

impl<R: Read> Read for Buffer<R> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, io::Error> {
        // If our buffer is empty, we might need to fill it.
        if self.len() == 0 {
            // If we can do a big read (bigger than our internal buffer), we
            // read directly from the reader instead of going through our
            // buffer. Otherwise, we fill our buffer and read from there.
            if buf.len() >= self.cap() {
                return self.reader.read(buf);
            } else {
                self.fill_buf()?;
            }
        }

        let n = min(self.len(), buf.len());
        buf[..n].copy_from_slice(&self.raw_buf()[..n]);
        self.consume(n);

        Ok(n)
    }
}

impl<R: Read> ParseBuf for Buffer<R> {
    fn prepare(&mut self, num_bytes: usize) -> Result<(), Error> {
        #[cold]
        #[inline(never)]
        fn fill(_self: &mut Buffer<impl Read>, diff: usize) -> Result<(), Error> {
            let bytes_read = _self.fill_buf_by(diff)?;

            if bytes_read < diff {
                return Err(ParseError::UnexpectedEof(_self.offset() + _self.len()).into());
            }

            Ok(())
        }

        if self.len() < num_bytes {
            return fill(self, num_bytes - self.len());
        }

        Ok(())
    }

    fn saturating_prepare(&mut self, num_bytes: usize) -> Result<(), Error> {
        if self.len() < num_bytes {
            let diff = num_bytes - self.len();
            self.fill_buf_by(diff)?;
        }

        Ok(())
    }

    fn consume(&mut self, num_bytes: usize) {
        assert!(self.start + num_bytes <= self.end);

        self.start += num_bytes;
        self.consumed_total += num_bytes;

        // If we consumed all the data, we set both indices to 0.
        //
        // TODO: optimization potential: this branch is inlined in a lot of
        // functions. Would be nice if we could remove it here. So we have to
        // find out where we rely on this property and rewrite it there. This
        // is probably only in `grow_buf` or other costly methods, so adding a
        // check there is not a problem.
        if self.start == self.end {
            self.start = 0;
            self.end = 0;
        }
    }

    fn is_eof(&mut self) -> Result<bool, Error> {
        if self.len() == 0 {
            self.grow_buf(1);
            Ok(self.fill_buf()? == 0)
        } else {
            Ok(false)
        }
    }

    fn offset(&self) -> usize {
        self.consumed_total
    }

    fn raw_buf(&self) -> &[u8] {
        // TODO: here is some optimization potential. Both bounds are
        // apparently always checked. But we know "for sure" that those bounds
        // are valid. I am just not "sure" enough to put an `unsafe` block
        // here. This needs to be evaluated more closely to verify the parse
        // buffer implementation is correct. Then we can use `get_unchecked_`
        // here.
        &self.buf[self.start..self.end]
    }
}
