use std::{
    cmp::{max, min},
    io::{self, BufReader, Read},
    ops,
};

use super::{Error, Input};


/// The initial size of the buffer in bytes. (TODO: this should be higher than
/// 16, but 16 is good for catching bugs).
const START_BUFFER_SIZE: usize = 16;

/// The maximum size the internal buffer can grow to. This is just a security
/// mechanism: usually, parsers won't parse a huge chunk of data at once. Each
/// "piece" of information in a file is usually very small, almost always below
/// 1KB. The internal buffer is only used to offer a byte slice for parsing one
/// such entity. However, if a parser has a bug, it might try to request very
/// large chunk of data. To avoid dying from OOM, we limit the buffer size.
const MAX_BUFFER_SIZE: usize = 4 * 1024 * 1024;



crate struct Buffer<R: Read> {
    buf: Vec<u8>,
    reader: BufReader<R>,
    start: usize,
    end: usize,
    consumed_total: usize,
}

impl<R: Read> Buffer<R> {
    crate fn new(reader: R) -> Result<Self, io::Error> {
        let mut out = Self {
            buf: vec![0; START_BUFFER_SIZE],
            reader: BufReader::new(reader),
            start: 0,
            end: 0,
            consumed_total: 0,
        };

        // Read once to prefill the buffer.
        out.fill_buf()?;

        Ok(out)
    }

    fn len(&self) -> usize {
        self.end - self.start
    }

    fn cap(&self) -> usize {
        self.buf.len()
    }

    crate fn consumed_total(&self) -> usize {
        self.consumed_total
    }

    /// Tries to fill the buffer with some new data, starting at `self.end`.
    ///
    /// `self.end` must not be equal to `self.cap()`! This function doesn't
    /// grow the buffer, it simply reads some data to the back of the buffer.
    fn fill_buf(&mut self) -> Result<(), io::Error> {
        let n = self.reader.read(&mut self.buf[self.end..])?;
        self.end += n;
        Ok(())
    }

    fn fill_buf_by(&mut self, additional: usize) -> Result<(), io::Error> {
        let space_after = self.cap() - self.end;
        let space_before = self.start;

        // If we still have enough buffer space left at the end, we can just
        // read new data. If that's not the case, we have to do some work to
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
                new.extend_from_slice(&self);
                new.resize(new_len, 0);
                self.buf = new;

            }

            // In both cases, the data starts at the very beginning now.
            self.end -= self.start;
            self.start = 0;
        }

        // Read new data until we have read `additional` many bytes. We ignore
        // `Interrupted` errors and just continue.
        let mut bytes_read = 0;
        while bytes_read < additional {
            match self.reader.read(&mut self.buf[self.end..]) {
                Ok(0) => {
                    // We know that `self.buf[self.end..]` is not an empty
                    // slice (we made sure of that above by growing the buffer
                    // or moving data). This means that the reader is
                    // exhausted.
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "unexpected EOF",
                    ));
                }
                Ok(n) => bytes_read += n,
                Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }

        self.end += bytes_read;
        Ok(())
    }
}

impl<R: Read> ops::Deref for Buffer<R> {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        &self.buf[self.start..self.end]
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
        buf[..n].copy_from_slice(&self[..n]);
        self.consume(n);

        Ok(n)
    }
}

impl<R: Read> Input for Buffer<R> {
    fn prepare(&mut self, num_bytes: usize) -> Result<(), Error> {
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
        if self.start == self.end {
            self.start = 0;
            self.end = 0;
        }
    }
}
