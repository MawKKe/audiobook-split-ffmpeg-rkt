# audiobook-split-ffmpeg-rkt

Split audiobook file into per-chapter files using chapter metadata and ffmpeg.

Now written in [Racket](https://racket-lang.org/) !

Note about project:
- **WIP - DOES NOT WORK YET**
- **WIP - DOES NOT WORK YET**
- **WIP - DOES NOT WORK YET**

This is merely an exercise project for me to learn Racket. It may never be complete.

If you need audiobook spliiter, refer to these more complete projects:
- [audiobook-split-ffmpeg (in Python)](https://github.com/MawKKe/audiobook-split-ffmpeg)
- [audiobook-split-ffmpeg (in Go)](https://github.com/MawKKe/audiobook-split-ffmpeg-go)

## Requirements

Install `ffmpeg`, and make sure it is available via your `$PATH`.

Otherwise, everything else is implemented using the Racket base libraries.

## Current status

Can parse chapters from input file and produce a list of ffmpeg arguments for each.

Does not run ffmpeg to do the actual splitting

## Run demo

    $ racket process.rkt
