# outpack.orderly

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/outpack.orderly/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/outpack.orderly/actions)
[![codecov.io](https://codecov.io/github/mrc-ide/outpack.orderly/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/outpack.orderly?branch=main)
<!-- badges: end -->

A temporary package to play around with outpack migration etc.

## Installation

To install `outpack.orderly`:

```r
remotes::install_github("mrc-ide/outpack.orderly", upgrade = FALSE)
```

## Usage from docker

Pass the `src` and `destination` directories as command line arguments.
Note that the `dest` directory must not exist, so you have to mount a parent directory 
into the container and specify the desired dest as a subdirectory:
```
docker run -v /orderly/path:/orderly \
           -v /outpack/parent:/parent \
           mrcide/outpack.orderly /orderly /parent/outpack
```


## License

MIT © Imperial College of Science, Technology and Medicine
