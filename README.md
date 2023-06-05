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

```

Usage:
  mrcide/outpack.orderly <src> <dest> [--once] [--minutes=<x>] [--custom=<cron-schedule>]

Options:
  --once                      Perform migration once.
  --minutes=<x>               Schedule migration every x minutes.
  --custom=<cron-schedule>    Schedule migration using cron.

```

You will have to first mount the `orderly` and `outpack` directories as volumes.
`orderly` can be readonly. 

```
docker run -v orderly:/orderly:ro \
           -v outpack:/outpack \
           mrcide/outpack.orderly /orderly /outpack --once
```

If running on a schedule, you most likely want to run in detached mode:

```
docker run -v orderly:/orderly:ro \
           -v outpack:/outpack \
           -d \
           mrcide/outpack.orderly /orderly /outpack --custom="* * * * *"
```

If not using named volumes, you probably want to run as the host user to avoid 
root owned files being created:

```
docker run -v /some/path/orderly:/orderly:ro \
           -v /another/path/outpack:/outpack \
           -d \
           -u $UID \
           mrcide/outpack.orderly /orderly /outpack --custom="* * * * *"
```


## Usage without docker

Recommended usage is from docker but to run a one-off migration on metal,
first install this package from source and then use `./orderly2outpack.R`:

```

Usage:
  ./orderly2outpack.R <src> <dest>

```

## License

MIT © Imperial College of Science, Technology and Medicine
