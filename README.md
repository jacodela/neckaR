
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neckaR <a href='https://lisamaierlab.com/'><img src='man/figures/neckaR.png' align="right" height="139" /></a>

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/jacodela/neckaR/blob/main/LICENSE.md)
<!-- badges: end -->

The `neckaR` package is a collection of functions for the analysis of
bacterial growth curves. The functions included in the package assist in
the loading of files with optical density readings and experimental
design, the quality control of the curves and the calculation of indices
such as the area under the curve and the maximum OD value. Created and
used by the Maier Lab at the University of Tübingen, Germany.

## Citation

If you use `neckaR`, please cite

> Müller P., de la Cuesta-Zuluaga J. et al.  High-Throughput Screening
> Strategies for the Identification of Active Compounds against Gut
> Bacteria. (2023).

The functions contained in `neckaR` are based on the code used in the
paper:

> Maier, L., Pruteanu, M., Kuhn, M. et al.  Extensive impact of
> non-antibiotic drugs on human gut bacteria. Nature 555, 623–628
> (2018). <https://doi.org/10.1038/nature25979>

# System Requirements

## Hardware requirements

The `neckaR` package requires only a standard personal computer with at
least 8 GB RAM to support the in-memory operations.

## Software requirements

### OS Requirements

The package has been tested on the following operating systems:

- Windows 10
- macOS Ventura 13.3.1
- Linux Ubuntu 20.04

## Installation

You can install the development version of `neckaR` from
[GitHub](https://github.com/) with:

``` r
# install neckaR with the vignettes
install.packages("devtools")
devtools::install_github("Lisa-Maier-Lab/neckaR", 
    build_vignettes = TRUE, 
    force = TRUE)
```

A fresh installation of `neckaR` and its dependencies should take
approximately 3 to 5 minutes, depending on the internet connection and
the computer of the user.

### Dependencies

The `neckaR` package depends on the following libraries:

    dplyr,
    tidyr,
    tibble,
    stringr,
    ggplot2,
    purrr,
    vctrs,
    fitdistrplus,
    readxl,
    magrittr,
    rlang,
    grDevices,
    stats,
    data.table

These dependencies should be installed together with `neckaR`; if this
is not the case, users can manually install them with the following
command:

``` r
install.packages(c("dplyr", "tidyr", "tibble", "stringr", "ggplot2", "purrr", "vctrs", 
    "fitdistrplus", "readxl", "magrittr", "rlang", "grDevices", "stats", "data.table"))
```

# Usage

You can check out the introductory vignette for a quick start tutorial
by typing:

``` r
vignette("Bacterial_Curve_Analysis", package="neckaR")
```

The execution of the complete vignette should take approximately 5
minutes, though the functions within the `neckaR` package are thought to
be executed interactively, that it, the user is encouraged to verify the
results of the intermediate steps and to adjust the parameters of the
steps accordingly.

For a comprehensive list of functions, you can explore the reference
documentation:

``` r
help(package="neckaR")
```

# Help & Contributing

`neckaR` is under active development, so user beware. Precisely because
of that, we rely you, the user, to point us towards things we can
improve. If you have questions or come across a bug, you can [create a
new issue here](https://github.com/jacodela/neckaR/issues).

# Why neckaR?

[The Neckar](https://en.wikipedia.org/wiki/Neckar) is a river that flows
through the state of Baden-Württemberg, in the southwest of Germany. On
the banks of the river Neckar lie the cities of Heidelberg and Tübingen,
where this package has been developed. First in the laboratory of Nassos
Typas at EMBL and now in the laboratory of Lisa Maier at the University
of Tübingen. It is impossible not to compare the meandering of the river
with the bacterial growth curves. Although to be perfectly honest, the
word ends with an R and we had to take advantage of that.
