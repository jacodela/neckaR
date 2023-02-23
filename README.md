
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neckaR <a href='https://lisamaierlab.com/'><img src='man/figures/neckaR.png' align="right" height="139" /></a>

<!-- badges: start -->
<!-- badges: end -->

The `neckaR` package is a collection of functions for the analysis of
bacterial growth curves. The functions included in the package assist in
the loading of files with optical density readings and experimental
design, the quality control of the curves and the calculation of indices
such as the area under the curve and the maximum OD value. Created and
used by the Maier Lab at the University of Tübingen, Germany.

## Citation

If you use `neckaR`, please cite \> \[Citation needed\]

The functions contained in `neckaR` are based on the code used in the
paper:

> Maier, L., Pruteanu, M., Kuhn, M. et al.  Extensive impact of
> non-antibiotic drugs on human gut bacteria. Nature 555, 623–628
> (2018). <https://doi.org/10.1038/nature25979>

## Installation

You can install the development version of neckaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jacodela/neckaR")
```

## Usage

You can check out the introductory vignette for a quick start tutorial
by typing:

``` r
vignette("Bacterial_Curve_Analysis",  package="neckaR")
```

For a comprehensive list of functions, you can explore the reference
documentation:

``` r
help(package="neckaR")
```

## Help & Contributing

If you have questions or come across a bug, you can [create a new issue
in](https://github.com/jacodela/neckaR/issues).

## Why neckaR?

[The Neckar](https://en.wikipedia.org/wiki/Neckar) is a river that flows
through the state of Baden-Württemberg, in the southwest of Germany. On
the banks of the river Neckar lie the cities of Heidelberg and Tübingen,
where this package has been developed. First in the laboratory of Nassos
Typas at EMBL and now in the laboratory of Lisa Maier at the University
of Tübingen. It is impossible not to compare the meandering of the river
with the bacterial growth curves. Although to be perfectly honest, the
word ends with an R and we had to take advantage of that.
