
<!-- README.md is generated from README.Rmd. Please edit that file -->

# funbox

<!-- badges: start -->

<!-- badges: end -->

This is a work-in-progress package of miscellaneous functions.

## Installation

You can install the development version of funbox from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("clayford/funbox")
```

## Example

One such function is `dot_chart()`.

``` r
library(funbox)
dot_chart(Freq ~ Dept + Gender + Admit, data = UCBAdmissions)
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

Another is a custom gglot2 theme, `theme_lb()`.

``` r
library(ggplot2)
ggplot(mtcars) +
  aes(x = hp, y = mpg, color = factor(carb)) +
  geom_point() +
  labs(color = "Number of\ncarburetors") +
  theme_lb(use_palette = TRUE)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" alt="" width="100%" />
