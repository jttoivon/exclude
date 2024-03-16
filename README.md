
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exclude

<!-- badges: start -->
<!-- badges: end -->

The goal of exclude is to keep track of excluded dataframe rows. It aims
to be non-intrusive and descriptive. Non-intrusive here means that, for
example, the base R `subset()` function or the `dplyr` `filter()`
function don’t need wrappers, and can be called as always. The
exclusions are measured and logged using additional function calls
(`init_exclude()` and `exclude()`). The logging is descriptive in that
user can specify a high-level description of the exclusion to the
`exclude()` function instead of a mechanical description of how the
exclusion was done. For example, the exclusion message can be “exclude
minors” instead of “`filter(age >= 18)`”.

## Installation

You can install the development version of exclude like so:

``` r
devtools::install_github("FRCBS/exclude")
```

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(exclude)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

The init_exclude() call measures the number of rows in the input
dataframe, and the exclude() function measures it again and computes the
difference, and logs the exclusion (with default message “Exclusion”).

``` r
## basic example code
mpg %>% 
  init_exclude() %>%
  filter(manufacturer == "audi") %>%
  exclude()
#> Exclusion: excluded count=216, remaining count=18
#> # A tibble: 18 × 11
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> 11 audi         a4 quattro   2    2008     4 auto… 4        19    27 p     comp…
#> 12 audi         a4 quattro   2.8  1999     6 auto… 4        15    25 p     comp…
#> 13 audi         a4 quattro   2.8  1999     6 manu… 4        17    25 p     comp…
#> 14 audi         a4 quattro   3.1  2008     6 auto… 4        17    25 p     comp…
#> 15 audi         a4 quattro   3.1  2008     6 manu… 4        15    25 p     comp…
#> 16 audi         a6 quattro   2.8  1999     6 auto… 4        15    24 p     mids…
#> 17 audi         a6 quattro   3.1  2008     6 auto… 4        17    25 p     mids…
#> 18 audi         a6 quattro   4.2  2008     8 auto… 4        16    23 p     mids…
e <- get_exclude()
e
#> Original data: excluded count=0, remaining count=234
#> Exclusion: excluded count=216, remaining count=18
```

The exclusion object can be converted to a tibble:

``` r
as_tibble(e)
#> # A tibble: 2 × 3
#>   name          count diff_count
#>   <chr>         <int>      <int>
#> 1 Original data   234         NA
#> 2 Exclusion        18        216
```

You can also plot the object with `plot(e)`, for example:

``` r
plot(e)
```

<!--
Since plot calls DiagrammeR::grViz(), which returns htmlwidget, it cannot be included in an
md file directly. Below I use DiagrammeRsvg::export_svg() to convert htmlwidget to svg,
and then refer to the svg file from the md file. Other solution is to use the webshot2 package:
htmlwidgets::saveWidget(plot(e), "/tmp/x.html")
webshot2::webshot("/tmp/x.html", "/tmp/x.png")
And the refer to the png file from the md file.
-->

<figure>
<img src="man/figures/plot.svg" alt="alt text" />
<figcaption aria-hidden="true">alt text</figcaption>
</figure>

``` r
## basic example code
statistics <- function(df) { list(count = nrow(df), models=n_distinct(df$model)) }
mpg %>% 
  init_exclude(statistics = statistics) %>%
  filter(manufacturer == "audi") %>%
  exclude("Exclude non-Audi") %>%
  filter(cyl < 8) %>%
  exclude("Exclude 8-cylinder cars")
#> Exclude non-Audi: excluded count=216 models=35, remaining count=18 models=3
#> Exclude 8-cylinder cars: excluded count=1 models=0, remaining count=17 models=3
#> # A tibble: 17 × 11
#>    manufacturer model      displ  year   cyl trans drv     cty   hwy fl    class
#>    <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr> <chr>
#>  1 audi         a4           1.8  1999     4 auto… f        18    29 p     comp…
#>  2 audi         a4           1.8  1999     4 manu… f        21    29 p     comp…
#>  3 audi         a4           2    2008     4 manu… f        20    31 p     comp…
#>  4 audi         a4           2    2008     4 auto… f        21    30 p     comp…
#>  5 audi         a4           2.8  1999     6 auto… f        16    26 p     comp…
#>  6 audi         a4           2.8  1999     6 manu… f        18    26 p     comp…
#>  7 audi         a4           3.1  2008     6 auto… f        18    27 p     comp…
#>  8 audi         a4 quattro   1.8  1999     4 manu… 4        18    26 p     comp…
#>  9 audi         a4 quattro   1.8  1999     4 auto… 4        16    25 p     comp…
#> 10 audi         a4 quattro   2    2008     4 manu… 4        20    28 p     comp…
#> 11 audi         a4 quattro   2    2008     4 auto… 4        19    27 p     comp…
#> 12 audi         a4 quattro   2.8  1999     6 auto… 4        15    25 p     comp…
#> 13 audi         a4 quattro   2.8  1999     6 manu… 4        17    25 p     comp…
#> 14 audi         a4 quattro   3.1  2008     6 auto… 4        17    25 p     comp…
#> 15 audi         a4 quattro   3.1  2008     6 manu… 4        15    25 p     comp…
#> 16 audi         a6 quattro   2.8  1999     6 auto… 4        15    24 p     mids…
#> 17 audi         a6 quattro   3.1  2008     6 auto… 4        17    25 p     mids…
e2 <- get_exclude()
e2
#> Original data: excluded count=0 models=0, remaining count=234 models=38
#> Exclude non-Audi: excluded count=216 models=35, remaining count=18 models=3
#> Exclude 8-cylinder cars: excluded count=1 models=0, remaining count=17 models=3
```

``` r
plot(e2)
```

<figure>
<img src="man/figures/plot2.svg" alt="alt text" />
<figcaption aria-hidden="true">alt text</figcaption>
</figure>

In addition to exclusions in a dataframe, exclusions in a vector can be
tracked as well. For that the default statistics has to be modified.

``` r
## basic example code
statistics <- function(v) { list(count = length(v)) }   # Measure the length of the vector
v <- mpg$manufacturer
v <- v %>% 
  init_exclude(statistics = statistics) %>%
  discard(~ str_starts(., "h")) %>%
  exclude("Manufacturer starts with and h letter") 
#> Manufacturer starts with and h letter: excluded count=23, remaining count=211
e <- get_exclude()
e
#> Original data: excluded count=0, remaining count=234
#> Manufacturer starts with and h letter: excluded count=23, remaining count=211
```

Set the option `exclude.print_messages` to `FALSE` to disable exclusion
messages:

``` r
options(exclude.print_messages = FALSE)
```

## Similar packages

[tidylog](https://github.com/elbersb/tidylog)

The package is intrusive: it requires wrappers for each of the `dplyr`
and `tidyr` operations. Does not support base R. Has more abilities than
exclude, such as keeping track added or removed columns. It is not
descriptive: instead of logging what has been filtered, it tells how the
filtering was implemented. Last CRAN release is from 2020-07-03, so it
might not support the latest updates in tidyverse.

[ExclusionTable](https://cran.r-project.org/web/packages/ExclusionTable/index.html)

The used method is quite intrusive and specific to function `subset()`.

[prismadiagramR](https://cran.r-project.org/web/packages/prismadiagramR/index.html)

Only remotely similar to exclude. Has ability to produce exclusion
plots.
