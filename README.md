
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinywqg

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![R-CMD-check](https://github.com/poissonconsulting/dbflobr/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/dbflobr/actions)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/dbflobr/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/dbflobr?branch=master)
[![Apache
license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

shinywqg is an R package to run the ‘B.C. Ambient Water Quality
Guidelines’ Shiny application. Guidelines are determined based on
information from the BC Data Catalogue.

The shiny application is also available at
<https://bcgov-env.shinyapps.io/bc_wqg/>.

## Installation

To install the developmental version from
[GitHub](https://github.com/bcgov/shinywqg)

``` r
# install.packages("remotes")
remotes::install_github("bcgov/shinywqg")
```

## Demonstration

``` r
shinywqg::run_wqg_app()
```

## How to Test Changes to Water Quality Guidelines Data

If you would like to test changes of the guidelines on the app before
deploying. You can make minor modifications in the `R/mod_data.R` script
to read in the new data. Then run the app locally to ensure it is
working before deploying. You will need to add the new csv to `inst/app`
folder.

``` r
mod_data_server <- function(input, output, session) {
  ns <- session$ns
  observe({
    
    ### comment out the line before
    # file_name <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"
    # limits <- get_data(file_name)
    
    ### add this line
    limits <- readr::read_csv("all_wqgs-2.csv")
    
    
})}
```

## To Test Changes to the App

Make the required changes to the scripts in the `R` folder (like
described above). Open the `app.R` script in the root folder, and run
the code. This should launch the app with your changes implemented.

## How to Update the Internal Backups

Once a new version of the guidelines have been added to the BC Data
Catalogue you will need to update the internal backup data.

-   Open `data-raw/internal.R`
-   Run the script from the top to bottom
-   Deploy the app

## How to Add New Lookup Tables

Obtain the unique hash for the new lookup once it has been added to the
BC Data Catalogue. Add this hash to the Limit column of the Water
Quality Guidelines. The app should automatically add a lookup function
for that chemical/use/media.

You will need to manually add it to the internal backup data. Open
`data-raw/internal.R` add a line to the script for the new lookup.

``` r
# example
hash_ni_chronic <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"

# update this line with the new hash name 
lookup_hash <- c(hash_cu_chronic, hash_cu_acute, hash_ni_chronic)
```

### How to Find Unique Hash for Item on BC Data Cataloguee

There is more then one way to find the unique hash. Here are few ways.

1.  Look at the url of the item in the BC Data Catalogue website. The
    random coding at the end of the url is the unique hash needed. For
    the example below the unique hash is
    `85d3990a-ec0a-4436-8ebd-150de3ba0747`

Example:
“<https://catalogue.data.gov.bc.ca/dataset/85d3990a-ec0a-4436-8ebd-150de3ba0747>”

1.  Using the `bcdata` R package

``` r
library(bcdata)

# this list all the potential items
bcdc_list()

# find the name of the file and put it in the `bcdc_browse` function 
bcdc_search("water-quality-guidelines-of-b-c-")

# the ID listed is the unique hash you need 
```

## Contribution

Please report any [issues](https://github.com/bcgov/shinywqg/issues).

[Pull requests](https://github.com/bcgov/shinywqg/pulls) are always
welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/bcgov/shinywqg/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
