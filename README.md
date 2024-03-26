
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinywqg

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![R-CMD-check](https://github.com/bcgov/shinywqg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcgov/shinywqg/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bcgov/shinywqg/branch/master/graph/badge.svg)](https://codecov.io/gh/bcgov/shinywqg?branch=master)
[![Apache
license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

shinywqg is a Shiny application to run the ’B.C. Ambient Water Quality
Guidelines. Guidelines are determined based on information from the BC
Data Catalogue.

The shiny application is also available at
<https://bcgov-env.shinyapps.io/bc_wqg/>.

## Usage

### Installation

To install the developmental version from
[GitHub](https://github.com/bcgov/shinywqg)

``` r
# install.packages("remotes")
remotes::install_github("bcgov/shinywqg")
```

### Demonstration

``` r
shinywqg::run_wqg_app()
```

## Internal Info

To update the app and data, the process is as follows:

1.  [Test the app against updated data
    locally](#how-to-test-changes-to-water-quality-guidelines-data)
2.  Upload the new data to the BC Data Catalogue
3.  [Update the internal backup data in the
    app](#how-to-update-the-internal-backups)
4.  [Deploy the updated app](#deployment)

### How to Test Changes to Water Quality Guidelines Data

First, ensure you have an up-to-date local copy of the repository, by
pulling from GitHub (make sure you are on the master branch).

When you make changes to the water guidelines you can check that they
don’t break the app before you upload the new version to the BC Data
Catalogue. To do this make minor modifications in the `R/mod_data.R`
script to read in the new data. You will need to add the new data to the
`inst/app` folder.

``` r
mod_data_server <- function(input, output, session) {
  ns <- session$ns
  observe({
    
    ### comment out these lines
    # file_name <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"
    # limits <- get_data(file_name)
    
    ### add this line
    limits <- readr::read_csv("all_wqgs-2.csv")
    
    
})}
```

Make the required changes to the scripts in the `R` folder (like
described above). Then open the `app.R` script which is located in the
root folder and click “run” in the toolbar. Or, in the console run:

``` r
pkgload::load_all(".")
shinywqg::run_wqg_app()
```

This should launch the app with your changes implemented.

### How to Update the Internal Backups

Once a new version of the guidelines have been added to the BC Data
Catalogue you will need to update the internal backup data.

- Open `data-raw/internal.R`
- Run the script from the top to bottom
- Deploy the app

### Deployment

Open the `scripts/deploy.R` file. To check that everything is fully
working run the test version first. If all looks good, run the public
one. Then commit and push the updated
`rsconnect/shinyapps.io/bcgov-env/BC_WQG.dcf` file to GitHub.

### How to Add New Lookup Tables

Obtain the unique ID for downloading the data from the BC Data Catalogue
once it has been added to the BC Data Catalogue. Add this ID to the
Limit column of the Water Quality Guidelines. The app should
automatically add a lookup function for that chemical/use/media.

You will need to manually add it to the internal backup data. Open
`data-raw/internal.R` add one line assigning the ID to a variable and
then add that variable to the lookup hash vector.

``` r
# example
hash_ni_chronic <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"

# update this line with the new hash name 
lookup_hash <- c(hash_cu_chronic, hash_cu_acute, hash_ni_chronic)
```

#### How to Find Unique Hash/ID for an Item on the BC Data Catalogue

There is more then one way to find the unique hash. Here are few ways.

1.  Look at the url of the item in the BC Data Catalogue website. The
    random coding at the end of the url is the unique hash needed. For
    the example below the unique hash is
    `85d3990a-ec0a-4436-8ebd-150de3ba0747`

Example:
“<https://catalogue.data.gov.bc.ca/dataset/85d3990a-ec0a-4436-8ebd-150de3ba0747>”

2.  Using the `bcdata` R package

``` r
library(bcdata)

# this list all the potential items
bcdc_list()

# find the name of the file and put it in the `bcdc_browse` function 
bcdc_search("water-quality-guidelines-of-b-c-")

# read the output to get the ID for the record
```

## Contribution

Please report any [issues](https://github.com/bcgov/shinywqg/issues).

[Pull requests](https://github.com/bcgov/shinywqg/pulls) are always
welcome.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/bcgov/shinywqg/blob/master/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
