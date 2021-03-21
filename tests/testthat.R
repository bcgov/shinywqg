# need to manually run the test since it is not a package anymore
# run this script then you can run each script in testthat

library(testthat)
library(magrittr)

load("R/sysdata.rda", envir=.GlobalEnv)
source("R/check_guidelines.R")
source("R/functions_data.R")
source("R/functions_limits.R")
source("R/functions_output.R")
source("R/functions_utils.R")
source("R/utils.R")
source("R/sys.R")
