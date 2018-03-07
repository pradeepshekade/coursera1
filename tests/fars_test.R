library(testthat)
source("../R/fars_functions.R")
expect_match("accident_2015.csv.bz2", make_filename(2015))
expect_match("accident_2013.csv.bz2", make_filename(2013))


