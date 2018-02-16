
library(testthat)
library(infx)

test_check("infx",
           "^(dataset|experiment|material|plate|project|sample|search)$")
