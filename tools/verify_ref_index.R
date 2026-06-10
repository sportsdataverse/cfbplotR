setwd("C:/Users/saiem/Documents/GitHub-Data/sdv-dev/cfbplotR")
pkg <- pkgdown::as_pkgdown(".")
idx <- pkgdown:::data_reference_index(pkg)
cat("reference index OK — all topics covered\n")
