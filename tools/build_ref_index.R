setwd("C:/Users/saiem/Documents/GitHub-Data/sdv-dev/cfbplotR")
pkgdown::clean_site(force = TRUE)
pkgdown::build_reference_index(pkgdown::as_pkgdown("."))
cat("build_reference_index completed successfully\n")
