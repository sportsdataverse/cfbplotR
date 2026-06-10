setwd("C:/Users/saiem/Documents/GitHub-Data/sdv-dev/cfbplotR")
pkg <- pkgdown::as_pkgdown(".")
topics <- pkg$topics
cat("=== Non-internal topics that must be in reference index ===\n")
cat(paste(sort(topics$name[!topics$internal]), collapse = "\n"), "\n")
