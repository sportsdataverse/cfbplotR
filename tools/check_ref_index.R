setwd("C:/Users/saiem/Documents/GitHub-Data/sdv-dev/cfbplotR")
pkg <- pkgdown::as_pkgdown(".")
topics <- pkg$topics
cat("=== ALL TOPICS pkgdown sees ===\n")
cat(paste(sort(topics$name), collapse = "\n"), "\n")
cat("\n=== Internal/keyword-internal topics ===\n")
cat(paste(sort(topics$name[topics$internal]), collapse = "\n"), "\n")
