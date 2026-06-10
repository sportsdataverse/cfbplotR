test_that("geom_cfb_wordmarks renders", {
  skip_on_ci(); skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(a = 1:2, b = 1, teams = c("Oregon", "UCLA"))
  p <- ggplot(df, aes(a, b)) + geom_cfb_wordmarks(aes(team = teams), width = 0.2) + theme_void()
  vdiffr::expect_doppelganger("geom_cfb_wordmarks-basic", p)
})
