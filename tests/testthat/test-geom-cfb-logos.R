test_that("geom_cfb_logos renders", {
  skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(a = 1:3, b = 1, teams = c("Alabama", "Georgia", "Oregon"))
  p <- ggplot(df, aes(a, b)) + geom_cfb_logos(aes(team = teams), width = 0.1) + theme_void()
  vdiffr::expect_doppelganger("geom_cfb_logos-basic", p)
})
