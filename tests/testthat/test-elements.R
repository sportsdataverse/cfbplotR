test_that("element_cfb_logo axis renders", {
  skip_on_cran(); skip_if_offline(); skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(teams = c("Alabama","Georgia","Oregon"), v = c(1,2,3))
  p <- ggplot(df, aes(teams, v)) + geom_col() + theme_minimal() +
    theme(axis.text.x = element_cfb_logo())
  vdiffr::expect_doppelganger("element_cfb_logo-axis", p)
})
