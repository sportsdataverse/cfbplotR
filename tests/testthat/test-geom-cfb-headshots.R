test_that("geom_cfb_headshots renders", {
  skip_on_cran(); skip_if_offline(); skip_if_not_installed("vdiffr")
  library(ggplot2)
  df <- data.frame(a = 1:2, b = 1, id = c("4361182", "4426385"))
  p <- ggplot(df, aes(a, b)) + geom_cfb_headshots(aes(player_id = id), width = 0.15) + theme_void()
  vdiffr::expect_doppelganger("geom_cfb_headshots-basic", p)
})
