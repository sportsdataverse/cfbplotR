test_that("gt_fmt_cfb_logo embeds an <img>", {
  skip_if_not_installed("gt")
  tab <- gt::gt(data.frame(team = c("Alabama", "Georgia")))
  out <- gt_fmt_cfb_logo(tab, columns = "team")
  html <- gt::as_raw_html(out)
  expect_match(html, "<img")
})
