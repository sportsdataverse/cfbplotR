test_that("clean_team_abbrs aliases clean_school_names", {
  expect_equal(clean_team_abbrs("Bama"), clean_school_names("Bama"))
})
test_that("cfb_team_factor returns an ordered factor of valid teams", {
  f <- cfb_team_factor(c("Georgia","Alabama"))
  expect_s3_class(f, "factor")
  expect_true(all(levels(f) %in% valid_team_names()))
})
test_that(".cfbplotR_clear_cache runs", {
  expect_invisible(.cfbplotR_clear_cache())
})
