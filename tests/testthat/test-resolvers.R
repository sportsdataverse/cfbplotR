test_that("logo_from_school resolves valid + invalid teams", {
  out <- cfbplotR:::logo_from_school(c("Alabama", "Georgia"))
  expect_type(out, "character")
  expect_length(out, 2)
  expect_false(any(is.na(out)))
  # invalid falls back to the NCAA logo, with a warning
  expect_warning(bad <- cfbplotR:::logo_from_school("Not A Team"))
  expect_equal(bad, cfbplotR:::logo_from_school("NCAA"))
})

test_that("headshot_from_id builds ESPN urls", {
  out <- cfbplotR:::headshot_from_id("4361182")
  expect_match(out, "espncdn\\.com")
})
