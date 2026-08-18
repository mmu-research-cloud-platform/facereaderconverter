test_that("locf", {
  coding_df = read.csv("junk/testdata_detailed.csv") |>
    dplyr::mutate(id = 1, subject = "parent")

  coding = convert_to_episodes(coding_df, delta = 1, consecutive_missing = 150)
  expect_no_error({
    locf(coding = coding)
  })
  expect_no_error({
    locf(coding$coding)
  })

  y = locf(coding)

  expect_all_true(names(y$coding) == names(coding$coding))
  expect_equal(
    c(y$metadata$fps, y$metadata$consecutive_missing),
    c(30, 150)
  )

  y = locf(coding, consecutive_missing = 2)
  expect_equal(
    c(y$metadata$fps, y$metadata$consecutive_missing),
    c(30, 2)
  )
  expect_lte(
    y[["coding"]] |>
      as.data.frame() |>
      dplyr::filter(!is.na(run_id), is.na(value), !in_state) |>
      count(run_id) |>
      pull(n) |>
      max(),
    2
  )
})
