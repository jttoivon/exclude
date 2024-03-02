test_that("exclusion works", {
  df <- tibble::tibble(a=1:2, b=c("first", "second")) %>% init_exclude() %>% dplyr::filter(b=="second") %>% exclude()
  e <- get_exclude()
  edf <- as_tibble(e)
  expect_equal(nrow(df), 1)
  expect_equal(nrow(edf), 2)
})
