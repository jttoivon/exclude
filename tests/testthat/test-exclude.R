test_that("exclusion works", {
  df <- tibble::tibble(a=1:2, b=c("first", "second")) %>% 
    init_exclude() %>% 
    dplyr::filter(b=="second") %>% 
    exclude()
  e <- get_exclude()
  edf <- as_tibble(e)
  expect_equal(nrow(df), 1)
  expect_equal(nrow(edf), 2)
})

test_that("exclusion initialization works", {
  old <- push(NULL, "test")   # Store the original object (if it exists)
  withr::defer(push(old, "test"))   # Restore the original object at the end of the function
  df <- tibble::tibble(a=1:2)
  init_exclude(df, "test")
  e <- get_exclude()
  expect_s3_class(e, "exclude")
})

test_that("init_exclude does not modify data", {
  old <- push(NULL, "test")   # Store the original object (if it exists)
  df <- tibble::tibble(x=1:2, y=c("a", "b"))
  withr::defer(push(old, "test"))   # Restore the original object at the end of the function
  expect_equal(init_exclude(df, "test"), df)
})

# Test get_exclude

test_that("get_exclude non-existent works", {
  old <- push(NULL, "non-existing")   # Make sure that the exclusion object does not exist
  withr::defer(push(old, "non-existing"))   # Do this at the end of function
  expect_equal(get_exclude("non-existing"), NULL)
})


test_that("before exclusions no exclusions are reported", {
  #old <- push(NULL, "test")   # Store the original object (if it exists)
  #withr::defer(push(old, "test"))   # Restore the original object at the end of the function
  local_exclude("test")
  df <- tibble::tibble(a=1:2)
  init_exclude(df, "test")
  e <- get_exclude("test")
  df <- tibble::tibble(name="Original data", count=2, diff_count=NA_integer_)
  expect_equal(as_tibble(e), df)
})
