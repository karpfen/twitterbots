context("basic functionality")
test_that("text_similarity", {

  user_ids <- c (1, 1, 2, 2, 3)
  texts <- c ("a", "a", "a", "b", "a")

  res <- get_text_similarity (user_ids, texts)

  expect_that (all (res$user_id == c (1, 2, 3)), equals (TRUE))
  expect_that (all (res$number_of_tweet == c (2, 2, 1)), equals (TRUE))
  expect_that (all (res$distance [1:2] == c (0, 0.5)), equals (TRUE))
  expect_that (is.na (res$distance [3]), equals (TRUE))

  expect_error (get_text_similarity (1, c (1, 2)),
                "user_ids and content have to be of same length.")
})
