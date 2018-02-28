context("basic functionality")
test_that("text_similarity", {

  user_ids <- c ("1", "1", "2", "2", "3")
  texts <- c ("a", "a", "phone", "phony", "a")

  res <- get_text_similarity (user_ids, texts)

  expect_that (all (res$user_id == c (1, 2, 3)), equals (TRUE))
  expect_that (all (res$number_of_tweets == c (2, 2, 1)), equals (TRUE))
  expect_that (all (res$distance [1:2] == c (0, 0.2)), equals (TRUE))
  expect_that (is.na (res$distance [3]), equals (TRUE))

  expect_error (get_text_similarity (user_ids [1], texts [1:2]),
                "user_ids and content have to be of same length.")
  expect_error (get_text_similarity (c (1, 2), texts [1:2]),
                "user_ids has to be of type character and vector.")
  expect_error (get_text_similarity (user_ids [1:2], c (1, 2)),
                "content has to be of type character and vector.")
  expect_error (get_text_similarity (user_ids [1], texts [1]),
                "user_ids and content have to contain at least 2 elements.")
})

test_that("temporal_indicators", {

              user_ids <- c ("1", "1", "1", "2", "2")
              times <- c ("2000-01-01 00:00:00", "2000-01-01 10:00:00",
                          "2000-01-02 00:00:00", "2000-01-01 00:00:00",
                          "2000-01-02 00:00:00")

              res <- get_tweets_per_day (user_ids, times)

              expect_that (all (res$user_id == c (1, 2)), equals (TRUE))
              expect_that (all (res$number_of_tweets == c (3, 2)), equals (TRUE))
              expect_that (all (res$tweets_per_day == c (3, 2)), equals (TRUE))

              expect_error (get_tweets_per_day (user_ids [1], times [1:2]),
                            "user_ids and times have to be of same length.")
              expect_error (get_tweets_per_day (c (1, 2), times [1:2]),
                            "user_ids has to be of type character and vector.")
              expect_error (get_tweets_per_day (user_ids [1:2], c (1, 2)),
                            "times has to be of type character and vector.")
              expect_error (get_tweets_per_day (user_ids [1], times [1]),
                            "user_ids and times have to contain at least 2 elements.")
})
