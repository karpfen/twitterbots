#' Calculate how similar each user's text messeges are to each other.
#'
#' @param user_ids A \code{vector} of type \code{character}.
#' @param times A \code{vector} of type \code{character} containing the time
#' stamps of the messages.
#' @param quiet If \code{FALSE}, prints status information.
#'
#' @return A \code{tibble} containing a user id and the number of tweets per day
#' for each user.
#'
#' @export
get_tweets_per_day <- function (user_ids, times, quiet = FALSE)
{
    if (!(is.character (user_ids) & is.vector (user_ids)))
        stop ("user_ids has to be of type character and vector.")
    if (!(is.character (times) & is.vector (times)))
        stop ("times has to be of type character and vector.")
    if (length (user_ids) != length (times))
        stop ("user_ids and times have to be of same length.")
    if (length (user_ids) < 2)
        stop ("user_ids and times have to contain at least 2 elements.")

    unq_user_ids <- unique (user_ids)
    tweets_per_day <- rep (NA, length (unq_user_ids))
    number_of_tweets <- as.integer (rep (1, length (unq_user_ids)))

    if (!quiet)
    {
        pb = txtProgressBar (min = 0, max = length (tweets_per_day),
                             initial = 0, style = 3)
    }

    for (i in seq_along (tweets_per_day))
    {
        if (!quiet)
            setTxtProgressBar(pb, i)
        u_id <- unq_user_ids [i]

        u_times <- as.POSIXct ((times [which (user_ids == u_id)]))
        number_of_tweets [i] <- length (u_times)
        if (length (u_times) > 1)
        {
            mxmn <- range (u_times)
            ddiff <- difftime (mxmn [1], mxmn [2], units = "days") %>%
                abs %>% as.numeric
            tweets_per_day [i] <- number_of_tweets [i] / ddiff
        }
    }
    res <- tibble (unq_user_ids, number_of_tweets, tweets_per_day)
    names (res) <- c ("user_id", "number_of_tweets", "tweets_per_day")
    res
}
