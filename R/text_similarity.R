#' Calculate how similar each user's text messeges are to each other.
#'
#' @param user_ids A \code{vector} of type \code{character}.
#' @param content A \code{vector} of type \code{character} containing the text
#' content of the messages.
#' @param quiet If \code{FALSE}, prints status information.
#'
#' @return A \code{tibble} containing user ids, number of tweets per user and
#' similarity distance.
#'
#' @export
get_text_similarity <- function (user_ids, content, quiet = FALSE)
{
    if (!(is.character (user_ids) & is.vector (user_ids)))
        stop ("user_ids has to be of type character and vector.")
    if (!(is.character (content) & is.vector (content)))
        stop ("content has to be of type character and vector.")
    if (length (user_ids) != length (content))
        stop ("user_ids and content have to be of same length.")
    if (length (user_ids) < 2)
        stop ("user_ids and content have to contain at least 2 elements.")

    unq_user_ids <- unique (user_ids)
    distances <- rep (NA, length (unq_user_ids))
    number_of_tweets <- as.integer (rep (1, length (unq_user_ids)))

    if (!quiet)
    {
        pb = txtProgressBar (min = 0, max = length (distances), initial = 0,
                             style = 3)
    }

    for (i in seq_along (distances))
    {
        if (!quiet)
            setTxtProgressBar(pb, i)
        u_id <- unq_user_ids [i]

        u_text <- (content [which (user_ids == u_id)])
        number_of_tweets [i] <- length (u_text)

        if (length (u_text) > 1)
        {
            text_lengths <- mean (sapply (u_text, nchar))
            text_sims <- mean (rcpp_adist (u_text))
            distances [i] <- text_sims / text_lengths 
        }
    }
    res <- tibble (unq_user_ids, number_of_tweets, distances)
    names (res) <- c ("user_id", "number_of_tweet", "distance")
    res
}
