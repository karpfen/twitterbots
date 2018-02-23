#include <Rcpp.h>
#include "rcpp_adist.h"

//' rcpp_adist
//'
//' Calculates the mean Levenshtein distance in relation to the mean length for
//' a list of texts.
//'
//' @param content \code{Rcpp::StringVector} containing the text content.
//'
//' @noRd
// [[Rcpp::export]]
float rcpp_adist (Rcpp::StringVector content)
{
    float mean_dist = 0;
    unsigned int c_size = content.size ();
    unsigned int distances [c_size - 1];
    for (int i = 0; i < c_size - 1; i ++)
    {
        std::string s1 = Rcpp::as <std::string> (content [i]);
        std::string s2 = Rcpp::as <std::string> (content [i + 1]);
        mean_dist += levenshtein_distance (s1, s2);
    }
    mean_dist /= (c_size - 1);

    return mean_dist;
}

//' @noRd
int levenshtein_distance (std::string s1, std::string s2)
{
    unsigned int s1len, s2len, x, y, lastdiag, olddiag;
    s1len = s1.size ();
    s2len = s2.size ();
    unsigned int* steps = new unsigned int [s1len + 1];

    for (y = 1; y <= s1len; y ++)
        steps [y] = y;

    for (x = 1; x <= s2len; x ++)
    {
        steps [0] = x;
        for (y = 1, lastdiag = x - 1; y <= s1len; y ++)
        {
            olddiag = steps [y];
            steps [y] = MIN3 (steps [y] + 1, steps [y - 1] + 1, lastdiag + (s1 [y - 1] == s2 [x - 1] ? 0 : 1));
            lastdiag = olddiag;

        }

    }

    return(steps [s1len]);
}
