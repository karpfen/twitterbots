#define MIN3(a, b, c) \
    ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))

int levenshtein_distance (std::string s1, std::string s2);

float rcpp_adist (Rcpp::StringVector content);
