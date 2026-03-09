# 0.3 Functions -----

# Make dummies variables function
make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(gsub(".*\\$", "", deparse(substitute(v))), prefix, s)
  d
}
