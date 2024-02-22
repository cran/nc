## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
nc::capture_first_vec(
  names(iris), 
  part = ".*", "[.]", dim = ".*", 
  engine = "ICU", nomatch.error = FALSE)

## -----------------------------------------------------------------------------
str(compiled <- nc::var_args_list(part = ".*", "[.]", dim = ".*"))

## -----------------------------------------------------------------------------
m <- stringi::stri_match_first_regex(names(iris), compiled$pattern)
colnames(m) <- c("match", names(compiled$fun.list))
m

