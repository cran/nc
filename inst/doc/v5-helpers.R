## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
one.pattern <- function(pat){
  if(is.character(pat)){
    pat
  }else{
    nc::var_args_list(pat)[["pattern"]]
  }
}
show.patterns <- function(...){
  L <- list(...)
  str(lapply(L, one.pattern))
}

## -----------------------------------------------------------------------------
show.patterns(
  "variable: (?<variable>.*)",      #repetitive regex string
  list("variable: ", variable=".*"),#repetitive nc R code
  nc::field("variable", ": ", ".*"))#helper function avoids repetition

## -----------------------------------------------------------------------------
show.patterns(
  "Alignment (?<Alignment>[0-9]+)",
  list("Alignment ", Alignment="[0-9]+"),
  nc::field("Alignment", " ", "[0-9]+"))

## -----------------------------------------------------------------------------
show.patterns(
  "Chromosome:\t+(?<Chromosome>.*)",
  list("Chromosome:\t+", Chromosome=".*"),
  nc::field("Chromosome", ":\t+", ".*"))

## -----------------------------------------------------------------------------
show.patterns(
  "(?:-(?<chromEnd>[0-9]+))?",                #regex string
  list(list("-", chromEnd="[0-9]+"), "?"),    #nc pattern using lists
  nc::quantifier("-", chromEnd="[0-9]+", "?"))#quantifier helper function

## -----------------------------------------------------------------------------
show.patterns(
  "(?: (?<name>[^,}]+))?",
  list(list(" ", name="[^,}]+"), "?"),
  nc::quantifier(" ", name="[^,}]+", "?"))

## -----------------------------------------------------------------------------
show.patterns(
  "(?:(?<first>bar+)|(?<second>fo+))",
  list(first="bar+", "|", second="fo+"),
  nc::alternatives(first="bar+", second="fo+"))

## -----------------------------------------------------------------------------
subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")

## -----------------------------------------------------------------------------
pattern <- nc::alternatives_with_shared_groups(
  month="[a-z]{3}",
  day=list("[0-9]{2}", as.integer),
  year=list("[0-9]{4}", as.integer),
  list(american=list(month, " ", day, ", ", year)),
  list(european=list(day, " ", month, " ", year)))

## -----------------------------------------------------------------------------
(match.dt <- nc::capture_first_vec(subject.vec, pattern))

## -----------------------------------------------------------------------------
Sys.setlocale(locale="C")#to recognize months in English.
match.dt[, date := data.table::as.IDate(
  paste(month, day, year), format="%b %d %Y")]
print(match.dt, class=TRUE)

## -----------------------------------------------------------------------------
nc::capture_first_vec(
  c("Toby Dylan Hocking","Hocking, Toby Dylan"),
  nc::alternatives_with_shared_groups(
    family="[A-Z][a-z]+",
    given="[^,]+",
    list(given_first=list(given, " ", family)),
    list(family_first=list(family, ", ", given))
  )
)

