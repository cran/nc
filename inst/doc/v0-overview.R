## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
subject.vec <- c(
  "chr10:213054000-213,055,000",
  "chrM:111000",
  "chr1:110-111 chr2:220-222")
nc::capture_first_vec(
  subject.vec, chrom="chr.*?", ":", chromStart="[0-9,]+", as.integer)

## -----------------------------------------------------------------------------
library(data.table)
subject.dt <- data.table(
  JobID = c("13937810_25", "14022192_1"),
  Elapsed = c("07:04:42", "07:04:49"))
int.pat <- list("[0-9]+", as.integer)
nc::capture_first_df(
  subject.dt,
  JobID=list(job=int.pat, "_", task=int.pat),
  Elapsed=list(hours=int.pat, ":", minutes=int.pat, ":", seconds=int.pat))

## -----------------------------------------------------------------------------
nc::capture_all_str(
  subject.vec, chrom="chr.*?", ":", chromStart="[0-9,]+", as.integer)

## -----------------------------------------------------------------------------
(one.iris <- data.frame(iris[1,]))
nc::capture_melt_single  (one.iris, part  =".*", "[.]", dim   =".*")
nc::capture_melt_multiple(one.iris, column=".*", "[.]", dim   =".*")
nc::capture_melt_multiple(one.iris, part  =".*", "[.]", column=".*")

## -----------------------------------------------------------------------------
dir.create(iris.dir <- tempfile())
icsv <- function(sp)file.path(iris.dir, paste0(sp, ".csv"))
data.table(iris)[, fwrite(.SD, icsv(Species)), by=Species]
dir(iris.dir)

## -----------------------------------------------------------------------------
nc::capture_first_glob(file.path(iris.dir,"*.csv"), Species="[^/]+", "[.]csv")

## -----------------------------------------------------------------------------
subject.vec <- c("sex_child1", "age_child1", "sex_child2")
pattern <- list(
  variable="age|sex", "_",
  nc::field("child", "", "[12]", as.integer))
nc::capture_first_vec(subject.vec, pattern)

## -----------------------------------------------------------------------------
subject.vec <- c("mar 17, 1983", "26 sep 2017", "17 mar 1984")
pattern <- nc::alternatives_with_shared_groups(
  month="[a-z]{3}", day="[0-9]{2}", year="[0-9]{4}",
  list(month, " ", day, ", ", year),
  list(day, " ", month, " ", year))
nc::capture_first_vec(subject.vec, pattern)

