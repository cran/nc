## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
pos.vec <- c(
  "chr10:213,054,000-213,055,000",
  "chrM:111,000",
  "chr1:110-111 chr2:220-222") # two possible matches.

## -----------------------------------------------------------------------------
(chr.dt <- nc::capture_first_vec(
  pos.vec, 
  chrom="chr.*?",
  ":",
  chromStart="[0-9,]+"))
str(chr.dt)

## -----------------------------------------------------------------------------
keep.digits <- function(x)as.integer(gsub("[^0-9]", "", x))
(int.dt <- nc::capture_first_vec(
  pos.vec, 
  chrom="chr.*?",
  ":",
  chromStart="[0-9,]+", keep.digits))
str(int.dt)

## -----------------------------------------------------------------------------
pos.pattern <- list("[0-9,]+", keep.digits)
range.pattern <- list(
  chrom="chr.*?",
  ":",
  chromStart=pos.pattern,
  list(
    "-",
    chromEnd=pos.pattern
  ), "?")
nc::capture_first_vec(pos.vec, range.pattern)

## -----------------------------------------------------------------------------
nc::var_args_list(range.pattern)

## -----------------------------------------------------------------------------
nc::capture_first_vec(bad.vec, range.pattern, nomatch.error=FALSE)

## -----------------------------------------------------------------------------
(sacct.df <- data.frame(
  Elapsed = c(
    "07:04:42", "07:04:42", "07:04:49",
    "00:00:00", "00:00:00"),
  JobID=c(
    "13937810_25",
    "13937810_25.batch",
    "13937810_25.extern",
    "14022192_[1-3]",
    "14022204_[4]"),
  stringsAsFactors=FALSE))

## -----------------------------------------------------------------------------
int.pattern <- list("[0-9]+", as.integer)
range.pattern <- list(
  "\\[",
  task1=int.pattern,
  list(
    "-",#begin optional end of range.
    taskN=int.pattern
  ), "?", #end is optional.
  "\\]")
nc::capture_first_df(sacct.df, JobID=range.pattern, nomatch.error=FALSE)

## -----------------------------------------------------------------------------
task.pattern <- list(
  "_",
  list(
    task=int.pattern,
    "|",#either one task(above) or range(below)
    range.pattern))
nc::capture_first_df(sacct.df, JobID=task.pattern)

## -----------------------------------------------------------------------------
job.pattern <- list(
  job=int.pattern,
  task.pattern,
  list(
    "[.]",
    type=".*"
  ), "?")
nc::capture_first_df(sacct.df, JobID=job.pattern)

## -----------------------------------------------------------------------------
elapsed.pattern <- list(
  hours=int.pattern,
  ":",
  minutes=int.pattern,
  ":",
  seconds=int.pattern)
nc::capture_first_df(sacct.df, JobID=job.pattern, Elapsed=elapsed.pattern)

## -----------------------------------------------------------------------------
fsa.vec <- c(#control samples:
  "A01-Ladder-PP16-001.10sec.fsa",
  "D07_Ladder-IP_004.5sec.fsa",
  "A12_RB121514ADG_001.10sec.fsa", 
  "A10_RB102191515LEA-IP_001.10sec.fsa",
  ##single-source samples:
  "A02-RD12-0002-35-0.5PP16-001.10sec.fsa", 
  "G01_RD14-0003-35d3S30-0.01563P-Q10.0_003.10sec.fsa",
  "A06_RD14-0003-24d3a-0.0625IP-Q0.8_001.10sec.fsa", 
  "A08-RD12-0002-01d-0.125PP16-001.10sec.fsa",
  "A10-RD12-0002-04d1-0.0625PP16-001.10sec.fsa", 
  "C02_RD14-0003-15d2b-0.25IP-Q0.5_003.5sec.fsa",
  ##mixture samples:
  "A02-RD12-0002-1_2-1;9-0.125PP16-001.10sec.fsa", 
  "H07_RD14-0003-35_36_37_38_39-1;4;4;4;1-M2I35-0.75IP-QLAND_004.5sec.fsa")

## -----------------------------------------------------------------------------
well.pattern <- list(
  "^",
  well.letter="[A-H]",
  well.number="[0-9]+", as.integer)
nc::capture_first_vec(fsa.vec, well.pattern)

## -----------------------------------------------------------------------------
end.pattern <- list(
  "[-_]",
  capillary="[0-9]+", as.integer,
  "[.]",
  seconds="[0-9]+", as.integer,
  "sec[.]fsa$")
nc::capture_first_vec(fsa.vec, end.pattern)

## -----------------------------------------------------------------------------
between.dt <- nc::capture_first_vec(
  fsa.vec, well.pattern, between=".*?", end.pattern)
between.dt$between

## -----------------------------------------------------------------------------
mass.pattern <- list(
  template.nanograms="[0-9.]*", as.numeric)
kit.pattern <- nc::quantifier(
  kit=nc::alternatives("PP16", "IP", "P"),
  "?")
q.pattern <- nc::quantifier(
  "-Q",
  Q.chr=nc::alternatives("LAND", "[0-9.]+"),
  "?")
old.opt <- options(width=100)
(before.dt <- nc::capture_first_vec(
  between.dt$between,
  "^",
  before=".*?",
  mass.pattern, kit.pattern, q.pattern, "$"))
options(old.opt)

## -----------------------------------------------------------------------------
project.pattern <- list(project="RD[0-9]+-[0-9]+", "-")
single.pattern <- list(id="[0-9]+", as.integer)
mixture.pattern <- list(
  ids.chr="[0-9_]+",
  "-",
  parts.chr="[0-9;]+",
  "-")
single.or.mixture <- list(
  project.pattern,
  nc::alternatives(mixture.pattern, single.pattern))
control.pattern <- list(control="[^-_]+")
single.mixture.control <- list("[-_]", nc::alternatives(
  single.or.mixture, control.pattern))
(rest.dt <- nc::capture_first_vec(
  before.dt$before, single.mixture.control, rest=".*"))

## -----------------------------------------------------------------------------
dilution.pattern <- nc::quantifier(
  "[dM]",
  dilution.number="[0-9]?", as.integer,
  "?")
treatment.pattern <- nc::quantifier(
  treatment.letter="[a-zA-Z]",
  nc::quantifier(treatment.number="[0-9]+", as.integer, "?"),
  "?")
dilution.treatment <- list(
  dilution.pattern,
  treatment.pattern,
  "[_-]?")
nc::capture_first_vec(rest.dt$rest, "^", dilution.treatment, "$")

## -----------------------------------------------------------------------------
fsa.pattern <- list(
  well.pattern,
  single.mixture.control,
  dilution.treatment,
  mass.pattern, kit.pattern, q.pattern,  
  end.pattern)
(match.dt <- nc::capture_first_vec(fsa.vec, fsa.pattern))

## -----------------------------------------------------------------------------
disp.dt <- data.table::data.table(match.dt)
names(disp.dt) <- paste(1:ncol(disp.dt))
old.opt <- options("datatable.print.colnames"="none")
split(disp.dt, fsa.vec)
options(old.opt)

## -----------------------------------------------------------------------------
nc::var_args_list(fsa.pattern)$pattern

