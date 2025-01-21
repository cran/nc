## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

library(data.table)
data.table(iris)


## -----------------------------------------------------------------------------

iris.parts <- list(
  nc=nc::capture_melt_multiple(
    iris,
    column=".*?",
    "[.]",
    dim=".*"),
  tidyr=if(requireNamespace("tidyr"))tidyr::pivot_longer(
    iris, 
    cols=1:4, 
    names_to=c(".value", "dim"),
    names_sep="[.]"),
  stats=stats::reshape(
    iris,
    direction="long",
    timevar="dim",
    varying=1:4,
    sep="."),
  "data.table::melt"=melt(
    data.table(iris),
    measure.vars=patterns(
      Sepal="^Sepal",
      Petal="^Petal")
  )[data.table(
    variable=factor(1:2), dim=c("Length", "Width")
  ), on=.(variable)],
  if(requireNamespace("cdata"))cdata::rowrecs_to_blocks(
    iris,
    controlTable=data.frame(
      dim=c("Length", "Width"),
      Petal=c("Petal.Length", "Petal.Width"),
      Sepal=c("Sepal.Length", "Sepal.Width"),
      stringsAsFactors=FALSE),
    columnsToCopy="Species"))
iris.parts$nc


## -----------------------------------------------------------------------------

if(require(ggplot2)){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(dim ~ Species)+
    coord_equal()+
    geom_abline(slope=1, intercept=0, color="grey")+
    geom_point(aes(
      Petal, Sepal),
      data=iris.parts$nc)
}


## -----------------------------------------------------------------------------

iris.dims <- list(
  nc=nc::capture_melt_multiple(
    iris,
    part=".*?",
    "[.]",
    column=".*"),
  stats=stats::reshape(
    structure(iris, names=sub("(.*?)[.](.*)", "\\2.\\1", names(iris))),
    direction="long",
    timevar="part",
    varying=1:4,
    sep="."))
iris.dims$nc


## -----------------------------------------------------------------------------

if(require(ggplot2)){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(part ~ Species)+
    coord_equal()+
    geom_abline(slope=1, intercept=0, color="grey")+
    geom_point(aes(
      Length, Width),
      data=iris.dims$nc)
}


## -----------------------------------------------------------------------------

TC <- data.table::data.table(
  age.treatment=c(1, 5),
  sex.control=c("M", "M"),
  sex.treatment=c("F", "F"),
  age.control=c(10, 50))


## -----------------------------------------------------------------------------

input.list <- list(
  "nc"=nc::capture_melt_multiple(
    TC,
    column=".*?",
    "[.]",
    group=".*"),
  "cdata"=if(requireNamespace("cdata"))cdata::rowrecs_to_blocks(
    TC,
    controlTable=data.frame(
      group=c("treatment", "control"),
      age=c("age.treatment", "age.control"),
      sex=c("sex.treatment", "sex.control"),
      stringsAsFactors=FALSE)),
  "data.table"=data.table::melt(TC, measure.vars=patterns(
    age="age",
    sex="sex")),
  "stats"=stats::reshape(
    TC,
    varying=1:4,
    direction="long"),
  "tidyr"=if(requireNamespace("tidyr"))tidyr::pivot_longer(
    TC, 1:4,
    names_to=c(".value", "group"),
    names_sep="[.]"))
output.list <- list()
for(pkg in names(input.list)){
  df.or.null <- input.list[[pkg]]
  if(is.data.frame(df.or.null)){
    output.list[[pkg]] <- data.table::data.table(df.or.null)[order(age)]
  }
}
output.list
sapply(output.list, function(DT)identical(DT$sex, c("F", "F", "M", "M")))


## -----------------------------------------------------------------------------

if(requireNamespace("tidyr")){
  data(who, package="tidyr")
}else{
  who <- data.frame(id=1, new_sp_m5564=2, newrel_f65=3)
}
names(who)


## -----------------------------------------------------------------------------

who.chr.list <- list(
  nc=nc::capture_melt_single(
    who,
    "new_?",
    diagnosis=".*",
    "_",
    gender=".",
    ages=".*"),
  tidyr=if(requireNamespace("tidyr"))tidyr::pivot_longer(
    who,
    new_sp_m014:newrel_f65,
    names_to=c("diagnosis", "gender", "ages"),
    names_pattern="new_?(.*)_(.)(.*)"))


## -----------------------------------------------------------------------------

who.pattern <- "new_?(.*)_(.)((0|[0-9]{2})([0-9]{0,2}))"
as.numeric.Inf <- function(y)ifelse(y=="", Inf, as.numeric(y))
who.typed.list <- list(
  nc=nc::capture_melt_single(
    who,
    "new_?",
    diagnosis=".*",
    "_",
    gender=".",
    ages=list(
      ymin.num="0|[0-9]{2}", as.numeric,
      ymax.num="[0-9]{0,2}", as.numeric.Inf),
    value.name="count",
    na.rm=TRUE),
  tidyr=if(requireNamespace("tidyr"))try(tidyr::pivot_longer(
    who,
    cols=grep(who.pattern, names(who)),
    names_transform=list(
      ymin.num=as.numeric,
      ymax.num=as.numeric.Inf),
    names_to=c("diagnosis", "gender", "ages", "ymin.num", "ymax.num"),
    names_pattern=who.pattern,
    values_drop_na=TRUE,
    values_to="count")))
str(who.typed.list)


## -----------------------------------------------------------------------------

if(requireNamespace("tidyr")){
  gather.result <- tidyr::gather(
    who,
    "variable",
    "count",
    grep(who.pattern, names(who)),
    na.rm=TRUE)
  extract.result <- tidyr::extract(
    gather.result,
    "variable",
    c("diagnosis", "gender", "ages", "ymin.int", "ymax.int"),
    who.pattern,
    convert=TRUE)
  transform.result <- base::transform(
    extract.result,
    ymin.num=as.numeric(ymin.int),
    ymax.num=ifelse(is.na(ymax.int), Inf, as.numeric(ymax.int)))
  str(transform.result)
}


## -----------------------------------------------------------------------------

reshape2.result <- if(requireNamespace("reshape2")){
  reshape2:::melt.data.frame(
    who,
    measure.vars=grep(who.pattern, names(who)),
    na.rm=TRUE,
    value.name="count")
}


## -----------------------------------------------------------------------------

dt.result <- data.table::melt.data.table(
  data.table(who),
  measure.vars=patterns(who.pattern),
  na.rm=TRUE,
  value.name="count")


## -----------------------------------------------------------------------------

who.df <- data.frame(who)
is.varying <- grepl(who.pattern, names(who))
names(who.df)[is.varying] <- paste0("count.", names(who)[is.varying])
stats.result <- stats::reshape(
  who.df,
  direction="long",
  timevar="variable",
  varying=is.varying)


## -----------------------------------------------------------------------------

if(requireNamespace("cdata")){
  cdata.result <- cdata::rowrecs_to_blocks(
    who, 
    cdata::build_unpivot_control(
      "variable",
      "count",
      grep(who.pattern, names(who), value=TRUE)),
    columnsToCopy=grep(who.pattern, names(who), value=TRUE, invert=TRUE))
}


## -----------------------------------------------------------------------------

## Example 1: melting a wider iris data back to original.
library(data.table)
iris.dt <- data.table(
  i=1:nrow(iris),
  iris[,1:4],
  Species=paste(iris$Species))
print(iris.dt)

## what if we had two observations on each row?
set.seed(1)
iris.rand <- iris.dt[sample(.N)]
iris.wide <- cbind(treatment=iris.rand[1:75], control=iris.rand[76:150])
print(iris.wide, topn=2, nrows=10)

## This is the usual data.table syntax for getting the original iris back.
iris.melted <- melt(iris.wide, value.factor=TRUE, measure.vars = patterns(
  i="i$",
  Sepal.Length="Sepal.Length$",
  Sepal.Width="Sepal.Width$",
  Petal.Length="Petal.Length$",
  Petal.Width="Petal.Width$",
  Species="Species$"))
identical(iris.melted[order(i), names(iris.dt), with=FALSE], iris.dt)

## nc can do the same thing -- you must define an R argument named
## column, and another named argument which identifies each group.
(nc.melted <- nc::capture_melt_multiple(
  iris.wide,
  group="[^.]+",
  "[.]",
  column=".*"))
identical(nc.melted[order(i), names(iris.dt), with=FALSE], iris.dt)

## This is how we do it using stats::reshape.
iris.wide.df <- data.frame(iris.wide)
names(iris.wide.df) <- sub("(.*?)[.](.*)", "\\2_\\1", names(iris.wide))
iris.reshaped <- stats::reshape(
  iris.wide.df,
  direction="long",
  timevar="group",
  varying=names(iris.wide.df),
  sep="_")
identical(data.table(iris.reshaped[, names(iris.dt)])[order(i)], iris.dt)

## get the parts columns and groups -- is there any difference
## between groups? of course not!
parts.wide <- nc::capture_melt_multiple(
  iris.wide,
  group=".*?",
  "[.]",
  column=".*?",
  "[.]",
  dim=".*")
if(require("ggplot2")){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(dim ~ group)+
    coord_equal()+
    geom_abline(slope=1, intercept=0, color="grey")+
    geom_point(aes(
      Petal, Sepal),
      data=parts.wide)
}


## -----------------------------------------------------------------------------

## Example 2. Lots of column types, from example(melt.data.table).
DT <- data.table(
  i_1 = c(1:5, NA),
  i_2 = c(NA,6:10),
  f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
  c_1 = sample(c(letters[1:3], NA), 6, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
  d_2 = as.Date(6:1, origin="2012-01-01"))
## add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]

## original DT syntax is quite repetitive.
melt(DT, measure=patterns(
  i="^i",
  f="^f",
  d="^d",
  l="^l"
))

## nc syntax uses a single regex rather than four.
nc::capture_melt_multiple(
  DT,
  column="^[^c]",
  "_",
  number="[12]")

## id.vars can be specified using original DT syntax.
melt(DT, id=1:2, measure=patterns(
  f="^f",
  l="^l"
))

nc::capture_melt_multiple(
  DT,
  column="^[fl]",
  "_",
  number="[12]")

## reshape does not support list columns.
reshape(
  DT,
  varying=grep("^[fid]", names(DT)),
  sep="_",
  direction="long",
  timevar="number")

## tidyr does, but errors for combining ordered and un-ordered factors.
if(requireNamespace("tidyr")){
  tidyr::pivot_longer(
    DT, grep("[cf]", names(DT), invert=TRUE),
    names_pattern="(.)_(.)",
    names_to=c(".value", "number"))
}


## -----------------------------------------------------------------------------

## Example 3, three children, one family per row, from data.table
## vignette.
family.dt <- fread(text="
family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")
(children.melt <- melt(family.dt, measure = patterns(
  dob="^dob", gender="^gender"
), na.rm=TRUE, variable.factor=FALSE))

## nc::field can be used to define group name and pattern at the
## same time, to avoid repetitive code.
(children.nc <- nc::capture_melt_multiple(
  family.dt,
  column="[^_]+",
  "_",
  nc::field("child", "", "[1-3]"),
  na.rm=TRUE))

## reshape works too.
stats::reshape(
  family.dt,
  varying=grep("child", names(family.dt)),
  direction="long",
  sep="_",
  timevar="child.str")


## -----------------------------------------------------------------------------

## Comparison with base R. 1. mfrow means parts on rows, mfcol means
## parts on columns. 2. same number of lines of code. 3. nc/ggplot2
## code has more names and fewer numbers.
imat <- as.matrix(iris[, 1:4])
ylim <- range(table(imat))
xlim <- range(imat)
old.par <- par(mfcol=c(2,2), mar=c(2,2,1,1))
for(col.i in 1:ncol(imat)){
  hist(
    imat[, col.i],
    breaks=seq(xlim[1], xlim[2], by=0.1),
    ylim=ylim,
    main=colnames(imat)[col.i])
}


## -----------------------------------------------------------------------------

pen.peaks.wide <- data.table::data.table(
  data.set=c("foo", "bar"),
  "10.1"=c(5L, 10L),
  "0.3"=c(26L, 39L))
pen.peaks.gather <- if(requireNamespace("tidyr"))tidyr::gather(
  pen.peaks.wide,
  "penalty",
  "peaks",
  -1,
  convert=TRUE)
str(pen.peaks.gather)

pen.peaks.nc <- nc::capture_melt_single(
  pen.peaks.wide,
  penalty="^[0-9.]+", as.numeric,
  value.name="peaks")
str(pen.peaks.nc)

pen.peaks.pivot <- if(requireNamespace("tidyr"))try(tidyr::pivot_longer(
  pen.peaks.wide,
  -1,
  names_to="penalty",
  names_transform=list(penalty=as.numeric),
  values_to="peaks"))
str(pen.peaks.pivot)

varying <- 2:3
pen.peaks.reshape.times <- stats::reshape(
  pen.peaks.wide,
  direction="long",
  varying=varying,
  times=as.numeric(names(pen.peaks.wide)[varying]),
  v.names="peaks",
  timevar="penalty")
str(pen.peaks.reshape.times)

pen.peaks.renamed <- pen.peaks.wide
names(pen.peaks.renamed) <- paste0(ifelse(
  grepl("^[0-9]", names(pen.peaks.wide)),
  "peaks_", ""),
  names(pen.peaks.wide))
pen.peaks.reshape.sep <- stats::reshape(
  pen.peaks.renamed,
  direction="long",
  varying=varying,
  sep="_",
  timevar="penalty")
str(pen.peaks.reshape.sep)


## -----------------------------------------------------------------------------
peaks.csv <- system.file(
  "extdata", "RD12-0002_PP16HS_5sec_GM_F_1P.csv",
  package="nc", mustWork=TRUE)
peaks.wide <- data.table::fread(peaks.csv)
tidyr.long <- tidyr::pivot_longer(
  peaks.wide,
  grep(" [0-9]", names(peaks.wide)),
  names_pattern = "(.*) ([0-9]+)",
  names_to = c(".value", "peak"),
  names_transform = list(peak=as.integer))
peaks.tall <- nc::capture_melt_multiple(
  peaks.wide,
  column=".*",
  " ",
  peak="[0-9]+", as.integer)
options(old.par)

