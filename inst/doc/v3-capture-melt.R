## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(data.table)
(iris.dt <- data.table(i=1:nrow(iris), iris[,1:4], Species=paste(iris$Species)))

## -----------------------------------------------------------------------------
(iris.tall <- nc::capture_melt_single(
  iris.dt,
  part=".*",
  "[.]",
  dim=".*"))

## -----------------------------------------------------------------------------
(iris.part.cols <- dcast(
  iris.tall,
  i + Species + dim ~ part))

## -----------------------------------------------------------------------------
nc::capture_melt_multiple(
  iris.dt,
  column=".*",
  "[.]",
  dim=".*")

## ---- fig.width=10------------------------------------------------------------

if(require(ggplot2)){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(dim ~ Species)+
    coord_equal()+
    geom_abline(slope=1, intercept=0, color="grey")+
    geom_point(aes(
      Petal, Sepal),
      data=iris.part.cols)
}


## -----------------------------------------------------------------------------

if(requireNamespace("tidyr")){
  data(who, package="tidyr")
}else{
  who <- data.frame(id=1, new_sp_m5564=2, newrel_f65=3)
}
names(who)


## -----------------------------------------------------------------------------
new.diag.gender <- list(
  "new_?",
  diagnosis=".*",
  "_",
  gender=".")
nc::capture_melt_single(who, new.diag.gender, ages=".*")

## -----------------------------------------------------------------------------
years.pattern <- list(new.diag.gender, ages=list(
  min.years="0|[0-9]{2}", as.numeric,
  max.years="[0-9]{0,2}", function(x)ifelse(x=="", Inf, as.numeric(x))))
(who.typed <- nc::capture_melt_single(
  who, years.pattern,
  value.name="count"))
str(who.typed)

## -----------------------------------------------------------------------------
family.dt <- fread(text="
family_id age_mother dob_child1 dob_child2 dob_child3 gender_child1 gender_child2 gender_child3
1         30 1998-11-26 2000-01-29         NA             1             2            NA
2         27 1996-06-22         NA         NA             2            NA            NA
3         26 2002-07-11 2004-04-05 2007-09-02             2             2             1
4         32 2004-10-10 2009-08-27 2012-07-21             1             1             1
5         29 2000-12-05 2005-02-28         NA             2             1            NA")

## -----------------------------------------------------------------------------
melt(family.dt, measure.vars=patterns(
  dob="^dob", gender="^gender"
))

## -----------------------------------------------------------------------------
(children.dt <- nc::capture_melt_multiple(
  family.dt,
  column=".*",
  "_",
  nc::field("child", "", "[1-3]", as.integer),
  na.rm=TRUE))
str(children.dt)

## -----------------------------------------------------------------------------
set.seed(1)
iris.rand <- iris.dt[sample(.N)]
iris.wide <- cbind(treatment=iris.rand[1:75], control=iris.rand[76:150])
print(iris.wide, topn=2, nrows=10)

## -----------------------------------------------------------------------------
iris.melted <- melt(iris.wide, measure.vars = patterns(
  i="i$",
  Sepal.Length="Sepal.Length$",
  Sepal.Width="Sepal.Width$",
  Petal.Length="Petal.Length$",
  Petal.Width="Petal.Width$",
  Species="Species$"))
identical(iris.melted[order(i), names(iris.dt), with=FALSE], iris.dt)

## -----------------------------------------------------------------------------
(nc.melted <- nc::capture_melt_multiple(
  iris.wide,
  group=".*?",
  "[.]",
  column=".*"))
identical(nc.melted[order(i), names(iris.dt), with=FALSE], iris.dt)

