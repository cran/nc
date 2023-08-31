## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
head(iris)

## -----------------------------------------------------------------------------
(iris.tall <- nc::capture_melt_single(
  iris,
  part=".*",
  "[.]",
  dim=".*",
  value.name="cm"))

## ---- fig.width=10------------------------------------------------------------

if(require(ggplot2)){
  ggplot()+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    facet_grid(part ~ dim)+
    geom_bar(aes(cm, fill=Species), data=iris.tall)
}


## -----------------------------------------------------------------------------
(iris.part.cols <- nc::capture_melt_multiple(
  iris,
  column=".*",
  "[.]",
  dim=".*"))

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
ert.gz <- system.file(
  "extdata", "ert_eff_ic_m.tsv.gz", package="nc", mustWork=TRUE)
ert.all <- data.table::fread(ert.gz, na.strings=":")
ert.all[1:5, 1:5]

## -----------------------------------------------------------------------------
ert.first <- ert.all[, 1]
csv.lines <- c(sub("\\\\.*", "", names(ert.first)), ert.first[[1]])
ert.first.dt <- data.table::fread(text=paste(csv.lines, collapse="\n"))
ert.wide <- data.table::data.table(ert.first.dt, ert.all[,-1])
ert.wide[1:5, 1:5]

## -----------------------------------------------------------------------------
(ert.tall <- nc::capture_melt_single(
  ert.wide,
  year="[0-9]{4}", as.integer,
  "M",
  month="[0-9]{2}", as.integer))

## -----------------------------------------------------------------------------

ert.tall[, month.IDate := data.table::as.IDate(
  sprintf("%d-%d-15", year, month))]
if(require("ggplot2")){
  ggplot()+
    geom_hline(aes(
      yintercept=value),
      color="grey",
      data=data.frame(value=100))+
    geom_line(aes(
      month.IDate, value, color=geo),
      data=ert.tall[geo %in% c("CA", "US", "JP", "FR")])+
    facet_grid(exch_rt ~ .)+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))
}


## -----------------------------------------------------------------------------
nc::capture_melt_single(ert.wide, month.POSIXct="[0-9].*", function(x){
  as.POSIXct(strptime(paste0(x,"15"), "%YM%m%d"))
})

## -----------------------------------------------------------------------------
iris.missing <- iris[, names(iris) != "Sepal.Length"]
head(iris.missing)

## -----------------------------------------------------------------------------
nc::capture_melt_multiple(iris.missing, iris.pattern, fill=TRUE)

## -----------------------------------------------------------------------------
peaks.csv <- system.file(
  "extdata", "RD12-0002_PP16HS_5sec_GM_F_1P.csv",
  package="nc", mustWork=TRUE)
peaks.wide <- data.table::fread(peaks.csv)
print(data.table::data.table(
  names=names(peaks.wide),
  class=sapply(peaks.wide, class)),
  topn=10)

## ---------------------------------------------------------------------------------------
peaks.tall <- nc::capture_melt_multiple(
  peaks.wide,
  column=".*",
  " ",
  peak="[0-9]+", as.integer,
  na.rm=TRUE)
options(width=90)
print(peaks.tall)
str(peaks.tall)

