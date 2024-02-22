## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
data.table::setDTthreads(1)
old.opt <- options(width=100)

## -------------------------------------------------------------------------------------------------
library(data.table)
dir.create(iris.dir <- tempfile())
icsv <- function(sp)file.path(iris.dir, paste0(sp, ".csv"))
(iris.dt <- data.table(iris))

## -------------------------------------------------------------------------------------------------
iris.dt[, fwrite(.SD, icsv(Species)), by=Species]
dir(iris.dir)

## -------------------------------------------------------------------------------------------------
data.table::fread(file.path(iris.dir,"setosa.csv"), nrows=2)

## -------------------------------------------------------------------------------------------------
(iglob <- file.path(iris.dir,"*.csv"))
Sys.glob(iglob)

## -------------------------------------------------------------------------------------------------
nc::capture_first_glob(iglob, Species="[^/]+", "[.]csv")

## -------------------------------------------------------------------------------------------------
db <- system.file("extdata/chip-seq-chunk-db", package="nc", mustWork=TRUE)
(glob <- paste0(db, "/*/*/counts/*gz"))
(matched.files <- Sys.glob(glob))

## -------------------------------------------------------------------------------------------------
readLines(matched.files[1], n=5)

## -------------------------------------------------------------------------------------------------
read.bedGraph <- function(f)data.table::fread(
  f, skip=1, col.names = c("chrom","start", "end", "count"))
read.bedGraph(matched.files[1])

## -------------------------------------------------------------------------------------------------
data.chunk.pattern <- list(
  data="H.*?",
  "/",
  chunk="[0-9]+", as.integer)
(data.chunk.dt <- nc::capture_first_glob(glob, data.chunk.pattern, READ=read.bedGraph))

## -------------------------------------------------------------------------------------------------
base.df.list <- list()
for(file.csv in matched.files){
  file.df <- read.bedGraph(file.csv)
  counts.path <- dirname(file.csv)
  chunk.path <- dirname(counts.path)
  data.path <- dirname(chunk.path)
  base.df.list[[file.csv]] <- data.frame(
    data=basename(data.path),
    chunk=basename(chunk.path),
    file.df)
}
base.df <- do.call(rbind, base.df.list)
rownames(base.df) <- NULL
head(base.df)
str(base.df)

## -------------------------------------------------------------------------------------------------
if(requireNamespace("arrow")){
  path <- tempfile()
  arrow::write_dataset(
    dataset=data.chunk.dt,
    path=path,
    format="csv",
    partitioning=c("data","chunk"),
    max_rows_per_file=1000)
  hive.glob <- file.path(path, "*", "*", "*.csv")
  (hive.files <- Sys.glob(hive.glob))
}

## -------------------------------------------------------------------------------------------------
if(requireNamespace("arrow")){
  data.table::fread(hive.files[1])
}

## -------------------------------------------------------------------------------------------------
if(requireNamespace("arrow")){
  hive.pattern <- list(
    nc::field("data","=",".*?"),
    "/",
    nc::field("chunk","=",".*?", as.integer),
    "/",
    nc::field("part","-","[0-9]+", as.integer))
  print(hive.dt <- nc::capture_first_glob(hive.glob, hive.pattern))
  hive.dt[, .(rows=.N), keyby=.(data,chunk,part)]
}

## -------------------------------------------------------------------------------------------------
(count.dt <- nc::capture_first_glob(
  glob,
  data.chunk.pattern,
  "/counts/", 
  name=list("McGill", id="[0-9]+", as.integer),
  READ=read.bedGraph))
count.dt[, .(count=.N), by=.(data, chunk, name, id, chrom)]

## -------------------------------------------------------------------------------------------------
if(require(ggplot2)){
  ggplot()+
    facet_wrap(~data+chunk+name+chrom, labeller=label_both, scales="free")+
    geom_step(aes(
      start/1e3, count),
      data=count.dt)
}

## -------------------------------------------------------------------------------------------------
vignettes <- system.file("extdata/vignettes", package="nc", mustWork=TRUE)
(vglob <- paste0(vignettes, "/*.Rmd"))
(vfiles <- Sys.glob(vglob))

## -------------------------------------------------------------------------------------------------
non.greedy.lines <- list(
  list(".*\n"), "*?")
optional.name <- list(
  list(" ", chunk_name="[^,}]+"), "?")
chunk.pattern <- list(
  before=non.greedy.lines,
  "```\\{r",
  optional.name,
  parameters=".*",
  "\\}\n",
  code=non.greedy.lines,
  "```")
READ.vignette <- function(f)nc::capture_all_str(f, chunk.pattern)
str(READ.vignette(vfiles[1]))

## -------------------------------------------------------------------------------------------------
chunk.dt <- nc::capture_first_glob(
  vglob,
  "/v",
  vignette_number="[0-9]", as.integer,
  "-",
  vignette_name=".*?",
  ".Rmd",
  READ=READ.vignette
)[
, chunk_number := seq_along(chunk_name), by=vignette_number
]
chunk.dt[, .(
  vignette_number, vignette_name, chunk_number, chunk_name, 
  lines=nchar(code))]

## -------------------------------------------------------------------------------------------------
cat(chunk.dt$code[2])

## -----------------------------------------------------------------------------
options(old.opt)

