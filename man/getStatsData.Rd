\name{getStatsData}
\alias{getStatsData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getStatsData(statsDataId = NULL, dataSetId = NULL, limit = NULL, lvTab = "", cdTab = NULL, lvTime = "", cdTime = NULL, lvArea = "", cdArea = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statsDataId}{
%%     ~~Describe \code{statsDataId} here~~
}
  \item{dataSetId}{
%%     ~~Describe \code{dataSetId} here~~
}
  \item{limit}{
%%     ~~Describe \code{limit} here~~
}
  \item{lvTab}{
%%     ~~Describe \code{lvTab} here~~
}
  \item{cdTab}{
%%     ~~Describe \code{cdTab} here~~
}
  \item{lvTime}{
%%     ~~Describe \code{lvTime} here~~
}
  \item{cdTime}{
%%     ~~Describe \code{cdTime} here~~
}
  \item{lvArea}{
%%     ~~Describe \code{lvArea} here~~
}
  \item{cdArea}{
%%     ~~Describe \code{cdArea} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (statsDataId = NULL, dataSetId = NULL, limit = NULL, 
    lvTab = "", cdTab = NULL, lvTime = "", cdTime = NULL, lvArea = "", 
    cdArea = NULL) 
{
    if (is.null(statsDataId) & is.null(dataSetId)) 
        stop("Either statsDataId or dataSetId should be specified.")
    gf <- match.call(expand.dots = FALSE)
    m <- match(c("statsDataId", "dataSetId", "limit", "lvTab", 
        "cdTab", "lvTime", "cdTime", "lvArea", "cdArea"), names(gf), 
        0L)
    gf <- gf[c(1L, 1L, m)]
    gf[[1L]] <- as.name("getForm")
    names(gf)[2L] <- "uri"
    gf[[2L]] <- paste(DBURL, "getStatsData", sep = "")
    if (length(gf$cdTab) > 1) 
        gf$cdTab <- paste(gf$cdTab, collapse = ",")
    if (length(gf$cdTime) > 1) 
        gf$cdTime <- paste(gf$cdTime, collapse = ",")
    if (length(gf$cdArea) > 1) 
        gf$cdArea <- paste(gf$cdArea, collapse = ",")
    gf$appId <- appId
    gf$lang <- lang
    root <- xmlRoot(xmlTreeParse(eval(gf)))
    status <- as.numeric(xmlValue(root[[1L]]["STATUS"][[1L]]))
    if (status != 0) 
        stop(xmlValue(root[[1L]]["ERROR_MSG"][[1L]]))
    tables <- getNodeSet(root[[3L]][[2L]], "//CLASS_INF//CLASS_OBJ[@id='tab']")[[1L]]
    name.tables <- xmlSApply(tables, xmlGetAttr, "name")
    names(name.tables) <- xmlSApply(tables, xmlGetAttr, "code")
    cats <- getNodeSet(root[[3L]][[2L]], "//CLASS_INF//CLASS_OBJ[contains(@id, 'cat')]")
    name.cats <- level.cats <- vector(length(cats), mode = "list")
    for (i in seq(cats)) {
        nc <- xmlSApply(cats[[i]], xmlGetAttr, "name")
        lc <- xmlSApply(cats[[i]], xmlGetAttr, "level")
        names(nc) <- names(lc) <- xmlSApply(cats[[i]], xmlGetAttr, 
            "code")
        name.cats[[i]] <- nc
        level.cats[[i]] <- lc
    }
    names(name.cats) <- sapply(cats, xmlGetAttr, "id")
    names(level.cats) <- sapply(cats, xmlGetAttr, "id")
    areas <- getNodeSet(root[[3L]][[2L]], "//CLASS_INF//CLASS_OBJ[@id='area']")[[1L]]
    name.areas <- xmlSApply(areas, xmlGetAttr, "name")
    level.areas <- xmlSApply(areas, xmlGetAttr, "level")
    names(name.areas) <- names(level.areas) <- xmlSApply(areas, 
        xmlGetAttr, "code")
    times <- getNodeSet(root[[3L]][[2L]], "//CLASS_INF//CLASS_OBJ[@id='time']")[[1L]]
    name.times <- xmlSApply(times, xmlGetAttr, "name")
    level.times <- xmlSApply(times, xmlGetAttr, "level")
    names(name.times) <- names(level.times) <- xmlSApply(times, 
        xmlGetAttr, "code")
    res.data <- vector(length(tables), mode = "list")
    for (i in 1:length(tables)) {
        data <- getNodeSet(root[[3L]][[3L]], paste("//DATA_INF//VALUE[@tab='", 
            names(name.tables[i]), "']", sep = ""))
        res.i <- data.frame(value = as.numeric(sapply(data, xmlValue)), 
            area.code = sapply(data, xmlGetAttr, "area"), time.code = sapply(data, 
                xmlGetAttr, "time"))
        res.i <- transform(res.i, area = name.areas[area.code], 
            area.level = level.areas[area.code], time = name.times[time.code], 
            time.level = level.times[time.code])
        for (nc in names(name.cats)) {
            nc.full <- sapply(data, xmlGetAttr, nc)
            res.i[paste(nc, ".code", sep = "")] <- nc.full
            res.i[nc] <- name.cats[nc][[1L]][nc.full]
            res.i[paste(nc, ".level", sep = "")] <- level.cats[nc][[1L]][nc.full]
        }
        res.data[[i]] <- res.i
    }
    names(res.data) <- name.tables
    res <- list(data = res.data, id = xmlValue(getNodeSet(root[[2L]], 
        "//PARAMETER//STATS_DATA_ID")[[1L]]), stat.name = xmlValue(getNodeSet(root[[3L]][[1L]], 
        "//TABLE_INF//STAT_NAME")[[1L]]), gov = xmlValue(getNodeSet(root[[3L]][[1L]], 
        "//TABLE_INF//GOV_ORG")[[1L]]), statistics.name = xmlValue(getNodeSet(root[[3L]][[1L]], 
        "//TABLE_INF//STATISTICS_NAME")[[1L]]), title = xmlValue(getNodeSet(root[[3L]][[1L]], 
        "//TABLE_INF//TITLE")[[1L]]), survey.date = xmlValue(getNodeSet(root[[3L]][[1L]], 
        "//TABLE_INF//SURVEY_DATE")[[1L]]))
    class(res) <- "jpstat"
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
