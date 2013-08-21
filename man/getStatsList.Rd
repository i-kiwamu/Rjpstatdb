\name{getStatsList}
\alias{getStatsList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getStatsList(searchWord = "", surveyYears = "", openYears = "", statsField = NULL, statsCode = NULL, searchKind = 1, statsNameList = "Y")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{searchWord}{
%%     ~~Describe \code{searchWord} here~~
}
  \item{surveyYears}{
%%     ~~Describe \code{surveyYears} here~~
}
  \item{openYears}{
%%     ~~Describe \code{openYears} here~~
}
  \item{statsField}{
%%     ~~Describe \code{statsField} here~~
}
  \item{statsCode}{
%%     ~~Describe \code{statsCode} here~~
}
  \item{searchKind}{
%%     ~~Describe \code{searchKind} here~~
}
  \item{statsNameList}{
%%     ~~Describe \code{statsNameList} here~~
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
function (searchWord = "", surveyYears = "", openYears = "", 
    statsField = NULL, statsCode = NULL, searchKind = 1, statsNameList = "Y") 
{
    gf <- match.call(expand.dots = FALSE)
    m <- match(c("searchWord", "surveyYears", "openYears", "statsField", 
        "statsCode", "searchKind", "statsNameList"), names(gf), 
        0L)
    gf <- gf[c(1L, 1L, m)]
    gf[[1L]] <- as.name("getForm")
    names(gf)[2L] <- "uri"
    gf[[2L]] <- paste(DBURL, "getStatsList", sep = "")
    gf$appId <- appId
    gf$lang <- lang
    root <- xmlRoot(xmlTreeParse(eval(gf)))
    status <- as.numeric(xmlValue(root[[1L]]["STATUS"][[1L]]))
    if (status != 0) 
        stop(xmlValue(root[[1L]]["ERROR_MSG"][[1L]]))
    ResList <- root[[3L]][names(root[[3L]]) == "LIST_INF"]
    res <- data.frame(Data_Set_ID = sapply(ResList, xmlGetAttr, 
        "id"), Stat_Name = sapply(ResList, function(x) paste(substr(xmlValue(x["STATISTICS_NAME"][[1L]]), 
        1, 10), "...", sep = "")), Stat_Code = sapply(ResList, 
        function(x) xmlGetAttr(x["STAT_NAME"][[1L]], "code")), 
        Org = sapply(ResList, function(x) xmlValue(x["GOV_ORG"][[1L]])), 
        Survey = sapply(ResList, function(x) xmlValue(x["SURVEY_DATE"][[1L]])), 
        Open = sapply(ResList, function(x) xmlValue(x["OPEN_DATE"][[1L]])))
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
