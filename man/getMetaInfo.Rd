\name{getMetaInfo}
\alias{getMetaInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getMetaInfo(statsDataId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statsDataId}{
%%     ~~Describe \code{statsDataId} here~~
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
function (statsDataId) 
{
    gf <- match.call(expand.dots = FALSE)
    m <- match(c("statsDataId"), names(gf), 0L)
    gf <- gf[c(1L, 1L, m)]
    gf[[1L]] <- as.name("getForm")
    names(gf)[2L] <- "uri"
    gf[[2L]] <- paste(DBURL, "getMetaInfo", sep = "")
    gf$appId <- appId
    gf$lang <- lang
    root <- xmlRoot(xmlTreeParse(eval(gf)))
    status <- as.numeric(xmlValue(root[[1L]]["STATUS"][[1L]]))
    if (status != 0) 
        stop(xmlValue(root[[1L]]["ERROR_MSG"][[1L]]))
    xmlToList(root[[3L]])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
