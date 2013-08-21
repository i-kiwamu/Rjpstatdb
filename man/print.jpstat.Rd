\name{print.jpstat}
\alias{print.jpstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.jpstat(jpstat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jpstat}{
%%     ~~Describe \code{jpstat} here~~
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
function (jpstat) 
{
    cat("ID: ", jpstat$id, "\n")
    cat("Stat name: ", jpstat$stat.name, "\n")
    cat("Government: ", jpstat$gov, "\n")
    cat("Statistics name: ", jpstat$statistics.name, "\n")
    cat("Title: ", jpstat$title, "\n")
    cat("Survey date: ", jpstat$survey.date, "\n")
    cat("Tables:\n")
    for (i in seq(jpstat$data)) {
        cat("  Name: ", names(jpstat$data)[i], "\n")
        print(head(jpstat$data[[i]]))
        cat("  Dimension: ", dim(jpstat$data[[i]]), "\n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
