\name{EMF.Gen.Mutate.Simple}
\alias{EMF.Gen.Mutate.Simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple mutation of 1 chromosomes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EMF.Gen.Mutate.Simple(original, mutationRate = 0.1, chromosomeRandFunc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{original}{
%%     ~~Describe \code{original} here~~
}
  \item{mutationRate}{
%%     ~~Describe \code{mutationRate} here~~
}
  \item{chromosomeRandFunc}{
%%     ~~Describe \code{chromosomeRandFunc} here~~
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
function (original, mutationRate = 0.1, chromosomeRandFunc = NULL)
{
    size = length(original)
    ret = original
    if (!is.null(chromosomeRandFunc)) {
        newSample = chromosomeRandFunc()
        for (c in 1:size) if (runif(1) <= mutationRate)
            ret[c] = newSample[c]
    }
    else {
        orderToSubstitute = sample(1:size, size)
        for (c in 1:size) if (runif(1) <= mutationRate) {
            substituteIndex = orderToSubstitute[c]
            substituteValue = ret[substituteIndex]
            ret[substituteIndex] = ret[c]
            ret[c] = substituteValue
        }
    }
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
