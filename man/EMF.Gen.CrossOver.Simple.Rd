\name{EMF.Gen.CrossOver.Simple}
\alias{EMF.Gen.CrossOver.Simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross Over two chromosomes 'parents' into two chromosomes 'children'
}
\description{
This method cross over two chromosomes 'parents' into two chromosomes 'children'.
It's possible to have one or two crossing points. By default, large chromosomes (>10) have 2 crossing points and small chromosomes (<=10). \n
If chromosomes have size = 2 only one crossing point will be chosen.
If chromosomes have size = 1, the two children will be exactly equal to their parents.

}
\usage{
EMF.Gen.CrossOver.Simple(p1, p2)
EMF.Gen.CrossOver.Simple(p1, p2, twoPoints = FALSE)
EMF.Gen.CrossOver.Simple(p1, p2, twoPointsOnSmall = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
Parent One.
}
  \item{p2}{
Parent Two.
}
  \item{twoPoints}{
Defines if two crossing points will be used on large chromosomes (>10).
By default = TRUE.
}
  \item{twoPointsOnSmall}{
Defines if two crossing points will be used on small chromosomes (<=10).
By default = FALSE.
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
Elthon Manhas de Feritas
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
function (p1, p2, twoPoints = TRUE)
{
    sizeP1 = length(p1)
    sizeP2 = length(p2)
    if (sizeP1 != sizeP2)
        stop("Parents must have same size.")
    if (twoPoints) {
        crossOverPoint1 = sample(1:(sizeP1 - 1), 1)
        crossOverPoint2 = crossOverPoint1
        while (crossOverPoint2 == crossOverPoint1) {
            crossOverPoint2 = sample(1:(sizeP1 - 1), 1)
        }
        cop1 = min(crossOverPoint1, crossOverPoint2)
        cop2 = max(crossOverPoint1, crossOverPoint2)
        cat(cop1)
        cat(cop2)
        child1 = c(p1[1:cop1], p2[(cop1 + 1):cop2], p1[(cop2 +
            1):sizeP1])
        child2 = c(p2[1:cop1], p1[(cop1 + 1):cop2], p2[(cop2 +
            1):sizeP1])
    }
    else {
        crossOverPoint = sample(1:(sizeP1 - 1), 1)
        child1 = c(p1[1:crossOverPoint], p2[(crossOverPoint +
            1):sizeP1])
        child2 = c(p2[1:crossOverPoint], p1[(crossOverPoint +
            1):sizeP1])
    }
    ret = matrix(c(child1, child2), nrow = 2, ncol = sizeP1,
        byrow = TRUE)
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
