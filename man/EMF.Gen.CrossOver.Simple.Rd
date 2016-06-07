\name{EMF.Gen.CrossOver.Simple}
\alias{EMF.Gen.CrossOver.Simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross Over two chromosomes 'parents' into two chromosomes 'children'
}
\description{
This method cross over two chromosomes 'parents' into two chromosomes 'children'.
It's possible to have one or two crossing points. By default, large chromosomes (>10) have 2 crossing points and small chromosomes (<=10).
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
function( p1, p2, twoPoints = TRUE, twoPointsOnSmall = FALSE  )
{
    #Se não for matriz, transforma em matriz
    if(!is.matrix(p1)) p1 = matrix(p1, ncol = length(p1));
    if(!is.matrix(p2)) p2 = matrix(p2, ncol = length(p2));

    sizeP1 = length(p1);
    sizeP2 = length(p2);

    if(sizeP1 != sizeP2)
        stop("Parents must have same size.");
    if(sizeP1 <= 1){
        ret = matrix( c(p1, p2), nrow = 2, ncol=sizeP1, byrow = TRUE );
        return ( ret );
    }

    pairs = dim(p1)[1]; #How many pairs
    chromoSize = dim(p1)[2]; #Each chromosome size

    #Prepare the children
    child1 = matrix( nrow = pairs, ncol = chromoSize);
    child2 = matrix( nrow = pairs, ncol = chromoSize);

    if( ( twoPoints && (chromoSize > 10) ) ||
        ( twoPointsOnSmall && (chromoSize > 2)) ) #TWO POINTS CROSSOVER
    {
        for(i in 1:pairs){
            crossOverPoints = sample(1:(chromoSize-1), size = 2); #2 distinct points

            cop1 = min(crossOverPoints);
            cop2 = max(crossOverPoints);

            #cat(cop1);cat(cop2);

            child1[i,] = c( p1[i, 1 : cop1 ], p2[i, (cop1+1) : cop2], p1[i, (cop2+1) : chromoSize] );
            child2[i,] = c( p2[i, 1 : cop1 ], p1[i, (cop1+1) : cop2], p2[i, (cop2+1) : chromoSize] );
        }
    }
    else #ONE POINT CROSSOVER
    {
        crossOverPoints = sample(1:(chromoSize-1), pairs, replace = TRUE);

        for(i in 1:pairs){
            child1[i,] = c( p1[i, 1:crossOverPoints[i]], p2[i, (crossOverPoints[i]+1):chromoSize] );
            child2[i,] = c( p2[i, 1:crossOverPoints[i]], p1[i, (crossOverPoints[i]+1):chromoSize] );
        }

    }

    return ( rbind(child1, child2) );
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
