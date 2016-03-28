\name{EMF.Gen.Workflow}
\alias{EMF.Gen.Workflow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Controls the Genetic Algoritm Code
}
\description{
The function controls all steps needed in an Genetic Algoritm
}
\usage{
EMF.Gen.Workflow()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{popSize}{
Population size.
By default = 1000
}
  \item{iters}{
Number of MAX iterations.
By default = 100
}
  \item{crossOver}{
How many children from CrossOver should be generated.
By default = 5000
}
  \item{mutationChance}{
A mutation chance on crossOver, between 0.00 and 1.00.
By default = 0.00
}
  \item{suggestions}{
A possible suggestion for the initial population
}
  \item{clone}{
Number of clones of the population (integer).
By default is about 2\% of the population size
}
  \item{cloneAndMutate}{
Number of clones and mutating of the population (integer).
By default is about 0.5\% of the population size
}
  \item{elitism}{
The number of chromosomes that are kept into the next generation.
        By default is about 1\% of the population size
}
  \item{foreigners}{
The number of new random chromosomes to be inserted in a generation.
}
  \item{chromosomeRandFunc}{
REQUIRED: User supplied method to generate a random chromosome
}
  \item{evalFunc}{
REQUIRED: User supplied method to calculate the evaluation function for the given chromosome
}
  \item{monitorFunc}{
Method run after each generation to allow monitoring of the optimization
}
  \item{crossOverFunc}{
Method to crossOver 2 parents into 1 or more children. By default the method EMF.Gen.CrossOver.Simple is called. This default method mix 2 parents into 2 children.
}
  \item{mutationFunc}{
Method to mutate a chromosome. By default the method EMF.Gen.Mutate.Simple is called. This default method mix some gens of a random chromosome generated with the method chromosomeRandFunc.
}
  \item{parentProb}{
ParentProb defines the probability for every population-position, to get crossed over.
By default a normal distribution is applyed based on the fitness (evalFunc, fast).
If it is NA, the probability for all chromosomes, to get crossed, is the same (faster).
If it is NULL, the probability for chromosomes will be based on te fitness (evalFunc).
}
  \item{verbose}{
If true the algorithm will be more verbose.
By default False.
}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Elthon Manhas de Freitas
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
