\name{EMF.Gen.Plot}
\alias{EMF.Gen.Plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an EMF.Gen Graph
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EMF.Gen.Plot(dataplot, title = NULL, ylab = NULL, xlab="Generation", includeMean = TRUE, includeWorst = TRUE, includeStdDev = TRUE, includeBestComparision = TRUE, invert = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataplot}{
%%     ~~Describe \code{dataplot} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{includeMean}{
%%     ~~Describe \code{includeMean} here~~
}
  \item{includeWorst}{
%%     ~~Describe \code{includeWorst} here~~
}
  \item{includeStdDev}{
%%     ~~Describe \code{includeStdDev} here~~
}
  \item{includeBestComparision}{
%%     ~~Describe \code{includeBestComparision} here~~
}
  \item{invert}{
%%     ~~Describe \code{invert} here~~
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
function (dataplot, title = NULL, ylab = NULL, includeMean = TRUE,
    includeWorst = TRUE, includeStdDev = TRUE, includeBestComparision = TRUE,
    invert = FALSE)
{
    g = dataplot$generation
    invertValue = 1
    if (invert)
        invertValue = -1
    temp <- data.frame(Generation = c(seq(1, g)), Statistic = c(rep("best",
        g)), StatisticalPoints = c(invertValue * dataplot$best[1:g]))
    if (includeMean)
        temp <- rbind(temp, data.frame(Generation = c(seq(1,
            g)), Statistic = c(rep("mean", g)), StatisticalPoints = c(invertValue *
            dataplot$mean[1:g])))
    if (includeWorst)
        temp <- rbind(temp, data.frame(Generation = c(seq(1,
            g)), Statistic = c(rep("worst", g)), StatisticalPoints = c(invertValue *
            dataplot$worst[1:g])))
    if (includeStdDev)
        temp <- rbind(temp, data.frame(Generation = c(seq(1,
            g)), Statistic = c(rep("std", g)), StatisticalPoints = c(invertValue *
            dataplot$stdDev[1:g])))
    minY = min(temp$StatisticalPoints)
    maxY = max(temp$StatisticalPoints)
    pl <- ggplot(temp, aes(x = Generation, y = StatisticalPoints,
        group = Statistic, colour = Statistic)) + geom_line(aes(linetype = Statistic)) +
        geom_point(aes(shape = Statistic, size = Statistic)) +
        scale_x_continuous(limits = c(0, g)) + scale_y_continuous(limits = c(minY -
        (maxY - minY) * 0.05 * 0.5 * (!invert) * includeBestComparision,
        maxY)) + scale_linetype_manual(values = c(mean = "solid",
        best = "solid", worst = "solid", std = "blank")) + scale_shape_manual(values = c(mean = 1,
        best = 1, worst = 1, std = 20)) + scale_size_manual(values = c(mean = 0,
        best = 0, worst = 0, std = 1)) + scale_color_manual(values = c(mean = "blue",
        best = "red", worst = "purple", std = "black"))
    pl <- pl + ylab(ylab)
    pl <- pl + ggtitle(title)
    if (includeBestComparision)
        pl <- pl + geom_hline(yintercept = min(dataplot$best),
            lty = 2) + annotate("text", x = 1, y = min(dataplot$best) -
            (maxY - minY) * 0.025 * 0.5, hjust = 0, size = 3,
            color = "black", label = paste("Best solution:",
                min(dataplot$best)))
    print(pl)
    return(pl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
