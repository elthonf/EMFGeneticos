EMF.Gen.Plot <- function(
    dataplot,
    title=NULL, ylab=NULL, xlab="Generation",
    includeMean=TRUE, includeWorst=FALSE, includeStdDev=TRUE, includeBestComparision=TRUE,
    minGen = 1, maxGen = NA,
    invert=FALSE
)
{

    invertValue = 1.0;
    if(invert)
        invertValue = -1.0;

    #Identify the last generation
    if(minGen < 1 || minGen > dataplot$generation)
        stop( past("Invalid initial generation:", minGen) );
    if(is.na(maxGen))
        maxGen = dataplot$generation;
    if(maxGen < minGen || maxGen > dataplot$generation )
        stop( past("Invalid max generation:", maxGen) );

    rangeGen = maxGen - minGen + 1;

    #Prepare data to plot (temp)
    temp <- data.frame(Generation = c( seq(minGen, maxGen) ),
                       Statistic = c( rep("best", rangeGen) ),
                       StatisticalPoints = c( invertValue* dataplot$best[minGen:maxGen] ) ) #c() if necessary

    if(includeMean)
        temp <- rbind(temp, data.frame(Generation = c( seq(minGen, maxGen) ),
                                       Statistic = c( rep("mean", rangeGen) ),
                                       StatisticalPoints = c( invertValue* dataplot$mean[minGen:maxGen] ) ))

    if(includeWorst)
        temp <- rbind(temp, data.frame(Generation = c( seq(minGen, maxGen) ),
                                       Statistic = c(rep("worst", rangeGen)),
                                       StatisticalPoints = c( invertValue* dataplot$worst[minGen:maxGen] ) ))

    if(includeStdDev)
        temp <- rbind(temp, data.frame(Generation = c( seq(minGen, maxGen) ),
                                       Statistic = c(rep("std", rangeGen)),
                                       StatisticalPoints = c( invertValue* dataplot$stdDev[minGen:maxGen] + invertValue* dataplot$mean[minGen:maxGen]) ))

    minY = min(temp$StatisticalPoints, na.rm = TRUE);
    maxY = max(temp$StatisticalPoints, na.rm = TRUE);

    #Prepare basic plot:
    pl <-
    ggplot(temp,
           aes(x = Generation,
               y = StatisticalPoints,
               group = Statistic,
               colour = Statistic)) +
        geom_line( aes(linetype=Statistic) ) +
        geom_point( aes(shape=Statistic, size=Statistic) ) +
        scale_x_continuous(limits = c(minGen,  maxGen)) +
        scale_y_continuous(limits = c( minY - (maxY-minY)*0.05*0.5 * (!invert)*includeBestComparision, maxY + (maxY-minY)*0.05*0.5 * (invert)*includeBestComparision) ) +
        scale_linetype_manual(values=c("mean"="solid", "best"="solid", "worst"="solid", "std"="blank")) +
        scale_shape_manual(values=c("mean"=1, "best"=1, "worst"=1, "std"=20)) +
        scale_size_manual(values=c("mean"=0, "best"=0, "worst"=0, "std"=1)) +
        #scale_colour_brewer(palette = "Set1") +
        scale_color_manual(values=c("mean"="blue", "best"="red", "worst"="purple", "std"="black"));

    #ADD Titles
    pl <- pl + ylab(ylab);
    pl <- pl + xlab(xlab);
    pl <- pl + ggtitle(title);

    if(includeBestComparision)
    {
        ybest = min(dataplot$best[minGen:maxGen], na.rm = TRUE) * invertValue;

        pl <- pl +
            #geom_hline(y = max(temp$Survivalpoints), lty = 2, color="gray") +
            geom_hline(yintercept = ybest, lty = 2, color="gray") +
            annotate("text", x = 1, y = ybest - (maxY-minY)*0.025*0.5*invertValue,
                     hjust = 0, size = 3, color = "black",
                     label = paste("Best solution:", min(dataplot$best[minGen:maxGen], na.rm = TRUE) * invertValue) );
    }


    #print and return
    print(pl);
    return (pl);
}

