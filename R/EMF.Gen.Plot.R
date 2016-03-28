EMF.Gen.Plot <- function(
    dataplot,
    title=NULL, ylab=NULL, xlab="Generation",
    includeMean=TRUE, includeWorst=FALSE, includeStdDev=TRUE, includeBestComparision=TRUE,
    invert=FALSE
)
{
    #Identify the last generation
    g = dataplot$generation;
    invertValue = 1.0;
    if(invert)
        invertValue = -1.0;

    #Prepare data to plot (temp)
    temp <- data.frame(Generation = c( seq(1, g) ),
                       Statistic = c( rep("best", g) ),
                       StatisticalPoints = c( invertValue* dataplot$best[1:g] ) ) #c() if necessary

    if(includeMean)
        temp <- rbind(temp, data.frame(Generation = c( seq(1, g) ),
                                       Statistic = c( rep("mean", g) ),
                                       StatisticalPoints = c( invertValue* dataplot$mean[1:g] ) ))

    if(includeWorst)
        temp <- rbind(temp, data.frame(Generation = c( seq(1, g) ),
                                       Statistic = c(rep("worst", g)),
                                       StatisticalPoints = c( invertValue* dataplot$worst[1:g] ) ))

    if(includeStdDev)
        temp <- rbind(temp, data.frame(Generation = c( seq(1, g) ),
                                       Statistic = c(rep("std", g)),
                                       StatisticalPoints = c( invertValue* dataplot$stdDev[1:g] + invertValue* dataplot$mean[1:g]) ))

    minY = min(temp$StatisticalPoints);
    maxY = max(temp$StatisticalPoints);

    #Prepare basic plot:
    pl <-
    ggplot(temp,
           aes(x = Generation,
               y = StatisticalPoints,
               group = Statistic,
               colour = Statistic)) +
        geom_line( aes(linetype=Statistic) ) +
        geom_point( aes(shape=Statistic, size=Statistic) ) +
        scale_x_continuous(limits = c(0,  g)) +
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
        ybest = min(dataplot$best) * invertValue;

        pl <- pl +
            #geom_hline(y = max(temp$Survivalpoints), lty = 2, color="gray") +
            geom_hline(yintercept = ybest, lty = 2, color="gray") +
            annotate("text", x = 1, y = ybest - (maxY-minY)*0.025*0.5*invertValue,
                     hjust = 0, size = 3, color = "black",
                     label = paste("Best solution:", min(dataplot$best) * invertValue) );
    }


    #print and return
    print(pl);
    return (pl);
}

