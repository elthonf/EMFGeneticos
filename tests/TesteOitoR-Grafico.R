g = f$generation

temp2 <- data.frame(Generation = c(seq(1, g), seq(1, g), seq(1, g), seq(1, g)),
                    Statistic = c(rep("mean", g), rep("best", g), rep("worst", g), rep("std", g) ),
                    StatisticalPoints = c( f$mean[1:g], f$best[1:g], f$worst[1:g], f$stdDev[1:g])
                    )



pl2 <-
    ggplot(temp2,
              aes(x = Generation,
                  y = StatisticalPoints,
                  group = Statistic,
                  colour = Statistic)) +
    geom_line( aes(linetype=Statistic) ) +
    geom_point( aes(shape=Statistic, size=Statistic) ) +
    scale_x_continuous(limits = c(0,  g)) +
    scale_y_continuous(limits = c( min(temp2$StatisticalPoints)-2 , max(temp2$StatisticalPoints))) +
    #geom_hline(y = max(temp$Survivalpoints), lty = 2) +
    geom_hline(yintercept = min(f$best), lty = 2) +
    annotate("text", x = 1, y = min(f$best) - 1,
             hjust = 0, size = 3, color = "black",
             label = paste("Best solution:", min(f$best))
    ) +

    scale_linetype_manual(values=c("mean"="solid", "best"="solid", "worst"="solid", "std"="blank")) +
    scale_shape_manual(values=c("mean"=1, "best"=1, "worst"=1, "std"=20)) +
    scale_size_manual(values=c("mean"=0, "best"=0, "worst"=0, "std"=1)) +
    #scale_colour_brewer(palette = "Set1") +
    scale_color_manual(values=c("mean"="blue", "best"="red", "worst"="purple", "std"="black")) +
    ggtitle("Título")


pl2 <- pl2 + ylab("My y label")
pl2 <- pl2 + ggtitle("Título2")

print(pl2)



