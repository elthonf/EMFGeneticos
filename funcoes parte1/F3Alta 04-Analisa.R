#Exibe os gráficos na tela
EMF.Gen.Plot(f3.exec01, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 1", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec02, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 2", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec03, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 3", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec04, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 4", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec05, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 5", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec06, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 6", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec07, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 7", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec08, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 8", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(f3.exec09, includeWorst = TRUE, title = "Solução para Função 3 de Alta Dimensionalidade exec.: 9", ylab = "Fitness", xlab = "Gerações")

#Variações
EMF.Gen.Plot(f3.exec01, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 1", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec01, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 1", ylab = "Fitness", xlab = "Gerações", minGen = 100, maxGen = 200, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec02, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 2", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec03, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 3", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec04, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 4", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec05, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 5", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec06, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 6", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec07, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 7", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec08, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 8", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 150, includeBestComparision = FALSE)
EMF.Gen.Plot(f3.exec08, includeWorst = FALSE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 8", ylab = "Fitness", xlab = "Gerações", minGen = 500, maxGen = 550, includeBestComparision = FALSE, includeStdDev = FALSE, includeMean = FALSE)
EMF.Gen.Plot(f3.exec09, includeWorst = TRUE, title = "Trecho da solução para Função 3 de Alta Dimensionalidade exec.: 9", ylab = "Fitness", xlab = "Gerações", minGen = 001, maxGen = 200, includeBestComparision = FALSE)

#Grava os logs e imagens
all = list( f3.exec01, f3.exec02, f3.exec03, f3.exec04, f3.exec05, f3.exec06, f3.exec07, f3.exec08, f3.exec09);
all.frame = data.frame();


for(i in 1:length(all)){
    temp = all[[i]];

    istring = toString(i);
    if(i < 10) istring = paste("0", istring, sep="");

    EMF.Gen.LogStatistics(execData = all[[i]],
                          execName = paste("Função 3 de Alta Dimensionalidade Execução ", istring, sep=""),
                          fileName = paste("F3Alta-", istring, sep=""),
                          title = paste("Solução para Função 3 de Alta Dimensionalidade exec.:", i),
                          ylab = "Fitness", xlab = "Gerações", includeWorst = TRUE
    );

    all.frame = rbind(all.frame,
                        c(i,
                        temp$input.params$popSize,
                        temp$input.params$iters,
                        temp$generation,
                        temp$input.params$crossOver,
                        temp$input.params$mutationChance,
                        temp$input.params$clone,
                        temp$input.params$cloneAndMutate,
                        temp$input.params$elitism,
                        min(temp$best, na.rm = TRUE),
                        which ( temp$best == min(temp$best, na.rm = TRUE) )[1]
                     )
    );
    rm(temp);
    rm(istring);
}
rm(i);

colnames(all.frame) <- c("Execução", "População", "Iterações Plan", "Iterações Exec", "CrossOver", "Chance Mutação Cross", "Clone", "Mutação", "Elitismo", "Melhor Resultado", "Geração Melhor");


rm(all);

View(all.frame);


print(all.frame$`Melhor Resultado`[9], digits = 20)
