#Exibe os gráficos na tela
EMF.Gen.Plot(cvrp.exec01, includeWorst = FALSE, title = "Solução para CVRP com operadores clássicos. exec.: 1", ylab = "Fitness", xlab = "Gerações",
             includeMean = FALSE, includeStdDev = FALSE,
             minGen = 1, maxGen = max(which( cvrp.exec01$best != Inf )))
EMF.Gen.Plot(cvrp.exec02, includeWorst = FALSE, title = "Solução para CVRP com operadores clássicos. exec.: 2", ylab = "Fitness", xlab = "Gerações",
             includeMean = FALSE, includeStdDev = FALSE,
             minGen = 1, maxGen = max(which( cvrp.exec02$best != Inf )))
EMF.Gen.Plot(cvrp.exec03, includeWorst = FALSE, title = "Solução para CVRP com operadores clássicos. exec.: 3", ylab = "Fitness", xlab = "Gerações",
             includeMean = FALSE, includeStdDev = FALSE,
             minGen = 1, maxGen = max(which( cvrp.exec03$best != Inf )))
EMF.Gen.Plot(cvrp.exec04, includeWorst = FALSE, title = "Solução para CVRP com operadores clássicos. exec.: 4", ylab = "Fitness", xlab = "Gerações",
             includeMean = FALSE, includeStdDev = FALSE,
             minGen = 1, maxGen = max(which( cvrp.exec04$best != Inf )))
EMF.Gen.Plot(cvrp.exec05, includeWorst = FALSE, title = "Solução para CVRP com operadores clássicos. exec.: 5", ylab = "Fitness", xlab = "Gerações",
             includeMean = FALSE, includeStdDev = FALSE,
             minGen = 1, maxGen = max(which( cvrp.exec05$best != Inf )))



#Variações

#Grava os logs e imagens
all = list( cvrp.exec01, cvrp.exec02, cvrp.exec03, cvrp.exec04, cvrp.exec05);
all.frame = data.frame();


for(i in 1:length(all)){
    temp = all[[i]];

    istring = toString(i);
    if(i < 10) istring = paste("0", istring, sep="");

    EMF.Gen.LogStatistics(execData = all[[i]],
                          execName = paste("Função CVRP com operadores clássicos ", istring, sep=""),
                          fileName = paste("cvrpClassicos", istring, sep=""),
                          title = paste("Solução para CVRP com operadores clássicos. exec.:", i),
                          ylab = "Fitness", xlab = "Gerações", includeWorst = FALSE, includeMean = FALSE, includeStdDev = FALSE,
                          minGen = 1, maxGen = max(which( all[[i]]$best != Inf ))
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
