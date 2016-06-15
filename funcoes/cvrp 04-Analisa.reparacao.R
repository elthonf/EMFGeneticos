#Se quiser recuperar, descomente a linha abaixo e nomeia a variável
all = c( An33k5.repara, Bn38k6.repara, Pn21k2.repara, An80k10.repara);


#Para exibir os gráficos completos na tela, escolha um índice de 1 a 12!
indice = 1;
EMF.Gen.Plot(all[[indice]], includeWorst = TRUE, title = paste( "Solução para CVRP com operadores específicos. exec.:", indice), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 1, maxGen = max(which( all[[indice]]$best != Inf )))
rm(indice);

#Variações para o relatório:

EMF.Gen.Plot(An33k5.repara[[1]], includeWorst = TRUE,
             title = paste( "CVRP[An33k5] com operador de correção. exec.:", 1), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE, includeBestComparision = FALSE,
             minGen = 1, maxGen = 75)
EMF.Gen.Plot(Bn38k6.repara[[1]], includeWorst = TRUE,
             title = paste( "CVRP[Bn38k6] com operadores de correção. exec.:", 1), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 1, maxGen = 1000)
EMF.Gen.Plot(Pn21k2.repara[[3]], includeWorst = TRUE,
             title = paste( "CVRP[Pn21k2] com operadores de correção. exec.:", 3), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 1, maxGen = 1000)
EMF.Gen.Plot(An80k10.repara[[3]], includeWorst = TRUE,
             title = paste( "CVRP[An80k10] com operadores de correção. exec.:", 3), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 1, maxGen = 1000)

#Imprimir rotas em formato visual, para o relatório
An33k5.repara[[1]]$lastPopulation[1,]
Bn38k6.repara[[1]]$lastPopulation[1,]
Pn21k2.repara[[3]]$lastPopulation[1,]
An80k10.repara[[2]]$lastPopulation[1,]


#Grava os logs e imagens
all.frame = data.frame();


for(i in 1:length(all)){
    temp = all[[i]];

    istring = toString(i);
    if(i < 10) istring = paste("0", istring, sep="");

        EMF.Gen.LogStatistics(execData = all[[i]],
                              execName = paste("CVRP operadores correcao ", istring, sep=""),
                              fileName = paste("cvrpCorrecao", istring, sep=""),
                              title = paste("CVRP com operadores de correção. exec.:", i),
                              ylab = "Fitness", xlab = "Gerações", includeWorst = TRUE, includeMean = TRUE, includeStdDev = TRUE,
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
                        min(temp$best[1:100], na.rm = TRUE),
                        min(temp$best, na.rm = TRUE),
                        which ( temp$best == min(temp$best, na.rm = TRUE) )[1]
                      )
    );
    rm(temp);
    rm(istring);
}
rm(i);

colnames(all.frame) <- c("Execução", "População", "Iterações Plan", "Iterações Exec", "CrossOver", "Chance Mutação Cross", "Clone", "Mutação", "Elitismo", "Melhor em 100", "Melhor Resultado", "Geração Melhor");


rm(all);

View(all.frame);
