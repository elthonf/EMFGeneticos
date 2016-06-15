
#Se quiser recuperar, descomente a linha abaixo e nomeia a variável
#all = c( An33k5.rodada1, An33k5.rodada2, An33k5.rodada3);
#all = c( Bn38k6.rodada1, Bn38k6.rodada2, Bn38k6.rodada3);
#all = c( Pn21k2.rodada1, Pn21k2.rodada2, Pn21k2.rodada3);
#all = c( An80k10.rodada1, An80k10.rodada2, An80k10.rodada3);
all = c( An33k5.rodada1, An33k5.rodada2, An33k5.rodada3, Bn38k6.rodada1, Bn38k6.rodada2, Bn38k6.rodada3, Pn21k2.rodada1, Pn21k2.rodada2, Pn21k2.rodada3, An80k10.rodada1, An80k10.rodada2, An80k10.rodada3)


#Para exibir os gráficos na tela, escolha o índice!
indice = 1;
EMF.Gen.Plot(all[[indice]], includeWorst = TRUE, title = paste( "Solução para CVRP com operadores específicos. exec.:", indice), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 1, maxGen = max(which( all[[indice]]$best != Inf )))
rm(indice);

#Variações para o relatório:

EMF.Gen.Plot(An33k5.rodada2[[1]], includeWorst = TRUE,
             title = paste( "CVRP[An33k5] com operadores específicos. Estr.:", 1), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 2, maxGen = 1000)
EMF.Gen.Plot(An33k5.rodada1[[1]], includeWorst = FALSE,
             title = paste( "CVRP[An33k5] com operadores específicos. Estr.:", 1), ylab = "Fitness", xlab = "Gerações",
             includeMean = FALSE, includeStdDev = FALSE,
             minGen = 1, maxGen = 1000)

EMF.Gen.Plot(Bn38k6.rodada2[[2]], includeWorst = FALSE,
             title = paste( "CVRP[Bn38k6] com operadores específicos. Estr.:", 2), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = FALSE,
             minGen = 1, maxGen = 1000)

EMF.Gen.Plot(An33k5.rodada1[[5]], includeWorst = FALSE,
             title = paste( "CVRP[An33k5] com operadores específicos. Estr.:", 5), ylab = "Fitness", xlab = "Gerações",
             includeMean = FALSE, includeStdDev = FALSE,
             minGen = 1, maxGen = 1000)

#SRM Fazendo efeito!
EMF.Gen.Plot(An80k10.rodada2[[6]], includeWorst = TRUE,
             title = paste( "CVRP[An80k10] com operadores específicos. Estr.:", 6), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 1, maxGen = 200)


#Imprimir rotas em formato visual para o artigo
An33k5.rodada1[[8]]$lastPopulation[1,]
Bn38k6.rodada1[[8]]$lastPopulation[1,]
Pn21k2.rodada1[[3]]$lastPopulation[1,]
An80k10.rodada1[[8]]$lastPopulation[1,]



#Grava os logs e imagens
all.frame = data.frame();


for(i in 1:length(all)){
    temp = all[[i]];

    istring = toString(i);
    if(i < 10) istring = paste("0", istring, sep="");

    EMF.Gen.LogStatistics(execData = all[[i]],
                          execName = paste("CVRP operadores específicos ", istring, sep=""),
                          fileName = paste("cvrpEspecificos", istring, sep=""),
                          title = paste("CVRP com operadores específicos. exec.:", i),
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
