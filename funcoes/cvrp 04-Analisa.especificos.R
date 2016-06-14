#all = list( cvrp.exec01, cvrp.exec02, cvrp.exec03, cvrp.exec04, cvrp.exec05, cvrp.exec06, cvrp.exec07, cvrp.exec08);
#Se quiser armazenar, descomente a linha abaixo e nomeia a variável!
#rodadaX = all;
#Se quiser recuperar, descomente a linha abaixo e nomeia a variável
all = Bn38k6.rodada1;

#LOG: rodada1 - 1 a 6 do problema A-n33-k5
#LOG: rodada2 - 1 a 6 do problema A-n33-k5


#Exibe os gráficos na tela, escolha o índice!
indice = 1;
EMF.Gen.Plot(all[[indice]], includeWorst = TRUE, title = paste( "Solução para CVRP com operadores específicos. exec.:", indice), ylab = "Fitness", xlab = "Gerações",
             includeMean = TRUE, includeStdDev = TRUE,
             minGen = 1, maxGen = max(which( all[[indice]]$best != Inf )))
rm(indice);


#Variações

#Grava os logs e imagens
all.frame = data.frame();


for(i in 1:length(all)){
    temp = all[[i]];

    istring = toString(i);
    if(i < 10) istring = paste("0", istring, sep="");

#     EMF.Gen.LogStatistics(execData = all[[i]],
#                           execName = paste("CVRP operadores específicos ", istring, sep=""),
#                           fileName = paste("cvrpClassicos", istring, sep=""),
#                           title = paste("Solução para CVRP com operadores específicos. exec.:", i),
#                           ylab = "Fitness", xlab = "Gerações", includeWorst = FALSE, includeMean = FALSE, includeStdDev = FALSE,
#                           minGen = 1, maxGen = max(which( all[[i]]$best != Inf ))
#     );

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
