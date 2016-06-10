#Exibe os gráficos na tela
EMF.Gen.Plot(rb.exec01, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 1", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(rb.exec02, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 2", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(rb.exec03, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 3", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(rb.exec04, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 4", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(rb.exec05, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 5", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(rb.exec06, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 6", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(rb.exec07, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 7", ylab = "Fitness", xlab = "Gerações")
EMF.Gen.Plot(rb.exec08, invert = TRUE, includeWorst = TRUE, title = "Solução para Rastrigin com 128 bits exec.: 8", ylab = "Fitness", xlab = "Gerações")


#Grava os logs e imagens
rb.all = list( rb.exec01, rb.exec02, rb.exec03, rb.exec04, rb.exec05, rb.exec06, rb.exec07, rb.exec08);
rb.all.frame = data.frame();


for(i in 1:length(rb.all)){
    temp = rb.all[[i]];

    istring = toString(i);
    if(i < 10) istring = paste("0", istring, sep="");

    EMF.Gen.LogStatistics(execData = rb.all[[i]],
                          execName = paste("Rastrigin Binário Execução ", istring, sep=""),
                          fileName = paste("rastriginBin128b", istring, sep=""),
                          invert = TRUE,
                          title = paste("Solução para Rastrigin com 128 bits exec.:", i),
                          ylab = "Fitness", xlab = "Gerações", includeWorst = TRUE
    );

    rb.all.frame = rbind(rb.all.frame,
                         c(i,
                           temp$input.params$popSize,
                           temp$input.params$iters,
                           temp$generation,
                           temp$input.params$crossOver,
                           temp$input.params$mutationChance,
                           temp$input.params$clone,
                           temp$input.params$cloneAndMutate,
                           temp$input.params$elitism,
                           min(temp$best, na.rm = TRUE) *-1,
                           which ( temp$best == min(temp$best, na.rm = TRUE) )[1]
                           )
                         );
    rm(temp);
    rm(istring);
}
rm(i);

colnames(rb.all.frame) <- c("Execução", "População", "Iterações Plan", "Iterações Exec", "CrossOver", "Chance Mutação Cross", "Clone", "Mutação", "Elitismo", "Melhor Resultado", "Geração Melhor");


rm(rb.all);

View(rb.all.frame);
