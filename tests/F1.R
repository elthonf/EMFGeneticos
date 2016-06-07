cat("Entrega: Funções F1, F2 e F3")
library(EMFGeneticos) #library(ggplot2)  # detach(package:ggplot2)
rm(list=ls())

D = 30
cat("Defining custom functions... \n")
#Função para gerar um chromossomo aleatório com D dimensões entre [-100 e 100]
f1.generate <- function(){
    r = runif(D, min=-100, max=100);
    return( r );
}
#Função para gerar um chromossomo aleatório com D dimensões entre [-500 e 500]
f3.generate <- function(){
    r = runif(D, min=-500, max=500);
    return( r );
}

#Função para avaliar um chromossomo em F1
f1.evaluate <- function(c){
    r = sum(c^2);
    return(-r);
}
#Função para avaliar um chromossomo em F2
f2.evaluate <- function(c){
    r = sum((c + 0.5)^2);
    return(-r);
}

#Função para avaliar um chromossomo em F3
f3.evaluate <- function(c){
    r = sum( -c * sin(sqrt(abs(c))) );
    return(-r);
}

#Função para monitorar a execução e imprimir resultado parcial
fx.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0))
        EMF.Gen.Plot(r, title = "Solução parcial da Função", ylab = "Fitness", invert = TRUE, includeWorst = TRUE);

    return (FALSE);
}





cat("Tudo pronto para iniciar o processamento de F1 ... \n")
f1.exec01 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 200,
    crossOver = 100,
    elitism = 1,
    clone = 50,
    cloneAndMutate = 50,
    mutationChance = 0.15,
    foreigners = 100,
    unique = TRUE,
    chromosomeRandFunc = f1.generate,
    evalFunc = f1.evaluate,
    monitorFunc = fx.monitor, #Opcional achei legal exibir de 10 em 10!
    verbose = FALSE)

EMF.Gen.Plot(f1.exec01, title = "Solução final para f1", ylab = "Fitness", invert = TRUE, includeWorst = TRUE);

cat("Tudo pronto para iniciar o processamento de F2 ... \n")
f2.exec01 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 200,
    crossOver = 100,
    elitism = 1,
    clone = 50,
    cloneAndMutate = 50,
    mutationChance = 0.15,
    foreigners = 100,
    unique = TRUE,
    chromosomeRandFunc = f1.generate,
    evalFunc = f2.evaluate,
    monitorFunc = fx.monitor, #Opcional remover para performance!
    verbose = FALSE)

EMF.Gen.Plot(f2.exec01, title = "Solução final para f2", ylab = "Fitness", invert = TRUE, includeWorst = TRUE);

cat("Tudo pronto para iniciar o processamento de F3 ... \n");
f3.exec01 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 500,
    crossOver = 100,
    elitism = 1,
    clone = 50,
    cloneAndMutate = 50,
    mutationChance = 0.15,
    foreigners = 100,
    chromosomeRandFunc = f3.generate,
    evalFunc = f3.evaluate,
    monitorFunc = fx.monitor, #Opcional remover para performance!
    verbose = FALSE);

EMF.Gen.Plot(f3.exec01, title = "Solução final para f3", ylab = "Fitness", invert = TRUE, includeWorst = TRUE);
EMF.Gen.Plot(f3.exec01, title = "Solução final para f3", ylab = "Fitness", invert = FALSE, includeWorst = TRUE);
?readline

interactive()

a = f3.generate()
f3.evaluate(a)
c=a
sqrt(c)

