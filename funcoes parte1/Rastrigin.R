cat("Função de Rastrigin");
library(EMFGeneticos); #library(ggplot2)  # para o caso de não utilizar o pacote, pelo menos será necessário chamar o ggplot2
rm(list=ls()); cat("\014"); #Limpa o console enviando um ctrl + l

#Função para gerar um chromossomo aleatório, vetor com 2 posições (x e y)
rastrigin.generate <- function(){
    r = runif(2, min=-5, max=5);
    return( r );
}

#Função para avaliar um chromossomo (Obj: MIN=0). Observação: não há a inversão final do -z
rastrigin.fitness <- function(c){
    x = c[1]; y = c[2];

    zx = x^2 -10*cos(2*pi*x) +10;
    zy = y^2 -10*cos(2*pi*y) +10;
    z = zx + zy;
    return(z);
}

#Função de mutação simples. Um dos genes do cromossomo é alterado.
rastrigin.mutate1 <- function(original, chromosomeRandFunc){
    i = sample(1:2, 1);

    ret = original;
    ret[i] = runif(1, min=-5, max=5);
    return(ret);
}

#Função de mutação "direcionada". O pior dos genes é alterado para um valor "melhorado".
rastrigin.mutate2 <- function(original, chromosomeRandFunc){
    m = max(abs(original));
    i = which(abs(original)==m);

    ret = original;
    ret[i] = runif(1, min=-m, max=m); #O gene a sofrer mutação, será melhorado, entre o valor anterior e seu oposto
    return(ret);
}

#Função de mutação que altera uma dos cromossomos em 10% do valor para cima ou para baixo.
rastrigin.mutate3 <- function(original, chromosomeRandFunc){
    i = sample(1:2, 1);

    ret = original;
    ret[i] = ret[i] * sample( c(0.9, 1.1), 1 );
    return(ret);
}

#Função para monitorar a execução e imprimir resultado parcial. Parada em fitness <= 0!
rastrigin.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0))
        EMF.Gen.Plot(r, title = "Solução parcial para Rastrigin", ylab = "Fitness", xlab = "Geração", invert = TRUE, includeWorst = TRUE);

    if((r$lastEvaluations[r$generation] <= 0)){
        EMF.Gen.Plot(r, title = "Encontrado ponto ótimo global para Rastrigin", ylab = "Fitness", xlab = "Geração", invert = TRUE, includeWorst = TRUE);
        return (TRUE);
    }

    return (FALSE);
}

# ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
#                               EXECUÇÃO DAS FUNÇÕES DE OTIMIZAÇÃO
# ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

#Estatégia 1: População pequena (200), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
rastrigin.exec01 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 200,
    crossOver = 200 * 0.5,
    elitism = 0,
    clone = 200 * 0.4,
    cloneAndMutate = 200 * 0.1,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate1,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:200, mean=1, sd=(200/2)),
    verbose = FALSE)

#Estatégia 2: População grande (2000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
rastrigin.exec02 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 0,
    clone = 2000 * 0.4,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate1,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)

#Estatégia 3: População grande (2000), com 1 cromossomo de elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
rastrigin.exec03 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 1,
    clone = 2000 * 0.4 -1,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate1,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)

#Estatégia 4: População grande (2000), com 1 cromossomo de elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação "direcionada"
rastrigin.exec04 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 1,
    clone = 2000 * 0.4 -1,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate2,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)

#Estatégia 5: População pequena (200), com 1 cromossomo de elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação "direcionada"
rastrigin.exec05 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 200,
    crossOver = 200 * 0.5,
    elitism = 1,
    clone = 200 * 0.4 -1,
    cloneAndMutate = 200 * 0.1,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate2,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:200, mean=1, sd=(200/2)),
    verbose = FALSE)


#Estatégia 6: População grande (2000), com 1 cromossomo de elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação "variando em 10% do valor"
rastrigin.exec06 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 1,
    clone = 2000 * 0.4 -1,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate3,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)

#Estatégia 7: População pequena (200), com 1 cromossomo de elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação "variando em 10% do valor"
rastrigin.exec07 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 200,
    crossOver = 200 * 0.5,
    elitism = 1,
    clone = 200 * 0.4 -1,
    cloneAndMutate = 200 * 0.1,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate3,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:200, mean=1, sd=(200/2)),
    verbose = FALSE)

#Estatégia 8: População grande (200) com muitos descendentes, com 1 cromossomo de elitismo. 100% de CrossOver, 30% clone (reprodução), 30% mutação "simples"
rastrigin.exec08 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 1,
    elitism = 1,
    clone = 2000 * 0.3 -1,
    cloneAndMutate = 2000 * 0.3,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate1,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)

#Estatégia 9: População grande (200) com muitos descendentes, com 1 cromossomo de elitismo. 100% de CrossOver, 30% clone (reprodução), 30% mutação "simples", mantendo apenas os únicos
rastrigin.exec09 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 1,
    elitism = 1,
    clone = 2000 * 0.3 -1,
    cloneAndMutate = 2000 * 0.3,
    chromosomeRandFunc = rastrigin.generate,
    evalFunc = rastrigin.fitness,
    mutationFunc = rastrigin.mutate1,
    monitorFunc = rastrigin.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    unique = TRUE,
    verbose = FALSE)



# ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
#                               EXPORTAÇÃO DOS LOGS DAS EXECUÇÕES
# ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

# i = EMF.Gen.Plot(rastrigin.exec01, title = "Solução final para Rastrigin", ylab = "Fitness", xlab = "Geração", invert = TRUE, includeWorst = TRUE);
# rm(i)

#plot(1:5)
#print(i);
