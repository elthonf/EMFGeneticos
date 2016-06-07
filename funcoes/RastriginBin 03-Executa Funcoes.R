cat("Função de Rastrigin utlizando cromossomo binário");

#BENCHMARK
#Estatégia 1: População grande (2000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
rb.exec01 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 0,
    clone = 2000 * 0.4,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover1,
    mutationFunc = rb.mutate1,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)


#Estatégia 2: População pequena (200), sem elitismo. 50% de CrossOver simples, 40% clone (reprodução), 10% mutação simples
rb.exec02 <- EMF.Gen.Workflow(
    iters = 250,
    popSize = 200,
    crossOver = 200 * 0.5,
    elitism = 0,
    clone = 200 * 0.4,
    cloneAndMutate = 200 * 0.1,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover1,
    mutationFunc = rb.mutate1,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:200, mean=1, sd=(200/2)),
    verbose = FALSE)



#Estatégia 3: População enorme (20000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
rb.exec03 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 20000,
    crossOver = 20000 * 0.5,
    elitism = 0,
    clone = 20000 * 0.4,
    cloneAndMutate = 20000 * 0.1,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover1,
    mutationFunc = rb.mutate1,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:20000, mean=1, sd=(20000/2)),
    verbose = TRUE)

#Estatégia 4: População grande (2000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples. CrossOver UNIFORME
rb.exec04 <- EMF.Gen.Workflow(
    iters = 250,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 0,
    clone = 2000 * 0.4,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover2,
    mutationFunc = rb.mutate1,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)

#Estatégia 5: População grande (2000), SEM elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação trocando posicao. CrossOver 1 ponto
rb.exec05 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 0,
    clone = 2000 * 0.4,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover1,
    mutationFunc = rb.mutate2,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)


#Estatégia 6: População grande (2000), 1 de elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples. CrossOver  1 ponto
rb.exec06 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 1,
    clone = 2000 * 0.4 -1,
    cloneAndMutate = 2000 * 0.1,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover1,
    mutationFunc = rb.mutate1,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)


#Estatégia 7: População média (1000) com filhos sobrando (1500), 10 de elitismo. 100% de CrossOver, 20% clone (reprodução), 30% mutação com 10% de ratio. CrossOver de 1 pontos e colocando mutação em 50% dos filhos
rb.exec07 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 1000,
    crossOver = 1000 * 1.0, mutationChance = 0.50,
    elitism = 10,
    clone = 1000 * 0.2,
    cloneAndMutate = 1000 * 0.3,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover1,
    mutationFunc = rb.mutate1,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = FALSE)



#Estatégia 08: População grande (2000), 1 de elitismo. 50% de CrossOver uniforme, 20% clone (reprodução), 30% mutação de 1 gene apenas.
rb.exec08 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 2000 * 0.5,
    elitism = 10,
    clone = 2000 * 0.20,
    cloneAndMutate = 2000 * 0.30,
    chromosomeRandFunc = rb.generate,
    evalFunc = rb.fitness,
    crossOverFunc = rb.crossover1,
    mutationFunc = rb.mutate3,
    monitorFunc = rb.monitor,
    parentProb = dnorm(1:2000, mean=1, sd=(2000/2)),
    verbose = FALSE)


