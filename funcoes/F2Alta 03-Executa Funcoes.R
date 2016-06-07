cat("Função F2 de alta dimensionalidade e unimodal utlizando cromossomo decimal");

#BENCHMARK
#Estatégia 1: População média (1000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
f2.exec01 <- EMF.Gen.Workflow(
    iters = 300,
    popSize = 1000,
    crossOver = 1000 * 0.5,
    elitism = 0,
    clone = 1000 * 0.4,
    cloneAndMutate = 1000 * 0.1,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover1,
    mutationFunc = f2.mutate1,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = FALSE)

#Estatégia 2: População pequena (300), sem elitismo. 50% de CrossOver simples, 40% clone (reprodução), 10% mutação simples
f2.exec02 <- EMF.Gen.Workflow(
    iters = 300,
    popSize = 300,
    crossOver = 300 * 0.5,
    elitism = 0,
    clone = 300 * 0.4,
    cloneAndMutate = 300 * 0.1,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover1,
    mutationFunc = f2.mutate1,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:300, mean=1, sd=(300/2)),
    verbose = FALSE)

#Estatégia 3: População enorme (10000), sem elitismo. 50% de CrossOver simples, 40% clone (reprodução), 10% mutação simples
f2.exec03 <- EMF.Gen.Workflow(
    iters = 300,
    popSize = 10000,
    crossOver = 10000 * 0.5,
    elitism = 0,
    clone = 10000 * 0.4,
    cloneAndMutate = 10000 * 0.1,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover1,
    mutationFunc = f2.mutate1,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:10000, mean=1, sd=(10000/2)),
    verbose = FALSE)

#Estatégia 4: População média (1000), 1 de elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
f2.exec04 <- EMF.Gen.Workflow(
    iters = 300,
    popSize = 1000,
    crossOver = 1000 * 0.5,
    elitism = 1,
    clone = 1000 * 0.4,
    cloneAndMutate = 1000 * 0.1,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover1,
    mutationFunc = f2.mutate1,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = FALSE)




#Estatégia 5: População média (1000), sem elitismo. 50% de CrossOver UNIFORME, 40% clone (reprodução), 10% mutação simples
f2.exec05 <- EMF.Gen.Workflow(
    iters = 300,
    popSize = 1000,
    crossOver = 1000 * 0.5,
    elitism = 0,
    clone = 1000 * 0.2,
    cloneAndMutate = 1000 * 0.3,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover2,
    mutationFunc = f2.mutate1,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = FALSE)

#Estatégia 6: População média (1000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação com FATOR
f2.exec06 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = 1000,
    crossOver = 1000 * 0.5,
    elitism = 0,
    clone = 1000 * 0.2,
    cloneAndMutate = 1000 * 0.3,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover1,
    mutationFunc = f2.mutate2,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = FALSE)


#Estatégia 7: População média (1000), sem elitismo. 50% de CrossOver, 20% clone (reprodução), 30% mutação com FATOR
f2.exec07 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = 1000,
    crossOver = 1000 * 0.5,
    elitism = 1,
    clone = 1000 * 0.2 -1,
    cloneAndMutate = 1000 * 0.3,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover1,
    mutationFunc = f2.mutate2,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = FALSE)

#Estatégia 8: População grande (10000), sem elitismo. 50% de CrossOver UNIFORME, 20% clone (reprodução), 30% mutação com FATOR
f2.exec08 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = 10000,
    crossOver = 10000 * 0.5,
    elitism = 1,
    clone = 10000 * 0.2 -1,
    cloneAndMutate = 10000 * 0.3,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover2,
    mutationFunc = f2.mutate2,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:10000, mean=1, sd=(10000/2)),
    verbose = FALSE)


melhoresUltima = rbind( f2.exec01$lastPopulation[1:10,], f2.exec02$lastPopulation[1:10,], f2.exec03$lastPopulation[1:10,],
                        f2.exec04$lastPopulation[1:10,], f2.exec05$lastPopulation[1:10,], f2.exec06$lastPopulation[1:10,],
                        f2.exec07$lastPopulation[1:10,], f2.exec08$lastPopulation[1:10,] );
melhoresUltima = unique( melhoresUltima );

#Estatégia 9: Idem estratégia 8, porém iniciando com os 10 melhores indivíduos de cada execução
f2.exec09 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = 10000,
    suggestions = melhoresUltima,
    crossOver = 10000 * 0.5,
    elitism = 50,
    clone = 10000 * 0.1 -50,
    cloneAndMutate = 10000 * 0.4,
    chromosomeRandFunc = f2.generate,
    evalFunc = f2.fitness,
    crossOverFunc = f2.crossover2,
    mutationFunc = f2.mutate2,
    monitorFunc = f2.monitor,
    parentProb = dnorm(1:10000, mean=1, sd=(10000/2)),
    verbose = FALSE)
