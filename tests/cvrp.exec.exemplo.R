
#BENCHMARK
#Estatégia 1: População média (1000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
cvrp.exec01 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 1000,
    crossOver = 1000 * 0.5,
    elitism = 1,
    clone = 1000 * 0.4 -1,
    cloneAndMutate = 1000 * 0.1,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossoverBatch,
    mutationFunc = cvrp.mutate2,
#    monitorFunc = f1.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = TRUE)


EMF.Gen.Plot(cvrp.exec01, includeWorst = TRUE, title = "Teste")


cvrp.exec02 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 10000,
    crossOver = 10000 * 0.5,
    elitism = 1,
    clone = 10000 * 0.4 -1,
    cloneAndMutate = 10000 * 0.1,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossoverBatch,
    mutationFunc = cvrp.mutate2,
    #    monitorFunc = f1.monitor,
    parentProb = dnorm(1:10000, mean=1, sd=(10000/2)),
    verbose = TRUE)


EMF.Gen.Plot(cvrp.exec02, includeWorst = TRUE, title = "Teste")
