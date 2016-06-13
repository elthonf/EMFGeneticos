
#BENCHMARK
prob1 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n33-k5.vrp", gamaOverCapacity = 50)  #SIM
prob2 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n80-k10.vrp", gamaOverCapacity = 50) #SIM
prob3 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/B-n38-k6.vrp", gamaOverCapacity = 50)  #SIM
#prob4 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/B-n68-k9.vrp", gamaOverCapacity = 50)
prob5 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/P-n21-k2.vrp", gamaOverCapacity = 50)  #SIM
#prob6 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/P-n70-k10.vrp", gamaOverCapacity = 50)

#Estatégia 1:
problema = prob1; popSize = 1000;
cvrp.exec01 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 1,
    clone = popSize * 0.3 -1,
    cloneAndMutate = popSize * 0.4,
    chromosomeRandFunc = cvrp.generate,
    #chromosomeRandFunc = cvrp.generate.bestInsertion,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    #mutationFunc = cvrp.mutate.srm,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)

#Estatégia 2:
problema = prob1; popSize = 1000;
cvrp.exec02 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 0,
    clone = popSize * 0.3,
    cloneAndMutate = popSize * 0.4,
    chromosomeRandFunc = cvrp.generate,
    #chromosomeRandFunc = cvrp.generate.bestInsertion,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    #mutationFunc = cvrp.mutate.srm,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)


#Estatégia 3:
problema = prob1; popSize = 100;
cvrp.exec03 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 1,
    clone = popSize * 0.3 -1,
    cloneAndMutate = popSize * 0.4,
    chromosomeRandFunc = cvrp.generate,
    #chromosomeRandFunc = cvrp.generate.bestInsertion,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    #mutationFunc = cvrp.mutate.srm,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)


#Estatégia 4:
problema = prob1; popSize = 10000;
cvrp.exec04 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 1,
    clone = popSize * 0.3 -1,
    cloneAndMutate = popSize * 0.4,
    chromosomeRandFunc = cvrp.generate,
    #chromosomeRandFunc = cvrp.generate.bestInsertion,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    #mutationFunc = cvrp.mutate.srm,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)
