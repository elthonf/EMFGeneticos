
#BENCHMARK
prob1 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n33-k5.vrp", gamaOverCapacity = 50)  #SIM
prob2 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n80-k10.vrp", gamaOverCapacity = 50) #SIM
prob3 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/B-n38-k6.vrp", gamaOverCapacity = 50)  #SIM
prob4 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/B-n68-k9.vrp", gamaOverCapacity = 50)
prob5 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/P-n21-k2.vrp", gamaOverCapacity = 50)  #SIM
prob6 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/P-n70-k10.vrp", gamaOverCapacity = 50)

#Estatégia 1: Instancia A! População média (1000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
problema = prob1;
problema$gamaOverCapacity = 50;
cvrp.exec01b <- EMF.Gen.Workflow(
    iters = 500,
    popSize = 1000,
    crossOver = 1000 * 0.2,
    elitism = 1,
    clone = 1000 * 0.3 -1,
    cloneAndMutate = 1000 * 0.5,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.caixeiro.batch,
    #mutationFunc = cvrp.mutate.composto,
    mutationFunc = cvrp.mutate.srm,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = TRUE)

#Estatégia 2: Instancia A! População grande (10000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
problema = prob1;
cvrp.exec02 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 10000,
    crossOver = 10000 * 0.5,
    elitism = 0,
    clone = 10000 * 0.4,
    cloneAndMutate = 10000 * 0.1,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.caixeiro.batch,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:10000, mean=1, sd=(10000/2)),
    verbose = TRUE)

#Estatégia 3: Instancia P! População grande (10000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
problema = prob5;
cvrp.exec03 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 10000,
    crossOver = 10000 * 0.5,
    elitism = 0,
    clone = 10000 * 0.4,
    cloneAndMutate = 10000 * 0.1,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.caixeiro.batch,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:10000, mean=1, sd=(10000/2)),
    verbose = TRUE)

#Estatégia 4: Instancia B! População média (1000), COM ELITISMO 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
problema = prob3;
cvrp.exec04 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 1000,
    crossOver = 1000 * 0.5,
    elitism = 1,
    clone = 1000 * 0.4 -1,
    cloneAndMutate = 1000 * 0.1,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = EMF.Gen.CrossOver.Simple,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:1000, mean=1, sd=(1000/2)),
    verbose = TRUE)


#Estatégia 5: Instancia de alta dimensionalidade! População grande (10000), sem elitismo. 50% de CrossOver, 40% clone (reprodução), 10% mutação simples
problema = prob2;
cvrp.exec05 <- EMF.Gen.Workflow(
    iters = 100,
    popSize = 10000,
    crossOver = 10000 * 0.5,
    elitism = 0,
    clone = 10000 * 0.4,
    cloneAndMutate = 10000 * 0.1,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = EMF.Gen.CrossOver.Simple,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:10000, mean=1, sd=(10000/2)),
    verbose = TRUE)


