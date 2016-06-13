
#BENCHMARK
prob1 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n33-k5.vrp", gamaOverCapacity = 50)  #SIM
prob2 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n80-k10.vrp", gamaOverCapacity = 50) #SIM
prob3 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/B-n38-k6.vrp", gamaOverCapacity = 50)  #SIM
#prob4 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/B-n68-k9.vrp", gamaOverCapacity = 50)
prob5 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/P-n21-k2.vrp", gamaOverCapacity = 50)  #SIM
#prob6 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/P-n70-k10.vrp", gamaOverCapacity = 50)

problema = prob5; #Setar qual problema será trabalhado.


cat( "Estatégia 1: População média: 1000" );
popSize = 1000;
cvrp.exec01 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 1,
    clone = popSize * 0.3 -1,
    cloneAndMutate = popSize * 0.4,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    #mutationFunc = cvrp.mutate.srm,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)

cat( "Estatégia 2: Remove o elitismo" );
popSize = 1000;
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


cat( "Estatégia 3: População pequena: 100" );
popSize = 100;
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


cat( "Estatégia 4: População enorme: 10.000" );
popSize = 10000;
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


cat( "Estatégia 5: Aumenta cross e diminui mutação" );
popSize = 1000;
cvrp.exec05 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.5,
    elitism = 1,
    clone = popSize * 0.3 -1,
    cloneAndMutate = popSize * 0.2,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    #mutationFunc = cvrp.mutate.srm,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)

cat( "Estatégia 6: Troca tipo de mutação" );
popSize = 1000;
cvrp.exec06 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 1,
    clone = popSize * 0.3 -1,
    cloneAndMutate = popSize * 0.4,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    mutationFunc = cvrp.mutate.srm,
    #mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)

cat( "Estatégia 7: Aumenta mutação e diminui clonagem" );
popSize = 1000;
cvrp.exec07 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 1,
    clone = popSize * 0.1 -1,
    cloneAndMutate = popSize * 0.6,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    #mutationFunc = cvrp.mutate.srm,
    mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)

cat( "Estatégia 8: Melhores parametros: SRM, aumento de mutação, elitismo, etc." );
popSize = 1000;
cvrp.exec08 <- EMF.Gen.Workflow(
    iters = 1000,
    popSize = popSize,
    crossOver = popSize * 0.3,
    elitism = 1,
    clone = popSize * 0.1 -1,
    cloneAndMutate = popSize * 0.6,
    chromosomeRandFunc = cvrp.generate,
    evalFunc = cvrp.evaluate,
    crossOverFunc = cvrp.crossover.pmx.batch,
    #mutationFunc = cvrp.mutate.composto,
    mutationFunc = cvrp.mutate.srm,
    #mutationFunc = cvrp.mutate.permut,
    monitorFunc = cvrp.monitor,
    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
    verbose = FALSE)


Pn21k2.rodada1 = list( cvrp.exec01, cvrp.exec02, cvrp.exec03, cvrp.exec04, cvrp.exec05, cvrp.exec06, cvrp.exec07, cvrp.exec08);
#Pn21k2.rodada2 = list( cvrp.exec01, cvrp.exec02, cvrp.exec03, cvrp.exec04, cvrp.exec05, cvrp.exec06, cvrp.exec07, cvrp.exec08);
#Pn21k2.rodada3 = list( cvrp.exec01, cvrp.exec02, cvrp.exec03, cvrp.exec04, cvrp.exec05, cvrp.exec06, cvrp.exec07, cvrp.exec08);
