
#Carrega os problemas
prob.An33k5 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n33-k5.vrp", gamaOverCapacity = 50)  #SIM
prob.An80k10 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/A-n80-k10.vrp", gamaOverCapacity = 50) #SIM
prob.Bn38k6 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/B-n38-k6.vrp", gamaOverCapacity = 50)  #SIM
prob.Pn21k2 = EMF.Gen.Cvrp.LoadProblem("tests/cvrp/P-n21-k2.vrp", gamaOverCapacity = 50)  #SIM

An33k5.repara = list()
Bn38k6.repara = list()
Pn21k2.repara = list()
An80k10.repara = list()


popSize = 1000;
for( i in 1:3 ){
    problema = prob.An33k5; #Setar qual problema será trabalhado!
    An33k5.repara[[i]] <- EMF.Gen.Workflow(
        iters = 1000,
        popSize = popSize,
        crossOver = popSize * 0.3,
        elitism = 1,
        clone = popSize * 0.3 -1,
        cloneAndMutate = popSize * 0.4,
        chromosomeRandFunc = cvrp.generate.simples,
        evalFunc = cvrp.evaluate,
        crossOverFunc = cvrp.crossover.repara,
        #mutationFunc = cvrp.mutate.composto,
        #mutationFunc = cvrp.mutate.srm,
        mutationFunc = cvrp.mutate.permut,
        monitorFunc = cvrp.monitor,
        parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
        verbose = FALSE);

    problema = prob.Bn38k6; #Setar qual problema será trabalhado!
    Bn38k6.repara[[i]] <- EMF.Gen.Workflow(
        iters = 1000,
        popSize = popSize,
        crossOver = popSize * 0.3,
        elitism = 1,
        clone = popSize * 0.3 -1,
        cloneAndMutate = popSize * 0.4,
        chromosomeRandFunc = cvrp.generate.simples,
        evalFunc = cvrp.evaluate,
        crossOverFunc = cvrp.crossover.repara,
        #mutationFunc = cvrp.mutate.composto,
        #mutationFunc = cvrp.mutate.srm,
        mutationFunc = cvrp.mutate.permut,
        monitorFunc = cvrp.monitor,
        parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
        verbose = FALSE);

    problema = prob.Pn21k2; #Setar qual problema será trabalhado!
    Pn21k2.repara[[i]] <- EMF.Gen.Workflow(
        iters = 1000,
        popSize = popSize,
        crossOver = popSize * 0.3,
        elitism = 1,
        clone = popSize * 0.3 -1,
        cloneAndMutate = popSize * 0.4,
        chromosomeRandFunc = cvrp.generate.simples,
        evalFunc = cvrp.evaluate,
        crossOverFunc = cvrp.crossover.repara,
        #mutationFunc = cvrp.mutate.composto,
        #mutationFunc = cvrp.mutate.srm,
        mutationFunc = cvrp.mutate.permut,
        monitorFunc = cvrp.monitor,
        parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
        verbose = FALSE);


    problema = prob.An80k10; #Setar qual problema será trabalhado!
    An80k10.repara[[i]] <- EMF.Gen.Workflow(
        iters = 1000,
        popSize = popSize,
        crossOver = popSize * 0.3,
        elitism = 1,
        clone = popSize * 0.3 -1,
        cloneAndMutate = popSize * 0.4,
        chromosomeRandFunc = cvrp.generate.simples,
        evalFunc = cvrp.evaluate,
        crossOverFunc = cvrp.crossover.repara,
        #mutationFunc = cvrp.mutate.composto,
        #mutationFunc = cvrp.mutate.srm,
        mutationFunc = cvrp.mutate.permut,
        monitorFunc = cvrp.monitor,
        parentProb = dnorm(1:popSize, mean=1, sd=(popSize/2)),
        verbose = FALSE);

}

rm(i);
