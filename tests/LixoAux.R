

cat("Testing EMF.Gen.Workflow ... \n")
vinte4RainhasExec8 <- EMF.Gen.Workflow(
    iters = 300,
    popSize = 2000,
    crossOver = 5000,
    elitism = 1,
    clone = 500,
    cloneAndMutate = 500,
    mutationChance = 0.15,
    foreigners = 100,
    chromosomeRandFunc = vinte4R.generate,
    evalFunc = vinte4R.evaluate,
    monitorFunc = vinte4R.monitor, #Optional. remove for performance!
    verbose = TRUE, distinction = TRUE)



cat("Testing EMF.Gen.Workflow ... \n") #################
caixeiroExec8 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 5000,
    elitism = 1,
    clone = 1000,
    cloneAndMutate = 300,
    mutationChance = 0.15,
    foreigners = 100,
    chromosomeRandFunc = caixeiro.generate,
    evalFunc = caixeiro.evaluate,
    crossOverFunc = caixeiro.crossover,
    mutationFunc = caixeiro.mutate,
    monitorFunc = caixeiro.monitor, #Remover para performance
    verbose = TRUE, distinction = TRUE)


EMF.Gen.Plot(caixeiroExec6, title = "Solução para o caixeiro viajante", ylab = "Distância")
EMF.Gen.Plot(caixeiroExec8, title = "Solução para o caixeiro viajante", ylab = "Distância")


