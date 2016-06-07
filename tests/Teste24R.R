library(EMFGeneticos) #library(ggplot2)  # detach(package:ggplot2)
rm(list=ls())

cat("Defining custom functions... \n")
#Função para gerar um chromossomo aleatório
vinte4R.generate <- function(){
    r = sample(1:24,24)
    return( r )
}
#Função para avaliar um chromossomo (Obj: MIN=0)
vinte4R.evaluate <- function(c){
    ret = 0
    size=length(c)[1]

    for(i in 1:(size-1)){
        for(j in (i+1):size){
            #Mesma linha
            if(c[i] == c[j])
                ret = ret + 2
            #Mesma diagonal decrescente
            else if( (j-i) == ( c[i] - c[j] ) )
                ret = ret + 2
            #Mesma diagonal crescente
            else if( (j-i) == ( c[j] - c[i] ) )
                ret = ret + 2
        }}
    return(ret)
}

vinte4R.print <- function(c){
    m = matrix(data=" -", nrow = length(c), ncol = length(c))
    for(i in 1:length(c))
    {
        m[c[i],i] = "X";
    }
    return ( m );
}

vinte4R.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0))
        EMF.Gen.Plot(r, title = "Solução parcial para as 24 rainhas", ylab = "Ameaças");

    if(r$best[r$generation] == 0 )  #STOP
    {
        EMF.Gen.Plot(r, title = "Solução para as 24 rainhas", ylab = "Ameaças");
        return (TRUE);
    }

    return (FALSE);
}

#oitoR.evaluate(oitoR.generate())


cat("Testing EMF.Gen.Workflow ... \n")
vinte4RainhasExec <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 1000,
    elitism = 1,
    clone = 500,
    cloneAndMutate = 500,
    mutationChance = 0.15,
    foreigners = 100,
    chromosomeRandFunc = vinte4R.generate,
    evalFunc = vinte4R.evaluate,
    monitorFunc = vinte4R.monitor, #Optional. remove for performance!
    verbose = TRUE)

# vinte4RainhasExec$best
# vinte4RainhasExec$lastPopulation[1,]
# vinte4RainhasExec$lastPopulation[dim(vinte4RainhasExec$lastPopulation)[1],]
# oitoR.print( vinte4RainhasExec$lastPopulation[1,] )


cat("Testing plots ... \n")
EMF.Gen.Plot(vinte4RainhasExec, title = "Solução para as 24 rainhas", ylab = "Ameaças")
EMF.Gen.Plot(vinte4RainhasExec, title = "Solução para as 24 rainhas", ylab = "Ameaças", includeWorst = TRUE)
EMF.Gen.Plot(vinte4RainhasExec, title = "Solução para as 24 rainhas", ylab = "Ameaças", includeMean = FALSE)
EMF.Gen.Plot(vinte4RainhasExec, title = "Solução para as 24 rainhas", ylab = "Ameaças", includeMean = FALSE, includeBestComparision = FALSE)
EMF.Gen.Plot(vinte4RainhasExec, title = "Solução para as 24 rainhas", ylab = "Ameaças", invert = TRUE)





cat("Testing \"Varredura\"")
