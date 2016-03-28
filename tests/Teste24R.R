library(EMFGeneticos)
library(ggplot2)  # detach(package:ggplot2)
rm(list=ls())

cat("Defining custom functions... \n")
#Função para gerar um chromossomo aleatório
oitoR.generate <- function(){
    r = sample(1:24,24)
    return( r )
}
#Função para avaliar um chromossomo (Obj: MIN=0)
oitoR.evaluate <- function(c){
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

oitoR.print <- function(c){
    m = matrix(data=" -", nrow = 8, ncol = 8)
    for(i in 1:length(c))
    {
        m[c[i],i] = "X";
    }
    return ( m );
}

oitoR.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0))
        EMF.Gen.Plot(r, title = "Solução para as oito rainhas", ylab = "Ameaças");

    if(r$best[r$generation] == 0 )  #STOP
    {
        EMF.Gen.Plot(r, title = "Solução para as oito rainhas", ylab = "Ameaças");
        return (TRUE);
    }

    return (FALSE);
}

#oitoR.evaluate(oitoR.generate())


cat("Testing EMF.Gen.Workflow ... \n")
oitoRainhasExec <- EMF.Gen.Workflow(
    iters = 10,
    popSize = 2000,
    crossOver = 1000,
    elitism = 1,
    clone = 500,
    cloneAndMutate = 500,
    mutationChance = 0.15,
    foreigners = 100,
    chromosomeRandFunc = oitoR.generate,
    evalFunc = oitoR.evaluate,
    #monitorFunc = oitoR.monitor, #Optional. remove for performance!
    verbose = TRUE)

# oitoRainhasExec$best
# oitoRainhasExec$lastPopulation[1,]
# oitoRainhasExec$lastPopulation[dim(oitoRainhasExec$lastPopulation)[1],]
# oitoR.print( oitoRainhasExec$lastPopulation[1,] )


cat("Testing plots ... \n")
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças")
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", includeWorst = TRUE)
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", includeMean = FALSE)
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", includeMean = FALSE, includeBestComparision = FALSE)
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", invert = TRUE)

