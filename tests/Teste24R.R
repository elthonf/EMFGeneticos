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

#oitoR.evaluate(oitoR.generate())


cat("Testing EMF.Gen.Workflow ... \n")
oitoRainhasExec2 <- EMF.Gen.Workflow(
    iters = 150,
    popSize = 2000,
    crossOver = 1000,
    elitism = 1,
    clone = 500,
    cloneAndMutate = 500,
    mutationChance = 0.15,
    foreigners = 100,
    chromosomeRandFunc = oitoR.generate,
    evalFunc = oitoR.evaluate,
    verbose = TRUE)

# oitoRainhasExec$best
# oitoRainhasExec$lastPopulation[1,]
# oitoRainhasExec$lastPopulation[dim(oitoRainhasExec$lastPopulation)[1],]
# oitoR.print( oitoRainhasExec$lastPopulation[1,] )


cat("Testing plots ... \n")
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças")
EMF.Gen.Plot(oitoRainhasExec2, title = "Solução para as oito rainhas", ylab = "Ameaças")
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", includeWorst = TRUE)
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", includeWorst = FALSE, includeMean = FALSE)
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", includeWorst = FALSE, includeMean = FALSE, includeBestComparision = FALSE)
EMF.Gen.Plot(oitoRainhasExec, title = "Solução para as oito rainhas", ylab = "Ameaças", invert = TRUE)


cat("Testing EMF.Gen.CrossOver.Simple ... \n")
p1 = oitoR.generate()
p2 = oitoR.generate()
c = EMF.Gen.CrossOver.Simple(p1, p2)
p1
p2
c[2,]
c[1,]

cat("Testing EMF.Gen.Mutate.Simple ... \n")
original = oitoR.generate()
newSample = oitoR.generate()

mutant = EMF.Gen.Mutate.Simple(original, mutationRate = 0.30, chromosomeRandFunc = oitoR.generate)
mutant = EMF.Gen.Mutate.Simple(original, mutationRate = 0.40)
