library(EMFGeneticos)
library(ggplot2)  # detach(package:ggplot2)
rm(list=ls()) #limpa

############# #############  ############# 1 : CARREGA DADOS!
cat("loading file")
file = "tests/caixeiro/Towns2012.csv"

towns = read.delim(file, header = FALSE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "",
                    col.names = c("seq", "name", "lat", "lon")) #, colClasses=c("numeric", "character", "numeric", "numeric"))

distanceOnEarth <- function( lat1, lon1, lat2, lon2)
{
    return ( sqrt ( ( lat1 - lat2) ^ 2 + ( lon1 - lon2) ^ 2 ) );
}

distance = matrix(  nrow = dim(towns)[1], ncol = dim(towns)[1])

for(l in 1:dim(towns)[1])
    for(c in 1:dim(towns)[1])
        distance[l, c] = distanceOnEarth(towns$lat[l], towns$lon[l], towns$lat[c], towns$lon[c]);

rm(distanceOnEarth)
rm(l)
rm(c)
rm(file)


############# #############  ############# 2 : DEFINE CUSTOM FUNCTIONS!
cat("Defining custom functions... \n")
#Função para gerar um cromossomo aleatório
caixeiro.generate <- function(){
    r = sample(1:dim(towns)[1],dim(towns)[1])
    return( r )
}

#Função para avaliar um cromossomo
caixeiro.evaluate <- function(c){
    ret = 0
    size=length(c)[1]

    cAux = c( c, c[1]) #Adiciona a volta à origem (opicional)

    for(i in 1:size){
        ret = ret + distance[ cAux[i], cAux[i+1]]
    }

    return(ret)
}

caixeiro.crossover<- function( p1, p2  )
{
    sizeP1 = length(p1);
    sizeP2 = length(p2);

    if(sizeP1 != sizeP2)
        stop("Parents must have same size.");

    crossOverPoint1 = sample(1:(sizeP1-1),1);
    crossOverPoint2 = crossOverPoint1;
    while(crossOverPoint2 == crossOverPoint1)
    {
        crossOverPoint2 = sample(1:(sizeP1-1),1);
    }
    cop1 = min(crossOverPoint1, crossOverPoint2);
    cop2 = max(crossOverPoint1, crossOverPoint2);
    #cat("{");cat(cop1);cat(",");cat(cop2);cat("}");

    #O cromossomo será dividido em 3 pontos: left (esquerda), core (centro), right (direito)
    #O Core é gerado primeiro e cada cromossomo seguinte terá cross se já não estiver no Core.

    child1 = c( rep( NA, cop1 ), p1[(cop1+1) : cop2], rep( NA, sizeP1 - cop2));
    child2 = c( rep( NA, cop1 ), p2[(cop1+1) : cop2], rep( NA, sizeP1 - cop2));

    child2[1:cop1] = p1[!(p1 %in% child2)][1:cop1] #Adiciona o que sobrou em child2, na ordem de p1
    child2[(cop2+1) : sizeP1] = p1[!(p1 %in% child2)]

    child1[1:cop1] = p2[!(p2 %in% child1)][1:cop1] #Adiciona o que sobrou em child1, na ordem de p2
    child1[(cop2+1) : sizeP1] = p2[!(p2 %in% child1)]

    ret = matrix( c(child1, child2), nrow = 2, ncol=sizeP1, byrow = TRUE );
    return ( ret );
}

caixeiro.menortrip <- function()
{
    trip = rep(NA, dim(towns)[1])
    trip[1] = 25
    i = 2
    while(i <= dim(towns)[1] )
    {
        destinations = distance[trip[i-1],] #Identify all destinations
        destinations = sort(destinations, index=TRUE) #Sort destinations
        availableDestinations = destinations$ix[ !(destinations$ix %in% trip) ]  #Remove visited towns

        trip[i] = availableDestinations[1]
        i = i +1
    }
    return ( trip );
}

caixeiro.mutate <- function(
    original,
    mutationRate = 0.10,
    chromosomeRandFunc=NULL  ){

    #Função padrão, porém omitindo a função que gera um cromossomo
    return ( EMF.Gen.Mutate.Simple(original = original, chromosomeRandFunc = NULL ));
}

caixeiro.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0))
        EMF.Gen.Plot(r, title = "Solução parcial do caixeiro viajante", ylab = "Distância");

    cat( paste( "[Monitor Message] Generation: ", r$generation, ", best: ", r$best[ r$generation ], "\n"));
    return (FALSE);
}

############# #############  ############# 3 : EXECUTA!


cat("Testing EMF.Gen.Workflow ... \n") #################
caixeiroExec6 <- EMF.Gen.Workflow(
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
    verbose = FALSE)

cat("Testing plots ... \n")
EMF.Gen.Plot(caixeiroExec, title = "Solução para o caixeiro viajante", ylab = "Distância")
EMF.Gen.Plot(caixeiroExec2, title = "Solução para o caixeiro viajante", ylab = "Distância")
EMF.Gen.Plot(caixeiroExec3, title = "Solução para o caixeiro viajante", ylab = "Distância")
EMF.Gen.Plot(caixeiroExec4, title = "Solução para o caixeiro viajante", ylab = "Distância")
EMF.Gen.Plot(caixeiroExec5, title = "Solução para o caixeiro viajante", ylab = "Distância")
EMF.Gen.Plot(caixeiroExec6, title = "Solução para o caixeiro viajante", ylab = "Distância")
