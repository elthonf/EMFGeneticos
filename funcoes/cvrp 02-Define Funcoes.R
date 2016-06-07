library(EMFGeneticos) #library(ggplot2)  # detach(package:ggplot2)
rm(list=ls()) #limpa

############# #############  ############# 0 : CARREGA O PROBLEMA!
#problema <- EMF.Gen.Cvrp.LoadProblem(filename = "tests/cvrp/A-n37-k6.vrp")
#problema1 <- EMF.Gen.Cvrp.LoadProblem(filename = "tests/cvrp/A-n37-k6.vrp")
#problema2 <- EMF.Gen.Cvrp.LoadProblem(filename = "tests/cvrp/P-n70-k10.vrp")
#problema3 <- EMF.Gen.Cvrp.LoadProblem(filename = "tests/cvrp/P-n70-k10.vrp")
#problema3 <- EMF.Gen.Cvrp.LoadProblem(filename = "tests/cvrp/P-n70-k10.vrp")



############# #############  ############# 1 : DEFINE FUNCOES DE CRIACAO E FITNESS!
cvrp.generate <- function(){
    #Função para gerar um cromossomo aleatório
    r = sample( problema$cromossomoAmostra );
    return( r );
}

cvrp.evaluateTruck <- function(cp){
    #Se o veiculo estiver vazio, retorna ZERO.
    if( length(cp) == 1 )
        return ( 0 );

    #Adiciona a origem (deposito) e o destino final (deposito)
    rota = c( problema$deposito, cp[2:length(cp)], problema$deposito);
    #Calcula o custo das distancias
    custo = 0;
    for(i in 1:(length(rota)-1)){
        custo = custo + problema$distancias[ rota[i], rota[i+1]];
    }

    #Calcula o OverCapacity
    peso = sum( problema$demanda[rota] );
    over = max( 0, (peso - problema$capacidadeVeiculos)) * problema$gamaOverCapacity;

    return(custo + over);
}

cvrp.evaluate <- function(c){
    #Função que avalia o cromossomo (Divide em segmentos e retorna o fitness)
    if(!cvrp.checkRapido(c))
        return ( Inf );

    c = c( problema$veiculos[1], c); #Adiciona o Veículo 1, pois o primeiro veículo não é representado no cromossomo

    #encontra as posicoes dos veiculos no cromossomo
    veiculos = which( c>problema$dimensoes);
    #Adiciona um separador ponto de segmento final para ganho de performance
    veiculos = c(veiculos, problema$tamanhoCromossomo + 2 );

    cs = list(); #Cria a lista de segmento do cromossomo por veículo (Vazia)
    for(i in 1:problema$qtdeVeiculos){
        cs[[i]] = c[ veiculos[i] : (veiculos[i+1]-1)];
    }

    #Loop que totaliza o fitness por veículo
    ret = 0;
    for(i in 1:problema$qtdeVeiculos){
        ret = ret + cvrp.evaluateTruck( cs[[i]] );
    }

    return (ret);
}

cvrp.corrige <- function(c){
    #Funcão que recebe um cromossomo "errado" e o corrige, COLOCANDO NO FINAL

    #1 - Remove as duplicidades
    ret = unique(c);
    #2 - Remove o que não deveria estar no cromossomo
    ret = ret[ret %in% problema$cromossomoAmostra];
    #3 - Identifica Adiciona algo que esteja faltando
    falta = problema$cromossomoAmostra[ ! problema$cromossomoAmostra %in% ret];
    if( length(falta) > 1 )
        ret = c( ret, sample( falta ) );
    ret = c( ret, falta );

    return ( ret );
}

cvrp.checkRapido <- function(c){
    #Função que avalia rapidamente se o cromossomo é válido ou não

    #1 - Verifica o tamanho
    if( length(c) != problema$tamanhoCromossomo )
        return ( FALSE );
    #2 - Verifica se tem duplicidades
    if( length(unique(c)) != problema$tamanhoCromossomo )
        return ( FALSE );
    #3 - Verifica se tem algum valor nao "Autorizado"
    if( length( c[ !c %in% problema$cromossomoAmostra ] ) > 0 )
        return ( FALSE );

    return ( TRUE );
}

cvrp.crossoverBatch <- function( p1, p2  ){
    #executa um loop de crossover
    linhas = dim(p1)[1];
    ret = NULL;
    for(i in 1:linhas){
        ret = rbind( ret, cvrp.crossover(p1[i,], p2[i,]) );
    }
    return (ret);
}

cvrp.crossover <- function( p1, p2  )
{
    #Funçao de Crossover específico para o problema. Faz um crossover de 2 pontos e garante que não se repete nada
    size = length(p1);

    #Obtem os 2 pontos do CrossOver
    crossOverPoints = sample(1:(size-1),2);
    cop1 = min(crossOverPoints);
    cop2 = max(crossOverPoints);
    #cat("{");cat(cop1);cat(",");cat(cop2);cat("}");

    #O cromossomo será dividido em 3 segmentos: left (esquerda), core (centro), right (direito)

    #O Core é gerado primeiro e cada cromossomo seguinte terá cross se já não estiver no Core.
    child1 = c( rep( NA, cop1 ), p1[(cop1+1) : cop2], rep( NA, size - cop2));
    child2 = c( rep( NA, cop1 ), p2[(cop1+1) : cop2], rep( NA, size - cop2));

    child2[1:cop1] = p1[!(p1 %in% child2)][1:cop1]; #Adiciona o que sobrou em child2, na ordem de p1
    child2[(cop2+1) : size] = p1[!(p1 %in% child2)];

    child1[1:cop1] = p2[!(p2 %in% child1)][1:cop1]; #Adiciona o que sobrou em child1, na ordem de p2
    child1[(cop2+1) : size] = p2[!(p2 %in% child1)];

    ret = matrix( c(child1, child2), nrow = 2, ncol=size, byrow = TRUE );
    return ( ret );
}


cvrp.mutate <- function(
    original,
    mutationRate = 0.10,
    chromosomeRandFunc=NULL  )
{
    #Get the size of the chromosome
    size = length(original);

    #prepare the return
    ret = original;

    #define a order to mutate
    orderToSubstitute = sample(1:size, size);
    for(c in 1:size)
        if(runif(1) <= mutationRate)
        {
            substituteIndex = orderToSubstitute[c];
            substituteValue =  ret[substituteIndex];

            ret[substituteIndex] = ret[c];
            ret[c] = substituteValue;
        }

    return (ret);
}

cvrp.mutate2 <- function(
    original,
    mutationRate = 0.10,
    chromosomeRandFunc=NULL  )
{
    #Get the size of the chromosome
    size = length(original);

    #prepare the return
    ret = original;

    #define a order to mutate
    indices = sample(1:size, 2);
    ret[indices[1]] = original[indices[2]];
    ret[indices[2]] = original[indices[1]];

    return (ret);
}

#a = c(35,36,13,32,9,27,14,10,19,43,12,26,1,2,34,23,8,15,20,22,5,29,3,25,16,6,11,37,28,18,4,42,17,30,7,33,24,31,41,21,39,40)


cvrp.monitor <- function(r){
    #if( (r$generation >= 10) && ((r$generation %% 10) == 0))
    #    EMF.Gen.Plot(r, title = "Solução parcial do CVRP", ylab = "Fitness", includeWorst = TRUE);

    #Para se todos (menos o Elitismo) forem infinitos
#     if(r$lastEvaluations[r$input.params$elitism+1] == Inf){
#         EMF.Gen.Plot(r, title = "Interrompida execução. Excesso de indivíduos inválidos", ylab = "Fitness", xlab = "Geração", invert = FALSE, includeWorst = TRUE);
#         return (TRUE);
#     }


    return (FALSE);
}


