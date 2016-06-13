library(EMFGeneticos) #library(ggplot2)  # detach(package:ggplot2)
rm(list=ls()) #limpa

############# #############  ############# 0 : CARREGA O PROBLEMA!
#problema <- EMF.Gen.Cvrp.LoadProblem(filename = "tests/cvrp/A-n37-k6.vrp")

############# #############  ############# 1 : DEFINE FUNCOES DE CRIACAO E FITNESS!
cvrp.generate <- function(){
    #Função para gerar um cromossomo aleatório a partir do domínio
    clientes = sample( problema$clientes );

    cs = list(); #Cria a lista de segmento do cromossomo por veículo (Vazia)
    for(i in 1:problema$qtdeVeiculos){
        cs[[i]] = problema$veiculos[i];
    }

    veiculo = 1;
    carga = 0;

    for(i in 1:(problema$dimensoes-1)){ #Distribui os clientes em veículos
        carga = carga + problema$demanda[clientes[i]];
        if(carga >= problema$capacidadeVeiculos && veiculo < problema$qtdeVeiculos){
            veiculo = veiculo + 1;
            carga = problema$demanda[clientes[i]];
        }
        cs[[veiculo]] = c( cs[[veiculo]] , clientes[i]);
    }
    #r = sample( problema$cromossomoAmostra );
    r = cvrp.getCromossomoFromRotas(cs);
    return( r );
}

cvrp.generate.bestInsertion <- function(){
    #Função para gerar um cromossomo aleatório a partir do domínio
    clientes = sample( problema$clientes );
    veiculos = sample( problema$veiculos, size = problema$dimensoes-1, replace = TRUE)

    cs = list(); #Cria a lista de segmento do cromossomo por veículo (Vazia)
    for(i in 1:problema$qtdeVeiculos){
        cs[[i]] = problema$veiculos[i];
        clientesRota = clientes[ veiculos == problema$veiculos[i] ];
        if(length(clientesRota) > 0)
            for( j in 1:length(clientesRota)){
                cs[[i]] = cvrp.bestInsertion(cs[[i]], clientesRota[j]);
            }
    }

    #r = sample( problema$cromossomoAmostra );
    r = cvrp.getCromossomoFromRotas(cs);
    return( r );
}


cvrp.getRotas <- function(c){
    #Funcao que desmembra o cromossomo e o converte em uma lista de rotas
    c = c( problema$veiculos[1], c); #Adiciona o Veículo 1, pois o primeiro veículo não é representado no cromossomo

    #encontra as posicoes dos veiculos no cromossomo
    veiculos = which( c>problema$dimensoes);
    #Adiciona um separador ponto de segmento final para ganho de performance
    veiculos = c(veiculos, problema$tamanhoCromossomo + 2 );

    cs = list(); #Cria a lista de segmento do cromossomo por veículo (Vazia)
    for(i in 1:problema$qtdeVeiculos){
        cs[[i]] = c[ veiculos[i] : (veiculos[i+1]-1)];
    }

    return (cs);
}

cvrp.getCromossomoFromRotas <- function(cs){
    #Funcão que gera um cromossomo a partir de uma lista de rotas (inverso do cvrp.getRotas)
    rotas = length(cs);
    ret = NULL;
    if(length(cs[[1]]) > 1) #Trata a primeira rota
        ret = cs[[1]][2:length(cs[[1]])]

    if(rotas > 1){
        for(i in 2:rotas){
            ret = c(ret, cs[[i]])
        }
    }
    return (ret);
}

cvrp.evaluateTruck <- function(cp){
    #Função para avaliar a rota de 1 único caminhão!
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

    #print( paste("Caminhao:", cp[1], "custo:", custo, "peso:", peso) );

    return(custo + over);
}

cvrp.evaluate <- function(c){
    #Função que avalia TODO o cromossomo (Divide em segmentos e retorna o fitness)
    if(!cvrp.checkRapido(c))
        return ( Inf );

    cs = cvrp.getRotas(c); #Cria a lista de segmento do cromossomo por veículo (Vazia)

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
    if( length(falta) > 1 ){
        ret = c( ret, sample( falta ) );
    }else{
        ret = c( ret, falta );
    }

    return ( ret );
}

cvrp.corrigeTruck <- function(cp){
    #Funcão que recebe uma rota de um veículo e determina o MENOR trajeto UNICAMENTE DA ROTA
    #A função sempre avalia a menor distância entre os endereços, a partir do depósito

    if(length(cp) <= 3 ) #Até 2 entregas (caminhao+2) não faz diferença a ordem
        return (cp);

    enderecosOrdenar = cp[2:length(cp)];
    ret = cp[1]; #Sempre inicia com o caminhao

    #Insere cliente a cliente na melhor opção
    for( j in 1:length(enderecosOrdenar)){
        ret = cvrp.bestInsertion(ret, enderecosOrdenar[j]);
    }

    return ( ret );
}

cvrp.corrigeNaOrdem <- function(c){
    #Funcão que recebe um cromossomo "errado" e o corrige, COLOCANDO NA ORDEM. NAO CONCLUIDO

    #1 - Identifica o que está faltando
    falta = problema$cromossomoAmostra[ ! problema$cromossomoAmostra %in% c];

    nFalta = length(falta);
    #Se nao falta nada, sai
    if(nFalta == 0 ) return (c);

    #Varre o vetor identificando necessidades de troca, NAO TERMINEI!
    ret = c;
    for(i in 1:length(c)){

    }


    #1 - Remove as duplicidades
    ret = unique(c);
    #2 - Remove o que não deveria estar no cromossomo
    ret = ret[ret %in% problema$cromossomoAmostra];
    #3 - Identifica Adiciona algo que esteja faltando
    falta = problema$cromossomoAmostra[ ! problema$cromossomoAmostra %in% ret];
    if( length(falta) > 1 ){
        ret = c( ret, sample( falta ) );
    }else{
        ret = c( ret, falta );
    }

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

cvrp.crossover.pmx.batch <- function( p1, p2  ){
    #executa um loop de crossover de caixeiro viajante
    linhas = dim(p1)[1];
    ret = NULL;
    for(i in 1:linhas){
        ret = rbind( ret, cvrp.crossover.pmx(p1[i,], p2[i,]) );
    }
    return (ret);
}

cvrp.crossover.pmx <- function( p1, p2  )
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

cvrp.crossover.repara <- function( p1, p2  ){
    ret = EMF.Gen.CrossOver.Simple( p1, p2 ); #Primeiro, faz o crossover simples de 2 pontos

    nRows = nrow(ret);
    for(i in 1:nRows){
        ret[i,] = cvrp.corrige( ret[i,] ) ; #E então, repara os descendentes
    }

    return (ret);
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

cvrp.mutate.permut <- function(
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

cvrp.mutate.change <- function(
    original,
    mutationRate = 0.10,
    chromosomeRandFunc=NULL  )
{
    #Get the size of the chromosome
    size = length(original);
    ret = original;

    #Escolhe 2 posições: 1 gene do cromossomo e determina 1 local onde ele será inserido
    indices = sample(1:size, 2);
    escolhido = indices[1];
    novaposicao = indices[2];

    #prepare the return, sem o cromossomo removido
    temp = original[-escolhido];

    #Insere no final
    if(novaposicao == size){
        ret = c(temp, original[escolhido]);
    }else if(novaposicao == 1){#insere no início
        ret = c(original[escolhido], temp);
    }else{#Insere no meio
        ret = c( temp[1:(novaposicao-1)], original[escolhido] , temp[novaposicao:(size-1)]);
    }

    return (ret);
}

cvrp.mutate.composto <- function(
    original,
    mutationRate = 0.10,
    chromosomeRandFunc=NULL  )
{
    #Verifica se deve mutar apenas dentro do cromossomo.
    ret = NULL;
    chance = runif(1, 0, 1);

    if(chance<= 0.40){ #40% para alterar dentro da rota
        cs = cvrp.getRotas(c = original);
        idRotaAlterar = sample(problema$qtdeVeiculos, 1);
        rotaAlterar = cs[[idRotaAlterar]];
        if(length(rotaAlterar) >2){
            trocas = sample(2:length(rotaAlterar), 2);

            cs[[idRotaAlterar]][trocas[1]] = rotaAlterar[trocas[2]];
            cs[[idRotaAlterar]][trocas[2]] = rotaAlterar[trocas[1]];
        }
        ret = cvrp.getCromossomoFromRotas(cs);
        #print(cs);
    }else{
        ret = cvrp.mutate.permut(original);
    }

    return (ret);
}


cvrp.mutate.srm <- function(
    original,
    mutationRate = 0.10,
    chromosomeRandFunc=NULL  )
{
    #baseado no artigo de 2004, escolhe um determinado cliente e tenta melhorá-lo na própria rota ou em outras rotas
    ret = NULL;
    chance = runif(1, 0, 1);

    #Obtem as rotas e escolhe uma a alterar (rota deve possuir mais que 1 entrega)
    cs = cvrp.getRotas(c = original);
    idRotaRemover = sample(problema$qtdeVeiculos, 1);
    rotaRemover = cs[[idRotaRemover]];
    idRotaInserir = 0;
    rotaInserir = NULL;
    while(length(rotaRemover) <=2){
        idRotaRemover = sample(problema$qtdeVeiculos, 1);
        rotaRemover = cs[[idRotaRemover]];
    }

    #Remove algum indivíduo da rota
    idItemRemover = sample(2:length(rotaRemover), size = 1);
    itemRemovido = rotaRemover[idItemRemover];
    rotaRemover = rotaRemover[-idItemRemover];

    #Tem 30% de chance de inserir na própria rota, senão escolhe outra
    if(chance <= 0.30){
        idRotaInserir = idRotaRemover;
        rotaInserir = rotaRemover;
    }else{
        idRotaInserir = sample((1:problema$qtdeVeiculos)[-idRotaRemover], 1, 1);
        rotaInserir = cs[[idRotaInserir]];
    }

    rotaInserir = cvrp.bestInsertion(rotaInserir, itemRemovido); #Insere na "Best Insertion"

    #Atualiza o array de rotas
    cs[[idRotaRemover]] = rotaRemover;
    cs[[idRotaInserir]] = rotaInserir;

    return (cvrp.getCromossomoFromRotas(cs));
}

cvrp.bestInsertion <- function(rotaInserir, itemInserir){
    #Insere no melhor lugar da rota
    if(length(rotaInserir) <= 2) #Rotas de 0 ou 1 entrega, insere no final
        return ( c(rotaInserir, itemInserir) );

    melhorRota = NULL;
    melhorFitness = Inf;
    for(i in 2:(length(rotaInserir) +1)){
        esqIndex = 1:(i-1);
        dirIndex = (i):length(rotaInserir);
        if(i == length(rotaInserir) +1) dirIndex = NULL;

        rotaCandidata = c( rotaInserir[esqIndex], itemInserir,  rotaInserir[dirIndex]);
        fitnessCandidato = cvrp.evaluateTruck(rotaCandidata);
        if(fitnessCandidato < melhorFitness){
            melhorFitness = fitnessCandidato;
            melhorRota = rotaCandidata;
        }
    }

    return (melhorRota);
}

#a = c(35,36,13,32,9,27,14,10,19,43,12,26,1,2,34,23,8,15,20,22,5,29,3,25,16,6,11,37,28,18,4,42,17,30,7,33,24,31,41,21,39,40)


cvrp.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0) && (r$worst[r$generation] != Inf))
    {
        EMF.Gen.Plot(r, title = "Solução parcial do CVRP", ylab = "Fitness", includeWorst = TRUE);
    }

    #Para se todos avaliados (menos o Elitismo) forem infinitos
#      if(r$lastEvaluations[r$input.params$elitism+1] == Inf){
#          print("Interrompida execução. Excesso de indivíduos inválidos");
#          return (TRUE);
#      }

    #Para se todos avaliados (INCLUINDO o Elitismo) forem infinitos
    if(r$lastEvaluations[1] == Inf){
        print("Interrompida execução. Excesso de indivíduos inválidos");
        return (TRUE);
    }

    return (FALSE);
}
