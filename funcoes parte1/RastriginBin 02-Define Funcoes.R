cat("Função de Rastrigin utlizando cromossomo binário");
library(EMFGeneticos); #library(ggplot2)  # para o caso de não utilizar o pacote, pelo menos será necessário chamar o ggplot2
rm(list=ls()); cat("\014"); #Limpa o console enviando um ctrl + l


#Lista para armazenar as variáveis globais do problema. rb = Rastrigin Binário
rb = list();
#Determina o número de bits do NÚMERO. O cromossomo terá tamanho igual ao DOBRO deste valor
rb$bitsSize = 64;
rb$chromoSize = rb$bitsSize * 2;
#Determina o mínimo e máximo valor, bem como o fator de conversão
rb$minValue = -5;
rb$maxValue = +5;
rb$range = rb$maxValue - rb$minValue;
rb$midValue = rb$maxValue - rb$range / 2;
rb$bitWeight = c( 2^(0:(rb$bitsSize-1-1)), 0) #O último sempre será zero
rb$factor = (rb$range / 2) / ( 2 ^ (rb$bitsSize-1) -1);
#Teste com 3 bits:
#t = rbind( c(0,0,0, 1), c(1,0,0, 1), c(0,1,0, 1), c(1,1,0, 1), c(0,0,1, 1), c(1,0,1, 1), c(0,1,1, 1), c(1,1,1, 1) )
#t %*% rb$bitWeight

#Função para converter um cromossomo em 2 doubles (X e Y). A ser utilizado no fitness.
rb.toDouble <- function(c){
    bits = rbind( c[ 1               : rb$bitsSize   ],  #Bits do numero X
                  c[ (rb$bitsSize+1) : rb$chromoSize ]); #Bits do numero Y

    intValue = bits %*% rb$bitWeight; #Transforma em inteiros (ultimo bit multiplica por zero)

    lastbit = bits[,rb$bitsSize]; #Obtem o ultimo bit
    lastbit = lastbit * -2 + 1; #Transforma (0 em 1) e (1 em -1)

    #Se o ultimo bit for 1, o valor é negativo
    doubleValue = intValue * rb$factor * lastbit + rb$midValue;

    return( doubleValue );
}

#Função para gerar um cromossomo aleatório, vetor com n*2 bits
rb.generate <- function(){
    r = sample(0:1, rb$chromoSize, replace = TRUE);
    return( r );
}

#Função para avaliar um cromossomo (Obj: MIN==0).
rb.fitness <- function(c){
    dvalues = rb.toDouble(c);
    x = dvalues[1]; y = dvalues[2];

    zx = x^2 -10*cos(2*pi*x) +10;
    zy = y^2 -10*cos(2*pi*y) +10;
    z = zx + zy;
    #Observação: veja que não há a inversão final do -z
    return(z);
}

#Função de mutação onde cada gene tem 10% de chance de ser alterado!
rb.mutate1 <- function(original, ... ){
    ret = original;

    chances = runif(length(original)); #Vetor com as chances de ser alterado
    changes = which(chances <= 0.1); #Vetor com os índices que serão alterados

    ret[changes] = !ret[changes]; #Agora, inverte apenas aqueles q estão no índice

    return(ret);
}

#Função de mutação onde 2  genes são alterados de posição
rb.mutate2 <- function( original, ... ){
    ret = original;

    #Define quais 2 serão alterados
    i = sample(1:length(original), size = 2);

    ret[i[1]] = original[i[2]];
    ret[i[2]] = original[i[1]];

    return(ret);
}

#Função de mutação onde APENAS 1 gene tem chance ser alterado!
rb.mutate3 <- function(original, ... ){
    ret = original;

    #Define qual gene será alterado
    i = sample(1:length(original), size = 1);

    ret[i] = !ret[i]; #Agora, inverte apenas aqueles q estão no índice

    return(ret);
}

#Cross Over 1: cross de 1 ponto
rb.crossover1 <- function( p1, p2 ){
    return (EMF.Gen.CrossOver.Simple(p1=p1, p2=p2, twoPoints = FALSE));
}

#Cross Over 2: cross uniforme
rb.crossover2 <- function( p1, p2 ){
    return (EMF.Gen.CrossOver.Uniforme(p1=p1, p2=p2));
}

#Função para monitorar a execução e imprimir resultado parcial.
rb.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0))
        EMF.Gen.Plot(r, title = "Solução parcial para Rastrigin Binário", ylab = "Fitness", xlab = "Geração", invert = TRUE, includeWorst = TRUE);
    # Parada em fitness <= 0!
    if((r$best[r$generation] <= 0)){
        EMF.Gen.Plot(r, title = "Encontrado ponto ótimo global para Rastrigin Binário", ylab = "Fitness", xlab = "Geração", invert = TRUE, includeWorst = TRUE);
        return (TRUE);
    }
    # Parada em 20 gerações sem evolução mais desvio padrão <= 5.0
    if(r$generation >= 50 && FALSE) #FALSE = REGRA DESATIVADA
        if( ( r$best[r$generation] == r$best[r$generation-30]) &&
            r$stdDev[r$generation] < 5.0 &&
            r$stdDev[r$generation-30] < 5.0 ){
            EMF.Gen.Plot(r, title = "Parado por estar há 20 gerações sem evolução.", ylab = "Fitness", xlab = "Geração", invert = TRUE, includeWorst = TRUE);
        return (TRUE);
    }

    return (FALSE);
}


