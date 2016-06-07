cat("Função de alta dimensionalidade e unimodal utlizando cromossomo decimal");
library(EMFGeneticos); #library(ggplot2)  # para o caso de não utilizar o pacote, pelo menos será necessário chamar o ggplot2
rm(list=ls()); cat("\014"); #Limpa o console enviando um ctrl + l

#Lista para armazenar as variáveis globais do problema.
f = list();
f$d = 30;      #Determina quantas dimensões a função possui
f$min = -100;  #Determina o valor mínimo possível da dimensão (Usado em criação, mutação, etc.)
f$max = +100;  #Determina o valor máximo possível da dimensão (Usado em criação, mutação, etc.)
f$digits = 10; #Determina o número de casas para a precisão da dimensão (Utilizado na geração e em mutações)
f$mutFatores = seq ( 1, 10, length = 1000 ); #Determina os possíveis fatores de mutação, que vão de 1.0 a 10.0
f$mutFatores = c( f$mutFatores, 1 / f$mutFatores);    #Adiciona o inverso dos mesmos fatores, para diminuir o valor
f$mutFatores = c( f$mutFatores, -f$mutFatores);       #Adiciona os negativos


#Função para gerar um chromossomo aleatório com D dimensões entre
f1.generate <- function(){
    r = runif(f$d, min=f$min, max=f$max);
    r = round(r, f$digits);
    return( r );
}

#Função para avaliar um único cromossomo em F1
f1.fitness <- function(c){
    return( sum(c^2) );
}

#Cross Over 1: cross de 1 ponto
f1.crossover1 <- function( p1, p2 ){
    return (EMF.Gen.CrossOver.Simple(p1=p1, p2=p2, twoPoints = FALSE));
}

#Cross Over 2: cross uniforme
f1.crossover2 <- function( p1, p2 ){
    return (EMF.Gen.CrossOver.Uniforme(p1=p1, p2=p2));
}

#Mutação 1: Apenas 1 gene tem valor alterado aleatoriamente
f1.mutate1 <- function(original, ... ){
    ret = original;

    #Define qual gene será alterado
    i = sample(1:length(original), size = 1);

    ret[i] = runif(1, min=f$min, max=f$min);
    ret[i] = round(ret[i], f$digits);

    return(ret);
}

#Mutação 2: Apenas 1 gene tem valor alterado por um fator que vai de  10% a 1000% do seu valor para cima ou para baixo, além de poder trocar o sinal
f1.mutate2 <- function(original, ... ){
    ret = original;

    #Define quais genes serão afetados
    prob = runif(length(original));
    i = which(prob <= 0.10); #10% de chance do gen ser alterado

    #i = sample(1:length(original), size = 1);

    #Altera multiplicando por 1 dos fatores
    fator = sample(f$mutFatores, size = 1);
    ret[i] = ret[i] * fator;

    #O valor é aredondado em n casas decimais e deve respeitar o teto de 100 e o piso de -100
    ret[i] = round(ret[i], f$digits);
    ret[i] = pmax(f$min, pmin(f$max, round(ret[i]))); #pmax é para array
    #ret[i] = max(f$min, min(f$max, ret[i]));

    return(ret);
}

#Função para monitorar a execução e imprimir resultado parcial
f1.monitor <- function(r){
    if( (r$generation >= 10) && ((r$generation %% 10) == 0))
        EMF.Gen.Plot(r, title = "Solução parcial da Função", ylab = "Fitness", invert = FALSE, includeWorst = TRUE);

    # Parada em fitness <= 0!
    if((r$best[r$generation] <= 0)){
        EMF.Gen.Plot(r, title = "Encontrado ponto ótimo global para a funcão F1", ylab = "Fitness", xlab = "Geração", invert = FALSE, includeWorst = TRUE);
        return (TRUE);
    }

    return (FALSE);
}
