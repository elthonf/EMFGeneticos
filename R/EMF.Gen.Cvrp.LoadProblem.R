EMF.Gen.Cvrp.LoadProblem <- function(
    filename = "tests/cvrp/A-n37-k6.vrp",
    gamaOverCapacity = 10000
)
{
    #Função que lê um arquivo do site http://www.bernabe.dorronsoro.es/vrp/
    ret <- list();
    ret$nome = filename;
    ret$gamaOverCapacity = gamaOverCapacity;

    #Funcao auxiliar para calcular distancias
    ret$distanciaEntre <- function( x1, x2, y1, y2)
    {
        return ( sqrt ( ( x1 - x2) ^ 2 + ( y1 - y2) ^ 2 ) );
    }

    #Lê o conteúdo do arquivo
    ret$conteudo <- readLines(filename);

    ##### Lê o arquivo
    tipo = "HEADER"; #Inicia no Header
    for(l in 1: length(ret$conteudo)){
        linha = ret$conteudo[l];
        if(tipo == "HEADER"){
            if(grepl("CAPACITY", linha))
                ret$capacidadeVeiculos = as.numeric(unlist(strsplit(linha, split = ":"))[2]);
            if(grepl("NAME", linha)){
                ret$nome = unlist(strsplit(linha, split = ":"))[2];
                #Determina as dimensões e a quantidade de veículos
                ret$dimensoes = as.numeric( gsub("n", "", unlist(strsplit(ret$nome, split = "-"))[2]) );
                ret$qtdeVeiculos = as.numeric( gsub("k", "", unlist(strsplit(ret$nome, split = "-"))[3]) );
            }
            if(grepl("NODE_COORD_SECTION", linha)) tipo = "NODE_COORD_SECTION";
        }else if (tipo == "NODE_COORD_SECTION"){
            if(grepl("DEMAND_SECTION", linha)){
                tipo = "DEMAND_SECTION";
            }else{
                valores = unlist( strsplit(linha, split = " ") );
                valores = valores[(length(valores)-2):length(valores)];
                ret$nodes = c(ret$nodes, as.numeric(valores[1]));
                ret$x = c( ret$x, as.numeric(valores[2]));
                ret$y = c( ret$y, as.numeric(valores[3]));
            }
        }else if (tipo == "DEMAND_SECTION"){
            if(grepl("DEPOT_SECTION", linha)){
                tipo = "DEPOT_SECTION";
            }else{
                valores = unlist( strsplit(linha, split = " ") )[1:2];
                ret$demanda = c( ret$demanda, as.numeric(valores[2]));
            }
        }
    }

    #Determina o depósito, os clientes e os veículos
    ret$deposito = ret$nodes[1];
    ret$clientes = ret$nodes[2:ret$dimensoes];
    ret$veiculos = (ret$dimensoes + 1) : (ret$dimensoes + ret$qtdeVeiculos);

    #Cria uma amostra. Obs.: O primeiro veículo não é representado no cromossomo
    ret$cromossomoAmostra = c( ret$nodes , ret$veiculos[2:ret$qtdeVeiculos] );
    ret$tamanhoCromossomo = length(ret$cromossomoAmostra);

    #Cria a matriz com as distâncias (já deixa calculado para performance)
    ret$distancias = matrix(  nrow = ret$dimensoes, ncol = ret$dimensoes ); #, dimnames = list(ret$nodes, ret$nodes))
    for(i in 1:ret$dimensoes)
        for(j in 1:ret$dimensoes)
            ret$distancias[i, j] = ret$distanciaEntre(ret$x[i], ret$x[j], ret$y[i], ret$y[j]);


    return (ret);
}
