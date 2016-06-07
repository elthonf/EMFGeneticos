EMF.Gen.CrossOver.Simple <- function( p1, p2, twoPoints = TRUE, twoPointsOnSmall = FALSE  )
{
    #Se não for matriz, transforma em matriz
    if(!is.matrix(p1)) p1 = matrix(p1, ncol = length(p1));
    if(!is.matrix(p2)) p2 = matrix(p2, ncol = length(p2));

    sizeP1 = length(p1);
    sizeP2 = length(p2);

    if(sizeP1 != sizeP2)
        stop("Parents must have same size.");
    if(sizeP1 <= 1){
        ret = matrix( c(p1, p2), nrow = 2, ncol=sizeP1, byrow = TRUE );
        return ( ret );
    }

    pairs = dim(p1)[1]; #How many pairs
    chromoSize = dim(p1)[2]; #Each chromosome size

    #Prepare the children
    child1 = matrix( nrow = pairs, ncol = chromoSize);
    child2 = matrix( nrow = pairs, ncol = chromoSize);

    if( ( twoPoints && (chromoSize > 10) ) ||
        ( twoPointsOnSmall && (chromoSize > 2)) ) #TWO POINTS CROSSOVER
    {
        for(i in 1:pairs){
            crossOverPoints = sample(1:(chromoSize-1), size = 2); #2 distinct points

            cop1 = min(crossOverPoints);
            cop2 = max(crossOverPoints);

            #cat(cop1);cat(cop2);

            child1[i,] = c( p1[i, 1 : cop1 ], p2[i, (cop1+1) : cop2], p1[i, (cop2+1) : chromoSize] );
            child2[i,] = c( p2[i, 1 : cop1 ], p1[i, (cop1+1) : cop2], p2[i, (cop2+1) : chromoSize] );
        }
    }
    else #ONE POINT CROSSOVER
    {
        crossOverPoints = sample(1:(chromoSize-1), pairs, replace = TRUE);

        for(i in 1:pairs){
            child1[i,] = c( p1[i, 1:crossOverPoints[i]], p2[i, (crossOverPoints[i]+1):chromoSize] );
            child2[i,] = c( p2[i, 1:crossOverPoints[i]], p1[i, (crossOverPoints[i]+1):chromoSize] );
        }

    }

    return ( rbind(child1, child2) );
}

#Para testar
# p1 = rep(1, 10); p2 = rep(3, 10);
# EMF.Gen.CrossOver.Uniforme(p1, p2)
# EMF.Gen.CrossOver.Simple(p1, p2)
#
# p1 = rbind( rep(1, 10), rep(2, 10) ); p2 = rbind( rep(3, 10), rep(4, 10) )
# EMF.Gen.CrossOver.Uniforme(p1, p2)
# EMF.Gen.CrossOver.Simple(p1, p2)
