EMF.Gen.CrossOver.Uniforme <- function( p1, p2, ...  )
{
    sizeP1 = length(p1);
    sizeP2 = length(p2);

    if(sizeP1 != sizeP2)
        stop("Parents must have same size.");
    if(sizeP1 <= 1){
        ret = matrix( c(p1, p2), nrow = 2, ncol=sizeP1, byrow = TRUE );
        return ( ret );
    }

    #1st, copy parents into childs
    child1 = p1;
    child2 = p2;

    #2nd, define changing genes. 50% of chance
    for(i in 1:sizeP1){
        if(runif(1) <= 0.5){
            child1[i] = p2[i];
            child2[i] = p1[i];
        }
    }

    #ret = matrix( c(child1, child2), nrow = 2, ncol=sizeP1, byrow = TRUE );
    return ( rbind(child1, child2) );

}
