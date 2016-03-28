EMF.Gen.CrossOver.Simple <- function( p1, p2, twoPoints = TRUE, twoPointsOnSmall = FALSE  )
{
    sizeP1 = length(p1);
    sizeP2 = length(p2);

    if(sizeP1 != sizeP2)
        stop("Parents must have same size.");
    if(sizeP1 <= 1){
        ret = matrix( c(p1, p2), nrow = 2, ncol=sizeP1, byrow = TRUE );
        return ( ret );
    }

    if( ( twoPoints && (sizeP1 > 10) ) ||
        ( twoPointsOnSmall && (sizeP1 > 2)) ) #TWO POINTS
    {
        crossOverPoint1 = sample(1:(sizeP1-1),1);
        crossOverPoint2 = crossOverPoint1;
        while(crossOverPoint2 == crossOverPoint1)
        {
            crossOverPoint2 = sample(1:(sizeP1-1),1);
        }
        cop1 = min(crossOverPoint1, crossOverPoint2);
        cop2 = max(crossOverPoint1, crossOverPoint2);

        #cat(cop1);cat(cop2);

        child1 = c( p1[ 1 : cop1 ], p2[(cop1+1) : cop2], p1[(cop2+1) : sizeP1] );
        child2 = c( p2[ 1 : cop1 ], p1[(cop1+1) : cop2], p2[(cop2+1) : sizeP1] );
    }
    else
    {
        crossOverPoint = sample(1:(sizeP1-1),1);

        #p1[ 1 : crossOverPoint ]
        #p2[ (crossOverPoint+1) : sizeP1 ]

        child1 = c( p1[1:crossOverPoint], p2[(crossOverPoint+1):sizeP1] );
        child2 = c( p2[1:crossOverPoint], p1[(crossOverPoint+1):sizeP1] );
    }

    ret = matrix( c(child1, child2), nrow = 2, ncol=sizeP1, byrow = TRUE );
    return ( ret );

}
