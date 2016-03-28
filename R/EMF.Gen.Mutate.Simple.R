EMF.Gen.Mutate.Simple <- function(
    original,
    mutationRate = 0.10,
    chromosomeRandFunc=NULL  )
{
    #Get the size of the chromosome
    size = length(original);

    #prepare the return
    ret = original;

    if(!is.null(chromosomeRandFunc))
    {
        newSample = chromosomeRandFunc();
        for(c in 1:size)
            if(runif(1) <= mutationRate)
                ret[c] = newSample[c];

    }
    else #Mutate the order of the chromosomes
    {
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
    }

    return (ret);
}
