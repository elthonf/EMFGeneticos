EMF.Gen.Workflow <- function(
    popSize=1000, iters=100,
    crossOver=5000, mutationChance=NA, #Mutation chance on CrossOver. runif(1)

    suggestions=NULL, #A possible suggestion for the start population
    clone=round(popSize * 0.02), #integer or 2%
    cloneAndMutate=ceiling(popSize * 0.005), #integer or 0.5% (ceiling<>floor)
    elitism=floor(popSize * 0.01), #integer or 1%
    foreigners = 0,

    chromosomeRandFunc=NULL, evalFunc=NULL, monitorFunc=NULL,
    crossOverFunc = EMF.Gen.CrossOver.Simple,
    mutationFunc = EMF.Gen.Mutate.Simple,

    parentProb = dnorm(1:popSize, mean=1, sd=(popSize/3)), #Probabilidade dos pais serem eleitos
    #Alternativas:
    # dnorm(1:popSize, mean=1, sd=(popSize/3)) -> Divide 30%, 60% e 100% em 63,0%, 29,9% e 07,1%
    # dnorm(1:popSize, mean=1, sd=(popSize/2)) -> Divide 30%, 60% e 100% em 47,1%, 33,4% e 19,5%
    # dnorm(1:popSize, mean=1, sd=(popSize/1)) -> Divide 30%, 60% e 100% em 34,5%, 31,6% e 33,9%
    verbose=FALSE
)
{
    if (is.null(evalFunc))
        stop("A evaluation function must be provided. See the evalFunc parameter.");
    if (is.null(chromosomeRandFunc))
        stop("A chromosome generation function must be provided. See the chromosomeRandFunc parameter.");

    #LOG the INPUT
    input.params = list(popSize=popSize, iters=iters,
                        crossOver=crossOver, mutationChance=mutationChance,
                        suggestions=suggestions, clone=clone, cloneAndMutate=cloneAndMutate,
                        elitism=elitism, chromosomeRandFunc=chromosomeRandFunc, evalFunc=evalFunc, monitorFunc=monitorFunc,
                        crossOverFunc=crossOverFunc, mutationFunc=mutationFunc,
                        parentProb=parentProb, verbose=verbose, startTime=Sys.time());

    #Discover the chromosome size
    cSize = length( chromosomeRandFunc() )[1];
    if (verbose) cat(paste("Chromosome size:", cSize, "\n"));

    ### ### ### ### ### STEP 1: INIT THE FIRST POPULATION ### ### ### ### ### ### ### ### ### ###
    if (verbose) cat("Generating the first population...\n");
    population = matrix(nrow=popSize, ncol=cSize); #Empty: Final Population

    suggestionCount = dim(suggestions)[1];
    if(is.null(suggestions)) {
        suggestionCount = 0;
    }
    else {
        for (i in 1:suggestionCount) {
            population[i,] = suggestions[i,];
        }
    }
    for (child in (suggestionCount+1):popSize) {
        population[child,] = chromosomeRandFunc();
    }

    ### ### ### ### ### STEP 2: DO ITERATIONS ### ### ### ### ### ### ### ### ### ###
    bestEvals = rep(NA, iters);
    stdDevEvals = rep(NA, iters);
    meanEvals = rep(NA, iters);
    worstEvals = rep(NA, iters);
    evalVals = rep(NA, dim(population)[1]);
    for (generation in 1:iters) #Include ONE, the starting generation
    {
        if (verbose) cat(paste("Starting working on generation", generation, "\n"));

        # Calculate fitness for each chromosome
        if (verbose) cat("  calculating evaluation values...");
        for (object in 1:dim(population)[1]) {
            if (is.na(evalVals[object])) {
                evalVals[object] = evalFunc(population[object,]);
                #if (verbose) cat(".");
            }
        }
        if (verbose) cat(" done.\n");

        if (verbose) cat("  sorting results...\n");
        sortedEvaluations = sort(evalVals, index=TRUE);
        sortedPopulation  = matrix(population[sortedEvaluations$ix,], ncol=cSize);
        evalVals = sortedEvaluations$x; #Linhas opcionais, deixa salvo ordenado então vou manter
        population = sortedPopulation; #Linhas opcionais, deixa salvo ordenado então vou manter

        if(dim(population)[1] > popSize){
            if (verbose) cat(paste("  killing the", dim(population)[1] - popSize, "excedent. Only the", popSize,"strongest will survive.\n"));
            population = population[1:popSize,];
            evalVals = evalVals[1:popSize];
        }

        if (verbose) cat("  logging statistical results...\n");
        bestEvals[generation] = min(evalVals);
        meanEvals[generation] = mean(evalVals);
        stdDevEvals[generation] = sd(evalVals);
        worstEvals[generation] = max(evalVals);
        if (verbose) cat(paste("  best eval: ", bestEvals[generation] ,".\n"));

        #If there's a monitor function, send status to it
        stopFromMonitor = FALSE;
        if (!is.null(monitorFunc)) {
            if (verbose) cat("Sending current state to monitorFunc()...\n");
            # report on GA settings
            result = list(type="custom chromosome",
                          input.params=input.params,
                          cSize=cSize, popSize=popSize,
                          generation=generation,
                          lastPopulation=population,
                          lastEvaluations=evalVals,
                          best=bestEvals, worst=worstEvals, mean=meanEvals, stdDev=stdDevEvals,
                          time=Sys.time());
            class(result) = "EMFGeneticos";

            stopFromMonitor = monitorFunc(result);
        }

        if ((generation >= iters) && verbose) cat("  stoped: last generation.\n");
        if (stopFromMonitor       && verbose) cat("  stoped: message from monitor.\n");
        if ((generation >= iters) || stopFromMonitor) break; # OK, computed the last generation. Then, LEAVE!



        ### ### ### ### ### STEP 3: CREATE NEXT GENERATION ### ### ### ### ### ### ### ### ### ###

        if (verbose) cat("  creating next generation...\n");
        newPopulation = matrix(nrow=0, ncol=cSize);
        newEvalVals = c();

        # 1st - Elitism: save the best!
        if (elitism > 0) {
            if (verbose) cat(paste("  applying elitism: ", elitism, "item(s) \n"));
            newPopulation = rbind( newPopulation, sortedPopulation[1:elitism,]);
            newEvalVals = c( newEvalVals, sortedEvaluations$x[1:elitism] );
        } # ok, save nothing

        # 2nd - CrossOver: Copulate
        if (crossOver > 1) {
            if (verbose) cat(paste("  applying crossOver: ", crossOver, "item(s) ... "));
            mutants = 0;

            parentIDs = matrix( nrow = crossOver/2, ncol = 2);
            if(crossOver >= 5000) #More eficient way to large crossOvers (could repeat)
            {
                parentIDs[,1] = sample(1:popSize, crossOver/2, prob= parentProb, replace = TRUE);
                parentIDs[,2] = sample(1:popSize, crossOver/2, prob= parentProb, replace = TRUE);
            }
            else{
                for( i in 1:(crossOver/2)){ #Generate in pairs, distinct parents
                    parentIDs[i,] = sample(1:popSize, 2, prob= parentProb);
                }
            }
            parents1 = sortedPopulation[parentIDs[,1],];
            parents2 = sortedPopulation[parentIDs[,2],];

            # crossOver them
            children = crossOverFunc( parents1, parents2 );

            if(!is.na( mutationChance ))
            {
                for(child in 1:dim(children)[1] ) {
                    if(runif(1) <= mutationChance){
                        children[child,] = mutationFunc(original=children[child,], chromosomeRandFunc=chromosomeRandFunc);
                        mutants = mutants + 1;
                    }
                }
            }

            newPopulation = rbind( newPopulation, children);
            newEvalVals = c( newEvalVals, rep(NA, dim(children)[1] ) ); #Compute eval later
            if (verbose) cat(paste(" done with ", mutants, " mutants. \n"));
        }

        # 3rd - Clone: Simple clone (Exclude Elite)
        if (clone > 0) {
            if (verbose) cat(paste("  applying perfect clone: ", clone, "item(s) \n"));

            parentIDs = sample((elitism+1):popSize, clone, prob= parentProb[(elitism+1):popSize]);
            parents = sortedPopulation[parentIDs,];

            newPopulation = rbind( newPopulation, parents);
            newEvalVals = c( newEvalVals, sortedEvaluations$x[parentIDs] );
        }

        # 4th - Clone: Clone and Mutate
        if (cloneAndMutate > 0) {
            if (verbose) cat(paste("  applying clone and mutate: ", cloneAndMutate, "item(s) \n"));

            parentIDs = sample(1:popSize, cloneAndMutate, prob= parentProb, replace = TRUE);
            parents = sortedPopulation[parentIDs,];
            for(p in 1:cloneAndMutate)
                parents[p,] = mutationFunc( original=parents[p,], chromosomeRandFunc=chromosomeRandFunc);

            newPopulation = rbind( newPopulation, parents);
            newEvalVals = c( newEvalVals, rep(NA, cloneAndMutate ) ); #Compute eval later
        }

        # 5th - Foreigners: Clone and Mutate
        if (foreigners > 0) {
            if (verbose) cat(paste("  applying foreigners: ", foreigners, "item(s) \n"));

            for (i in 1:foreigners) {
                child = chromosomeRandFunc();

                newPopulation = rbind( newPopulation, child);
                newEvalVals = c( newEvalVals, NA ); #Compute eval later
            }
        }

        # Final: if the next pop isnt enough, force to clone more (Exclude elite)
        if( dim(newPopulation)[1] < popSize ) {
            needed = popSize - dim(newPopulation)[1];
            if (verbose) cat(paste("  forcing clone on : ", needed, "item(s) \n"));

            parentIDs = sample((elitism+1):popSize, needed, prob= parentProb[(elitism+1):popSize]);
            parents = sortedPopulation[parentIDs,];

            newPopulation = rbind( newPopulation, parents);
            newEvalVals = c( newEvalVals, sortedEvaluations$x[parentIDs] );
        }

        population = newPopulation;
        evalVals   = newEvalVals;
    }

    # report on GA settings
    result = list(type="custom chromosome",
                  input.params=input.params,
                  cSize=cSize, popSize=popSize,
                  generation=generation,
                  lastPopulation=population,
                  lastEvaluations=evalVals,
                  best=bestEvals, worst=worstEvals, mean=meanEvals, stdDev=stdDevEvals,
                  time=Sys.time() );
    class(result) = "EMFGeneticos";

    return(result);
}
