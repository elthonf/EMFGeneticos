EMF.Gen.LogStatistics <- function(execData, execName, fileName,
                                  createLOG = TRUE, createCSV = TRUE, createPNG = TRUE, createPDF = TRUE,
                                  invert = FALSE, ...){
    fileNameLOG = paste("log/", fileName, ".log", sep = ""); #LOG File
    fileNameCSV = paste("log/", fileName, ".csv", sep = ""); #CSV File
    fileNamePNG = paste("log/", fileName, ".png", sep = ""); #PNG File
    fileNamePDF = paste("log/", fileName, ".pdf", sep = ""); #PDF File

    invertValue = 1.0;
    if(invert)
        invertValue = -1.0;

    if(createLOG){
        logFile = file(fileNameLOG, "wb"); #Abre o arquivo de log

        #Header principal
        writeChar("#\r\n#Log da execução de algoritmos genéticos.", con = logFile, eos = "\r\n");
        writeChar(execName, con = logFile, eos = "\r\n");
        writeChar("\r\n", con = logFile);

        #Log dos parâmetros de entrada
        writeChar("\r\n##\r\n##(1)..........Parâmetros de entrada.\r\n##", con = logFile, eos = "\r\n");
        for(i in 1: length(execData$input.params)){
            if(class( unlist(execData$input.params[i])) != "list"){

                li = paste( names(execData$input.params[i])
                            , " (", class( unlist(execData$input.params[i]) ) , ") :"
                            , execData$input.params[i]
                            , sep = "");
                writeChar(li, con = logFile, eos = "\r\n");
            }
        }

        #Sessão legal
        writeChar("\r\n##\r\n##(2)..........Informações da execução.\r\n##", con = logFile, eos = "\r\n");
        writeChar(paste("Início execução......:", execData$input.params$startTime), con = logFile, eos = "\r\n");
        writeChar(paste("Fim da execução......:", execData$time), con = logFile, eos = "\r\n");
        writeChar(paste("Tempo decorrido(secs):", difftime(execData$time, execData$input.params$startTime, units = "secs")), con = logFile, eos = "\r\n");
        writeChar(paste("Gerações executadas..:", execData$generation), con = logFile, eos = "\r\n");
        writeChar(paste("Melhor valor obtido..:", min(execData$best, na.rm = TRUE) * invertValue), con = logFile, eos = "\r\n");
        writeChar(paste("Melhor 1ª ocorrência.:", which (  execData$best == min(execData$best, na.rm = TRUE) )[1]), con = logFile, eos = "\r\n");

        #Sessao extra
        writeChar("\r\n##\r\n##(3)..........Arquivos auxiliares.\r\n##", con = logFile, eos = "\r\n");
        if(createCSV)
            writeChar(paste("Arquivo CSV com estatísticas: ", fileName, ".csv", sep=""), con = logFile, eos = "\r\n");
        if(createPNG)
            writeChar(paste("Arquivo PNG com o gráfico: ", fileName, ".png", sep=""), con = logFile, eos = "\r\n");
        if(createPDF)
            writeChar(paste("Arquivo PDF com o gráfico vetorizado: ", fileName, ".pdf", sep=""), con = logFile, eos = "\r\n");

        close(logFile)
    }

    if(createCSV){
        df = cbind( execData$mean * execData$popSize * invertValue,
                    execData$mean * invertValue,
                    execData$stdDev,
                    execData$best * invertValue,
                    execData$worst * invertValue)
        colnames(df) <- c("Total", "Media", "DesvPad", "Minimo", "Maximo")

        write.csv(df, file = fileNameCSV)
    }

    if(createPNG || createPDF){
        grafico = EMF.Gen.Plot(execData, invert = invert, ... = ... );

        if(createPNG){
            png(fileNamePNG);
            plot(grafico)
            dev.off()
        }

        if(createPDF){
            pdf(fileNamePDF);
            plot(grafico)
            dev.off()
        }
    }

}
