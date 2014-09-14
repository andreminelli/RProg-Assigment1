library(stringr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    total <- length(id)
    files <- character(total)
    values <- NULL
    for(i in 1:total) { 
        files[i] <- paste(directory, paste(str_pad(id[i], 3, pad="0"), ".csv", sep=""), sep="\\")
        data <- read.csv(files[i])
        pollutant_values <- data[pollutant]
        ok_values <- pollutant_values[!is.na(pollutant_values)]
        values <- c(values, ok_values)
    }
    mean(values)
}
