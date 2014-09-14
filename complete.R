complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    total <- length(id)
    files <- character(total)
    result <- data.frame(id = numeric(total), nobs = numeric(total))
    for(i in 1:total) { 
        files[i] <- paste(directory, paste(str_pad(id[i], 3, pad="0"), ".csv", sep=""), sep="\\")
        data <- read.csv(files[i])
        nobs <- sum(complete.cases(data))
        result$id[i] <- id[i]
        result$nobs[i] <- nobs
    }
    result
}