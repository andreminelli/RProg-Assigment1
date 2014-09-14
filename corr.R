corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    result <- numeric(0)
    files <- list.files(directory, full.names = TRUE)
    for(i in 1:length(files)) { 
        data <- read.csv(files[i])
        complete_cases <- complete.cases(data)
        nobs <- sum(complete_cases)
        if (nobs >  threshold) {
            complete_data <- data[complete_cases,]
            result <- c(result, cor(complete_data$nitrate, complete_data$sulfate))        
        }
    }
    result
}