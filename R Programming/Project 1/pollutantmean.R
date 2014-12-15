pollutantmean <- function(directory, pollutant, id = 1:332){
    size <- length(id)
    directories <- character(size)
    temp_count <- 1
    str <- paste(directory , "/", sep = "")
    
    for(i in id){
        
        file <- ""
        if(i >= 1 && i < 10){
            file <- paste("00" , i, sep = "")
        }
        else if(i >= 10 && i < 100){
            file <- paste("0" , i, sep = "")
        }
        else if(i >= 100 && i < 1000){
            file <- paste("" , i, sep = "")
        }
        else file <- i
        
        file <- paste(file, ".csv", sep = "")
        
        directories[temp_count] <- paste(str, file, sep = "")
        temp_count <- temp_count + 1
    }
    
    final <- c(NA)
    
    for(j in 1:size){
        data <- read.csv(directories[j])
        specCol <- data[[pollutant]]
        final <- c(final, specCol)
    }
    
    mean(final, na.rm = TRUE)
}