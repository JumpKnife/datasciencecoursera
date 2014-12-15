corr <- function(directory, threshold = 0){
    
    size <- 332
    directories <- character(size)
    temp_count <- 1
    str <- paste(directory , "/", sep = "")
    
    for(i in 1:size){
        
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
    
    myData <- complete(directory)
    mySet <- subset(myData, nobs > threshold)
    ids <- mySet[["id"]]
    
    res <- c()
    
    for(j in ids){
        temp <- read.csv(directories[j])
        tmp <- cor(temp[["nitrate"]], temp[["sulfate"]], use = "complete.obs")
        res <- c(res, tmp)
    }
    
    res
    
}