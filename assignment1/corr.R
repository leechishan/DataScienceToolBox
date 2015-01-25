corr <- function( directory, threshold=0 )
{
    cor_data <- numeric(0)
    
    for ( i in 1:332 )
    {
        filename <- sprintf("%s/%03d.csv", directory, i)
        read_data <- read.csv(filename)
        complete_cases <- complete.cases(read_data)
        
        if ( nrow(read_data[complete.cases(read_data),]) > threshold )
        {
            nitrate_data <- read_data[complete_cases, "nitrate"]
            sulfate_data <- read_data[complete_cases, "sulfate"]
            
            cor_data <- c(cor_data, cor(nitrate_data,sulfate_data))
        }
    }
    
    cor_data
}
