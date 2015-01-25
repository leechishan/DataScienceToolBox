pollutantmean <- function( directory, pollutant, id=1:332 )
{
    total_data <- 0
    total_count <- 0
    
    for ( i in id )
    {
        filename <- sprintf("%s/%03d.csv", directory, i)
        read_data <- read.csv(filename)
        pollutant_data <- read_data[!is.na(read_data[pollutant]), pollutant]
        
        total_data <- total_data + sum(pollutant_data)
        total_count <- total_count + length(pollutant_data)
    }
    
    total_data/total_count
}
