complete <- function( directory, id=1:332 )
{
    ret_data <- data.frame(id=numeric(0), nobs=numeric(0))
    
    for ( i in id )
    {
        filename <- sprintf("%s/%03d.csv", directory, i)
        read_data <- read.csv(filename)
        read_frame <- data.frame(id=i, nobs=nrow(read_data[complete.cases(read_data),]))
        
        ret_data <- rbind(ret_data, read_frame)
    }
    
    ret_data
}
