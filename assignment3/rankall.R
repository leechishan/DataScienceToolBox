rankall <- function(outcome, num = "best") 
{
    ## Read outcome data
    file_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    valid_outcome <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if ( length(valid_outcome[[outcome]]) == 0 )
    {
        stop("invalid outcome")
    }
    
    split_state <- split( file_data, file_data[,"State"] )

    rank <- function(split_data)
    {
        curr_state <- split_data[1,"State"]
        
        death_rate_data <- data.frame(Hospital.Name=split_data[,"Hospital.Name"], 
                                      Death_Rate=as.numeric(split_data[,valid_outcome[[outcome]]]),
                                      stringsAsFactors=FALSE)
        
        death_rate_data <- death_rate_data[!is.na(death_rate_data$Death_Rate),]    
        
        if ( is.numeric(num) )
        {
            if ( num > nrow(death_rate_data) )
            {
                return(data.frame(hospital=NA,state=curr_state))
            }
        }
        
        ii <- order(death_rate_data$Death_Rate, death_rate_data$Hospital.Name)
        death_rate_data <- death_rate_data[ii,]
        
        hospital_ret <- NA
        if ( is.character(num) )
        {
            if ( num == "best" )
            {
                hospital_ret <- data.frame(hospital=death_rate_data[1,"Hospital.Name"], 
                                           state=curr_state)
            }
            else if ( num == "worst" )
            {
                hospital_ret <- data.frame(hospital=death_rate_data[nrow(death_rate_data),"Hospital.Name"], 
                                           state=curr_state)
            }
        }
        else if ( is.numeric(num) )
        {
            hospital_ret <- data.frame(hospital=death_rate_data[num,"Hospital.Name"], 
                                       state=curr_state)
        }

        hospital_ret
    }
        
    do.call(rbind,lapply(split_state, rank))
}
