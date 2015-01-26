rankhospital <- function(state, outcome, num = "best") 
{
    ## Read outcome data
    file_data <- read.csv("outcome-of-care-measures.csv",colClasses="character")    
    
    ## Check that state and outcome are valid
    valid_state <- file_data[,"State"] == state
    if ( sum(valid_state) == 0 )
    {
        stop("invalid state")
    }
    
    valid_outcome <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if ( length(valid_outcome[[outcome]]) == 0 )
    {
        stop("invalid outcome")
    }
    
    death_rate_data <- data.frame(Hospital.Name=file_data[valid_state,"Hospital.Name"], 
                                  Death_Rate=as.numeric(file_data[valid_state,valid_outcome[[outcome]]]),
                                  stringsAsFactors=FALSE)
    
    death_rate_data <- death_rate_data[!is.na(death_rate_data$Death_Rate), ]
    
    if ( is.numeric(num) )
    {
        if ( num > nrow(death_rate_data) )
        {
            return(NA)
        }
    }
    
    ii <- order(death_rate_data$Death_Rate, death_rate_data$Hospital.Name)
    death_rate_data <- death_rate_data[ii,]
    
    hospital_ret <- NA
    if ( is.character(num) )
    {
        if ( num == "best" )
        {
            hospital_ret <- death_rate_data[1, "Hospital.Name"]
        }
        else if ( num == "worst" )
        {
            hospital_ret <- death_rate_data[nrow(death_rate_data), "Hospital.Name"] 
        }
    }
    else if ( is.numeric(num) )
    {
        hospital_ret <- death_rate_data[num, "Hospital.Name"]
    }
    
    hospital_ret
}
