#' Calculate the TRISS score
#' 
#' Takes data as input and returns data.triss, i.e. data with a column with TRISS calculated
#'
#' @param data Data.frame. The data to which TRISS has to be added. No default.
#' @return A tibble.
#' @examples
#' \dontrun{
#' data.triss <- calculate_triss(data)
#' }
#' @export
calculate_triss_score <- function(data) {
    
    ## Check arguments
    assert_that(is.data.frame(data))
    
    ## Start with data.triss as a copy of data
    data.triss <- data
    
    ## Age (55 and over is assigned 1, rest 0)
    data.triss <- data.triss %>%
        mutate(age.triss = if_else(data.triss$age >= 55, 1, 0))
    
    ## TRISS exponent constant
    ## Constant for blunt is used for blunt as well as both (blunt&penetrating)
    
    data.triss <- data.triss %>%
        mutate(const.triss = if_else(tyi == "Penetrating",
        ((-2.5355) + (0.9934*rts) + (-0.0651*iss) + (-1.1360*age.triss)),
        ((-0.4499) + (0.8065*rts) + (-0.0835*iss) + (-1.7340*age.triss))
        ))
    
    data.triss <- data.triss %>% mutate(triss = 1/(1+exp(-const.triss)))
    
    ## Returning data with RTS
    return(data.triss)
}

