#' Describe missing data
#'
#' Creates a list of missing data descriptions
#'
#' In a manuscript you typically want to describe your missing
#' data. You do this by describing how much missing there is in total
#' and then how much missing there was in each variable. You might
#' want to state which variables had the most missing data.
#' @param data A data.frame. The data. No default.
#' @return A list of lists. The nested list `total.incomplete` gives the
#'     number and percentages of incomplete observations.
#' @export
describe_missing_data <- function(data) {
    ## Check arguments
    assert_that(is.data.frame(data))

    ## Initialize the list of descriptions
    descriptions <- list()

    ## Total number of incomplete observations
    descriptions$total.incomplete$n <- sum(!complete.cases(data))
    descriptions$total.incomplete$p <- round(mean(!complete.cases(data)) * 100)
    
    ## Age
    age <- data$age
    descriptions$age <- get_missing_n_p(age)
    
    ## Sex
    sex <- data$sex
    descriptions$sex <- get_missing_n_p(sex)

    ## Mechanism of Injury
    moi.grouped <- data$moi.grouped
    descriptions$moi.grouped <- get_missing_n_p(moi.grouped)
    
    ## Type of Injury
    tyi <- data$tyi
    descriptions$tyi <- get_missing_n_p(tyi)
    
    ## Mode of transport
    mot.grouped <- data$mot.grouped
    descriptions$mot.grouped <- get_missing_n_p(mot.grouped)
    
    ## Patient Transfer Status
    tran <- data$tran
    descriptions$tran <- get_missing_n_p(tran)
    
    ## Systolic blood pressure
    sbp <- data$sbp
    descriptions$sbp <- get_missing_n_p(sbp)
    
    ## Heart Rate
    hr <- data$hr
    descriptions$hr <- get_missing_n_p(hr)
    
    ## Blood Oxygen Saturation
    spo2 <- data$spo2
    descriptions$spo2 <- get_missing_n_p(spo2)
    
    ## Respiratory Rate
    rr <- data$rr
    descriptions$rr <- get_missing_n_p(rr)
    
    ## Mortality
    s30d <- data$s30d
    descriptions$s30d <- get_missing_n_p(s30d)
    
    ### ISS
    #iss <- data$iss
    #descriptions$iss <- get_missing_n_p(iss)
    
    ##EQ5D Health Status
    eq5dhs <- data$eq5dhs
    descriptions$eq5dhs <- get_missing_n_p(eq5dhs)
    

    ## Return descriptions
    return (descriptions)
}

#' Get number and proportions of missing data
#' @param x Vector. No default.
#' @return A list with two elements, `n` and `p`, where `n` is the
#'     number and `p` is the percentage of missing data.
get_missing_n_p <- function(x) {
   
    n <- sum(is.na(x))
    p <- round(mean(is.na(x)) * 100,2)#I added digits =2 as certain p were just 0
    return (list(n = n, p = p))
}
