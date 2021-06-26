#' Data Description
#'
#' Takes data as input and returns a description of the variables in a patient-based database. 
#' It provides the mean, median, IQR of continuous variables such as age and 
#' proportion in percentages of categorical variables disease/condition grades as well as 
#' binary outcomes such as mortality.
#' 
#'
#' @param data A data.frame. The data to be summarized. No default.
#' @return A list
#' @export
#' 
describe_data <- function(data) {
    ## Check arguments
    assert_that(is.data.frame(data))
    
    ## Initialize the list of descriptions
    data.description <- list()
    
    ##Total number of patients
    data.description$total.patients <- nrow(data)
    
    ## Age
    age.summary <- summary(data$age)
    data.description$age.median <- signif(age.summary[[3]],digits = 3)
    data.description$age.iqr.upper <- signif(age.summary[[2]],digits=3)
    data.description$age.iqr.lower <- signif(age.summary[[5]],digits=3)
    
    ## Sex
    sex.summary <- table(data$sex)
    data.description$proportion.male <- round(as.numeric(sex.summary[[2]])/
                                              as.numeric(NROW(na.omit(data$sex)))*100,2)
    
    ## Mechanism of Injury
    
    ## Applying calculate_level_name function on mechanism of injury
    moi.data <- lapply(setNames(nm = levels(data$moi.grouped)), calculate_level_name, factor.var = data$moi.grouped)
    data.description$p.road <- moi.data[["road traffic injuries"]][["percent"]]
    data.description$p.fall <- moi.data[["falls"]][["percent"]]
    data.description$p.railway <- moi.data[["railway injuries"]][["percent"]]
    data.description$p.animalbite <- moi.data[["animal bites"]][["percent"]]
    data.description$p.assualt <- moi.data[["assault"]][["percent"]]
    data.description$p.other <- moi.data[["other"]][["percent"]]
    
    ## Applying sort_most_common function on mechanism of injury
    sorted.moi <- sort_most_common(data$moi.grouped)
    data.description$moi1 <- sorted.moi[1]
    data.description$moi2 <- sorted.moi[2]
    data.description$moi3 <- sorted.moi[3]
    data.description$moi4 <- sorted.moi[4]
    data.description$moi5 <- sorted.moi[5]
    
    ## Type of Injury
    tyi.summary <- table(data$tyi)
    data.description$proportion.blunt <- round(as.numeric(tyi.summary[[1]])/as.numeric(NROW(na.omit(data$tyi)))*100,2)
    
    ## Mode of Transport
    mot.summary <- table(data$mot)
    data.description$proportion.ambulance <- round(as.numeric(mot.summary[[1]])/as.numeric(NROW(na.omit(data$mot)))*100,2)
    data.description$proportion.police <- round(as.numeric(mot.summary[[2]])/as.numeric(NROW(na.omit(data$mot)))*100,2)
    data.description$proportion.private <- round(as.numeric(mot.summary[[3]])/as.numeric(NROW(na.omit(data$mot)))*100,2)
    data.description$proportion.foot <- round(as.numeric(mot.summary[[4]])/as.numeric(NROW(na.omit(data$mot)))*100,2)
    
    ## Applying sort_most_common function on mode of transport
    data$mot.grouped <- as.factor(data$mot)
    sorted.mot <- sort_most_common(data$mot.grouped)
    data.description$mot1 <- sorted.mot[1]
    data.description$mot2 <- sorted.mot[2]
    data.description$mot3 <- sorted.mot[3]
    data.description$mot4 <- sorted.mot[4]
    
    ## Patient Transfer status
    tran.summary <- table(data$tran)
    data.description$proportion.transferred <- round(as.numeric(tran.summary[[2]])/as.numeric(NROW(na.omit(data$tran)))*100,2)
    
    ## ISS
    #iss.summary <- summary(data$iss)
    #data.description$iss.mean <- signif(iss.summary[[4]],digits = 3)
    #data.description$iss.median <- signif(iss.summary[[3]],digits = 3)
    #data.description$iss.iqr.lower <- signif(iss.summary[[2]],digits=3)
    #data.description$iss.iqr.upper <- signif(iss.summary[[5]],digits=3)
    #data.description$iss.min <- signif(iss.summary[[1]],digits=3)
    #data.description$iss.max <- signif(iss.summary[[6]],digits=3)
    
    ## Subgrouping ISS
    #iss.group <- cut(clean.data$iss, breaks = c(0,10,16,25,36), 
                     #labels = c("Minor","Moderate","Severe","Very Severe"))
    #iss.group.summary <- table(iss.group)
    #data.description$proportion.minor.iss <- round(as.numeric(iss.group.summary[[1]])/
                                                   #as.numeric(NROW(na.omit(data$iss)))*100,2)
    ## Mortality
    dm <- as.numeric(NROW(na.omit(data$s30d)))
    
    ## Applying the function calculate_mortality_outcome
    outcome.data <- lapply(setNames(nm = levels(data$s30d)), calculate_mortality_outcome, data = data)
    
    data.description$p.outcome.dead <- outcome.data[["Dead"]][["percent"]]
    data.description$n.outcome.dead <- outcome.data[["Dead"]][["nm"]]
    data.description$total.outcome <- as.numeric(NROW(na.omit(clean.data$s30d)))  
    
    ## Finally, return the description of the data
    return (data.description)
}

sort_most_common <- function(v) {
    uniqv <- as.character(unique(v))
    uniqv[sort.list(tabulate(match(v, uniqv)), decreasing = TRUE)]
}

calculate_level_name <- function(level.name, factor.var) {
    assert_that(is.character(level.name))
    assert_that(is.factor(factor.var))
    nm <- sum(factor.var == level.name)
    dm <- length(factor.var)
    percent <- round((nm/dm) * 100,2)
    return.list <- list(nm = nm, percent = percent)
    return (return.list)
}

calculate_mortality_outcome <- function(level.name, data) {
    outcome.level <- dplyr::filter(data, data$s30d == level.name) 
    nm <- nrow(outcome.level)
    dm <- nrow(data)
    percent <- round((nm/dm) * 100,2)
    return.list <- list(nm = nm, percent = percent)
    return (return.list)
}
