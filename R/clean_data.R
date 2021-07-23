#' Clean data
#'
#' Takes "dirty" data as input and returns "clean" data.
#'
#' @param dirty.data Data.frame. The data to be cleaned. No default.
#' @return A tibble.
#' @examples
#' \dontrun{
#' clean.data <- clean_data(dirty.data)
#' }
#' @export
clean_data <- function(dirty.data) {
    ## Check arguments
    assert_that(is.data.frame(dirty.data))

    ## Start with clean.data as a copy of dirty.data
    clean.data <- dirty.data
    
    ## Age
    age <- as.numeric(dirty.data$age)
    summary(age)
    hist(age)
    age <- dplyr::na_if(age, 999) 
    age <- dplyr::na_if(age, 0) 
    age[age < 18] <- NA 
    clean.data$age <- age

    ## The lines between 
    ## here ----
    age.summary <- summary(clean.data$age)
    age.median <- signif(age.summary[[3]],digits = 3)
    age.iqr.upper <- signif(age.summary[[2]],digits=3)
    age.iqr.lower <- signif(age.summary[[5]],digits=3)
    ## and here are not about cleaning the data but rather to describe
    ## the data, right? I suggest you move it there.

    ## Sex
    ## See my comment on age above
    sex <- as.numeric(dirty.data$sex)
    summary(sex)
    table(sex)
    sex <- dplyr::na_if(sex, 999)
    sex <- factor(sex, levels = c(0,1), labels = c("Female", "Male"))
    clean.data$sex <- sex
    sex.summary <- table(clean.data$sex)
    proportion.male <- round(as.numeric(sex.summary[[2]])/as.numeric(NROW(na.omit(clean.data$sex)))*100,2)


    ### Mechanism of Injury
    moi <- dplyr::na_if(dirty.data$moi, 999)
    table(moi)
    clean.data$moi <- moi
    
    ## Creating function to create ICD Range
    get_icd_range <- function(first.char, start.number, end.number) {
        icd.range <- paste0(
            paste0(
                "^",
                first.char,
                stringr::str_pad(start.number:end.number, 2, pad = "0"),
                "[0-9]?$"),
            collapse = "|")
        return (icd.range)
    }
    moi.grouped <- moi
    icd.ranges <- list(c("Road traffic injuries" = get_icd_range("V", 0, 99)),
                       c("Railway injuries" = get_icd_range("V", 81, 81)),
                       c("Falls" = get_icd_range("W", 0, 19)),
                       c("Animal bites" = get_icd_range("W", 50, 64)),
                       c("Assault" = paste0(get_icd_range("X", 85, 99), "|", get_icd_range("Y", 0, 9))))
    for(icd.range in icd.ranges) moi.grouped[grep(icd.range, moi)] <- names(icd.range)
    moi.grouped[!(moi.grouped %in% sapply(icd.ranges, attr, "names"))] <- "Other"
    clean.data$moi.grouped <- as.factor(moi.grouped)
    
    ## Creating function for Sorting most common variable
    sort_most_common <- function(v) {
        uniqv <- unique(v)
        uniqv[sort.list(tabulate(match(v, uniqv)), decreasing = TRUE)]
    }
    
    ## Creating function for Creating levels
    dm <- as.numeric(nrow(clean.data))
    
    calculate_level_name <- function(level.name, factor.var) {
        assert_that(is.character(level.name))
        assert_that(is.factor(factor.var))
        nm <- sum(factor.var == level.name)
        percent <- round((nm/dm) * 100,2)
        return.list <- list(nm = nm, percent = percent)
        return (return.list)
    }
    
    
    ## Apply calculate_level_name function on mechanism of injury
    moi.data <- lapply(setNames(nm = levels(clean.data$moi.grouped)), calculate_level_name, factor.var = clean.data$moi.grouped)
    
    p.road <- moi.data[["Road traffic injuries"]][["percent"]]
    p.fall <- moi.data[["Falls"]][["percent"]]
    p.railway <- moi.data[["Railway injuries"]][["percent"]]
    p.animalbite <- moi.data[["Animal bites"]][["percent"]]
    p.assualt <- moi.data[["Assault"]][["percent"]]
    p.other <- moi.data[["Other"]][["percent"]]
    
    ##Applying sort_most_common function on mechanism of injury
    sorted_moi <- sort_most_common(clean.data$moi.grouped)
    moi1 <- sorted_moi[[1]][]
    moi2 <- sorted_moi[[2]][]
    moi3 <- sorted_moi[[3]][]
    moi4 <- sorted_moi[[4]][]
    moi5 <- sorted_moi[[5]][]
    
    ### Type of Injury
    tyi <- as.numeric(dirty.data$tyi)
    table(tyi)
    tyi <- dplyr::na_if(tyi, 999)
    tyi <- ifelse(tyi == "0", 0, ifelse(tyi == "1", 1, 1))
    tyi <- factor(tyi, levels = c(0,1), labels = c("Blunt", "Penetrating"))
    clean.data$tyi <- tyi
    tyi.summary <- table(clean.data$tyi)
    proportion.blunt <- round(as.numeric(tyi.summary[[1]])/as.numeric(NROW(na.omit(clean.data$tyi)))*100,2)
    
    ## Mode of Transport
    mot <- as.numeric(dirty.data$mot)
    table(mot)
    mot <- dplyr::na_if(mot,999)
    mot <- factor(mot, levels = c(0,1,2,3), labels = c("Ambulance","Police van", "Private Vehicles","On Foot"))
    clean.data$mot <- mot
    mot.summary <- table(clean.data$mot)
    proportion.ambulance <- round(as.numeric(mot.summary[[1]])/as.numeric(NROW(na.omit(clean.data$mot)))*100,2)
    proportion.police <- round(as.numeric(mot.summary[[2]])/as.numeric(NROW(na.omit(clean.data$mot)))*100,2)
    proportion.private <- round(as.numeric(mot.summary[[3]])/as.numeric(NROW(na.omit(clean.data$mot)))*100,2)
    proportion.foot <- round(as.numeric(mot.summary[[4]])/as.numeric(NROW(na.omit(clean.data$mot)))*100,2)
    
    ## Applying sort_most_common function on mode of transport
    clean.data$mot.grouped <- as.factor(clean.data$mot)
    sorted_mot <- sort_most_common(clean.data$mot.grouped)
    mot1 <- sorted_mot[[1]][]
    mot2 <- sorted_mot[[2]][]
    mot3 <- sorted_mot[[3]][]
    mot4 <- sorted_mot[[4]][]
    
    ## Patient Transfer status
    tran <- as.numeric(dirty.data$tran)
    table(tran)
    tran <- dplyr::na_if(tran,999)
    tran <- factor(tran, levels = c(0,1), labels = c("Direct Admissions","Transferred"))
    clean.data$tran <- tran
    tran.summary <- table(clean.data$tran)
    proportion.transferred <- round(as.numeric(tran.summary[[2]])/as.numeric(NROW(na.omit(clean.data$tran)))*100,2)
    
    ## GCS
    ## Eye
    egcs <- as.numeric(dirty.data$egcs)
    table(egcs)
    egcs <- dplyr::na_if(egcs,999)
    egcs <- dplyr::na_if(egcs,99)
    clean.data$egcs <- egcs
    
    ## Verbal
    vgcs <- as.numeric(dirty.data$vgcs)
    table(vgcs)
    vgcs <- dplyr::na_if(vgcs,999)
    vgcs <- dplyr::na_if(vgcs,99)
    clean.data$vgcs <- vgcs
    
    ## Motor
    mgcs <- as.numeric(dirty.data$mgcs)
    table(mgcs)
    mgcs <- dplyr::na_if(mgcs,999)
    mgcs <- dplyr::na_if(mgcs,99)
    clean.data$mgcs <- mgcs
    
    ##Calculating GCS
    
    gcs <- with(clean.data, clean.data$mgcs+ clean.data$egcs + clean.data$vgcs)
    table(gcs)
    gcs <- dplyr::na_if(gcs,999)
    clean.data$gcs <- as.numeric(gcs)
   
    
    ## Systolic blood pressure
    sbp <- as.numeric(dirty.data$sbp1)
    summary(sbp)
    sbp <- dplyr::na_if(sbp,999)
    clean.data$sbp <- sbp
        
    ## Heart Rate
    hr <- as.numeric(dirty.data$hr1)
    summary(hr)
    hr <- dplyr::na_if(hr,999)
    clean.data$hr <- hr
    
    ## Blood Oxygen Saturation
    spo2 <- as.numeric(dirty.data$spo21)
    summary(spo2)
    spo2 <- dplyr::na_if(spo2,999)
    clean.data$spo2 <- spo2
    
    ## Respiratory Rate
    rr <- as.numeric(dirty.data$rr1)
    summary(rr)
    rr <- dplyr::na_if(rr,999)
    clean.data$rr <- rr
    
    # ISS
    #iss <- as.numeric(dirty.data$iss)
    #summary(iss)
    #iss[is.na(iss)] <- 0#Replacing NA with "0" as most missing ISS scores are due to no injuries
    #clean.data$iss <- iss
    #iss.summary <- summary(clean.data$iss)
    #iss.mean <- signif(iss.summary[[4]],digits = 3)
    #iss.iqr.lower <- signif(iss.summary[[2]],digits=3)
    #iss.iqr.upper <- signif(iss.summary[[5]],digits=3)
    #iss.min <- signif(iss.summary[[1]],digits=3)
    #iss.max <- signif(iss.summary[[6]],digits=3)
    
    ## Subgrouping ISS
    #clean.data$iss.group <- cut(clean.data$iss, breaks = c(0,10,16,25,36), labels = c("Minor","Moderate","Severe","Very Severe"))
    #iss.group.summary <- table(clean.data$iss.group)
    #proportion.minor.iss <- round(as.numeric(iss.group.summary[[1]])/as.numeric(NROW(clean.data$iss.group))*100,2)
    
    ## Mortality
    s30d <- as.numeric(dirty.data$s30d)
    table(s30d)
    s30d <- gsub("2", "0",s30d)#Combining "Alive and admitted to a different hospital" as "alive"
    s30d <- dplyr::na_if(s30d,999)
    s30d <- factor(s30d, levels = c(0,1), labels = c("Alive","Dead"))
    clean.data$s30d <- s30d
    
    
    ##Creating a function to calculate_mortality_outcome
    dm <- as.numeric(NROW(na.omit(clean.data$s30d)))
    calculate_mortality_outcome <- function(level.name) {
        outcome.level <- dplyr::filter(clean.data, clean.data$s30d == level.name) 
        nm <- nrow(outcome.level)
        percent <- round((nm/dm) * 100,2)
        return.list <- list(nm = nm, percent = percent)
        return (return.list)
    }
    
    ## Applying the function calculate_mortality_outcome
    outcome.data <- lapply(setNames(nm = levels(clean.data$s30d)), calculate_mortality_outcome)
    p.outcome.dead <- outcome.data[["Dead"]][["percent"]]
    n.outcome.dead <- outcome.data[["Dead"]][["nm"]]
    total.outcome <- as.numeric(NROW(na.omit(clean.data$s30d)))
    
    
    ## EQ5D Health Status
    eq5dhs <- as.numeric(dirty.data$eq5dhs)
    summary(eq5dhs)
    eq5dhs <- dplyr::na_if(eq5dhs,999)
    eq5dhs <- dplyr::na_if(eq5dhs,-1)
    clean.data$eq5dhs <- eq5dhs
    
    ## EQ5D Mobility
    eq5dm <- as.numeric(dirty.data$eq5dm)
    summary(eq5dm)
    eq5dm <- dplyr::na_if(eq5dm,999)
    eq5dm <- dplyr::na_if(eq5dm,99)
    eq5dm <- dplyr::na_if(eq5dm,-1)
    eq5dm <- factor(eq5dm, levels = c(0,1,2), labels = c("No Problems", "Some Problems", "Confined to bed"))
    clean.data$eq5dm <- eq5dm
    eq5dm.summary <- table(clean.data$eq5dm)
    
    ## EQ5D Selfcare
    eq5dsc <- as.numeric(dirty.data$eq5dsc)
    summary(eq5dsc)
    eq5dsc <- dplyr::na_if(eq5dsc,999)
    eq5dsc <- dplyr::na_if(eq5dsc,99)
    eq5dsc <- dplyr::na_if(eq5dsc,-1)
    eq5dsc <- factor(eq5dsc, levels = c(0,1,2), labels = c("No Problems", "Some Problems", "Unable to wash or dress"))
    clean.data$eq5dsc <- eq5dsc
    
    ## EQ5D Usual Activities
    eq5dua <- as.numeric(dirty.data$eq5dua)
    summary(eq5dua)
    eq5dua <- dplyr::na_if(eq5dua,999)
    eq5dua <- dplyr::na_if(eq5dua,99)
    eq5dua <- dplyr::na_if(eq5dua,-1)
    eq5dua <- factor(eq5dua, levels = c(0,1,2), labels = c("No Problems", "Some Problems", "Unable to perform usual activities"))
    clean.data$eq5dua <- eq5dua
    
    ## EQ5D Pain
    eq5dpd <- as.numeric(dirty.data$eq5dpd)
    summary(eq5dpd)
    eq5dpd <- dplyr::na_if(eq5dpd,999)
    eq5dpd <- dplyr::na_if(eq5dpd,99)
    eq5dpd <- dplyr::na_if(eq5dpd,-1)
    eq5dpd <- factor(eq5dpd, levels = c(0,1,2), labels = c("No Pain", "Moderate Pain", "Extreme Pain"))
    clean.data$eq5dpd <- eq5dpd
    
    ## EQ5D Anxiety
    eq5dad <- as.numeric(dirty.data$eq5dad)
    summary(eq5dad)
    eq5dad <- dplyr::na_if(eq5dad,999)
    eq5dad <- dplyr::na_if(eq5dad,99)
    eq5dad <- dplyr::na_if(eq5dad,-1)
    eq5dad <- factor(eq5dad, levels = c(0,1,2), labels = c("No Anxious/depressed", "Moderately Anxious/depressed", "Extremely Anxious/depressed"))
    clean.data$eq5dad <- eq5dad

    ## Finally, return clean data
    return (clean.data)
}
