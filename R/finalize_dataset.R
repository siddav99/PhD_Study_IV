#' Final Dataset
#' 
#' Takes data as input and returns data.final, i.e. dataset with
#' variables for the study
#'
#' @param data Data.frame. No default.
#' @return A tibble.
#' @examples
#' \dontrun{
#' final.dataset <- finalize_dataset(data)
#' }
#' @export
finalize_dataset <- function(data) {
    ## Check arguments
    assert_that(is.data.frame(data))
    
    ## Select variables
    final.dataset <- data %>% select(age,
                                     sex,
                                     moi.grouped,
                                     tyi,
                                     mot.grouped,
                                     tran,
                                     sbp,
                                     rr,
                                     hr,
                                     spo2,
                                     gcs,
                                     s30d,
                                     eq5dhs,
                                     eq5dm,
                                     eq5dsc,
                                     eq5dua,
                                     eq5dpd,
                                     eq5dad)
    
    ## Return clean data
    return (final.dataset)
}

