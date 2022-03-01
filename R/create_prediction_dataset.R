#' Final Dataset
#' 
#' Takes data as input and returns data.final, i.e. dataset with
#' variables for the study
#'
#' @param data Data.frame. No default.
#' @return A tibble.
#' @examples
#' \dontrun{
#'predict.dataset <- create_prediction_dataset(data)
#' }
#' @export
create_prediction_dataset <- function(data) {
    ## Check arguments
    assert_that(is.data.frame(data))
    
    ## Select variables
    predict.dataset <- data %>% select(age,
                                     sex,
                                     moi.grouped,
                                     tyi,
                                     mot.grouped,
                                     tran,
                                     sbp,
                                     rr,
                                     hr,
                                     spo2,
                                     gcs
                                     )
    
    ## Return clean data
    return (predict.dataset)
}

