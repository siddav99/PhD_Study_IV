#' Calculate the revised trauma score
#' 
#' Takes  data as input and returns data.rts, i.e. data with a column with RTS calculated
#'
#' @param data Data.frame. The data to which RTS has to be added. No default.
#' @return A tibble.
#' @examples
#' \dontrun{
#' data.rts <- calculate_rts(data)
#' }
#' @export
calculate_rts <- function(data) {
    ## Check arguments
    assert_that(is.data.frame(data))
    
    ## Start with data.rts as a copy of data
    data.rts <- data
    
    ## Giving coded values to GCS, RR and SBP
    data.rts$gcs.rts <-  data.rts$gcs
    data.rts$gcs.rts[data.rts$gcs >= 13] <- 4
    data.rts$gcs.rts[data.rts$gcs >= 9 &  data.rts$gcs < 13] <- 3
    data.rts$gcs.rts[data.rts$gcs >= 6 &  data.rts$gcs < 9] <- 2
    data.rts$gcs.rts[data.rts$gcs >= 4 &  data.rts$gcs < 6] <- 1
    data.rts$gcs.rts[data.rts$gcs < 4] <- 0
    
    data.rts$sbp.rts <-  data.rts$sbp
    data.rts$sbp.rts[data.rts$sbp > 89] <- 4
    data.rts$sbp.rts[data.rts$sbp >=76 &  data.rts$sbp <= 89] <- 3
    data.rts$sbp.rts[data.rts$sbp >= 50 &  data.rts$sbp <= 75] <- 2
    data.rts$sbp.rts[data.rts$sbp >= 1 &  data.rts$sbp <= 49] <- 1
    data.rts$sbp.rts[data.rts$sbp == 0] <- 0
    
    data.rts$rr.rts <-  data.rts$rr
    data.rts$rr.rts [data.rts$rr >= 10 &  data.rts$rr <= 29] <- 4
    data.rts$rr.rts [data.rts$rr > 29] <- 3
    data.rts$rr.rts [data.rts$rr >= 6 &  data.rts$rr <= 9] <- 2
    data.rts$rr.rts [data.rts$rr >= 1 &  data.rts$rr <= 5] <- 1
    data.rts$rr.rts [data.rts$rr == 0] <- 0
    
    ## Calculating RTS
    data.rts$rts <- with(data.rts, (data.rts$gcs.rts * 0.9368) + (data.rts$sbp.rts * 0.7326)
                         + (data.rts$rr.rts * 0.2908))

    ## Convert data into tibble
    dplyr::as_tibble(data.rts)
    
    ## Returning data with RTS
    return(data.rts)
}
