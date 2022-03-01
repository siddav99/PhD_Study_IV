#' Prepare datset for SL
#'
#' Takes data as input and returns data.SL that is data for Sl.
#'
#' @param Data.frame. The data to be cleaned. No default.
#' @return A tibble.
#' @examples
#' \dontrun{
#' data.SL <- prepare_Sl_dataset(data)
#' }
#' @export
prepare_Sl_dataset <- function(data) {
  
  ## Check arguments
  assert_that(is.data.frame(data))
  
  ##Convert factors into numeric:
  data.SL <- data %>%
    
  mutate(
    
    ##Sex
    sex = recode(sex,"Female"= 0, "Male"= 1),
     
    ##Mechanism of Injury
    moi.grouped = recode(moi.grouped,"Animal bites"= 4, "Assault"= 5,"Falls"= 2,"Other"= 6, "Railway injuries"= 3, "Road traffic injuries"= 1),
 
  ##Type of Injury
  tyi = recode(tyi,"Blunt"= 0, "Penetrating"= 1, "Both" = 2),
  
  ##Mode of Transport
  mot.grouped = recode(mot.grouped,"Ambulance"= 0, "Police van"= 1, "Private Vehicles" = 2, "On Foot" = 3),
    
  ##Transfer Status
  tran = recode(tran,"Direct Admissions" = 0, "Transferred"= 1),
  
  ##Mortality at 30 days
  s30d = recode(s30d,"Dead" = 0, "Alive"= 1))
 
  
  ## Finally, return data.SL
  return (data.SL)
}