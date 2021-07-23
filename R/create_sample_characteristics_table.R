#' Create Sample Characteristics Table 
#' 
#' Takes  data as input returns a table with complete cases and multiple imputation results 
#'
#' @param data Data.frame. The data with the .imp and .id columns. No default.
#' @return Docx with table in the working directory.
#' @examples
#' \dontrun{
#' data <- create_Sample_characteristics_table(data)
#' }
#' @export
create_sample_characteristics_table <- function(data) {

    ## Labels of Variables
    codebook <- list(age = list(full.label = "Age in years",
                                abbreviated.label = ""),
                     sex = list(full.label = "Sex",
                                abbreviated.label = ""),
                     moi.grouped = list(full.label = "Mechanism of injury",
                                        abbreviated.label = ""),
                     tyi = list(full.label = "Type of injury",
                                abbreviated.label = ""),
                     mot.grouped = list(full.label = "Mode of transport",
                                        abbreviated.label = ""),
                     tran = list(full.label = "Transferred",
                                 abbreviated.label = ""),
                     sbp = list(full.label = "Systolic blood pressure",
                                abbreviated.label = "SBP"),
                     rr = list(full.label = "Respiratory rate",
                               abbreviated.label = "RR"),
                     hr = list(full.label = "Heart rate",
                               abbreviated.label = "HR"),
                     spo2 = list(full.label = "Blood oxygen saturation",
                                 abbreviated.label = "SpO2"),
                     gcs = list(full.label = "Glasgow coma scale",
                                abbreviated.label = "GCS"),
                     #iss = list(full.label = "Injury severity score",
                                #abbreviated.label = "ISS"),
                     #rts = list(full.label = "Revised trauma score",
                                #abbreviated.label = "RTS"),
                     #triss = list(full.label = "Trauma injury severity score",
                                  #abbreviated.label = "TRISS"),
                     eq5dhs = list(full.label = "EQ5D Health Status",
                                  abbreviated.label = ""),
                     eq5dm = list(full.label = "EQ5D Mobility",
                                   abbreviated.label = ""),
                     eq5dsc = list(full.label = "EQ5D Self Care",
                                   abbreviated.label = ""),
                     eq5dua = list(full.label = "EQ5D Usual Activities",
                                  abbreviated.label = ""),
                     eq5dpd = list(full.label = "EQ5D Pain/Discomfort",
                                   abbreviated.label = ""),
                     eq5dad = list(full.label = "EQ5D Anxiety/Depression",
                                  abbreviated.label = ""),
                     s30d = list(full.label = "30 day mortality",
                                 abbreviated.label = ""))

    codebook.options <- list(full.label.entry = "full.label",
                             abbreviated.label.entry = "abbreviated.label")
    
    sample.characteristics.table <- bengaltiger::CreateSampleCharacteristicsTable(
                                                     data,
                                                     include.complete.data = TRUE,    
                                                     include.missing = TRUE,
                                                     codebook = codebook,
                                                     only.codebook.variables = TRUE,
                                                     codebook.options = codebook.options,
                                                     table.name = "Study Sample Characteristics",
                                                     return.pretty = TRUE,
                                                     save.to.disk = FALSE,
                                                     save.to.results = FALSE)
    last.row <- nrow(sample.characteristics.table)
    return.object <- list(caption = sample.characteristics.table[1, 1],
                          note = sample.characteristics.table[last.row, 1],
                          table = setNames(
                              data.frame(sample.characteristics.table[3:(last.row - 1), ]),
                              sample.characteristics.table[2, ]))
    return (return.object)
}
