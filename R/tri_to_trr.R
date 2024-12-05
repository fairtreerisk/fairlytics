
#' TRItoTRR
#'
#' @description
#' The function takes a TRI object and convert the Index values to Returns
#'
#' @param dfTRI TRI dataframe in long format (columns: date, Ticker, TR_Index)
#'
#' @return TRR datafarme
#' @export
#'
#' @import dplyr
#'
#' @importFrom magrittr %>%
#'
TRItoTRR <- function(dfTRI) {

  dfTRI <- dfTRI %>% dplyr::arrange(date) # ensure consistency in date sequence

  # Filter the TRI object and calculate Returns
  tblReturn <- dfTRI %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(Ticker) %>%
    dplyr::mutate(Return = (TR_Index - lag(TR_Index)) / lag(TR_Index)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TR_Index)

  return(tblReturn)

}
