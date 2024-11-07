
#' TRRtoTRI
#'
#' @description
#' Converts a TRR dataframe to a TRI dataframe, starting with an index of 1
#'
#'
#' @param dfTRR TRR dataframe (columns: date, Ticker, TR_Index)
#' @param FocusAt character string. Should the index of 1 be set at the 'Start' or the 'End' , Default: 'Start'
#'
#' @return TRI dataframe
#' @export
#'
#' @examples TRRtoTRI(dfTRR = TRR_df, FocusAt ="Start")
TRRtoTRI <- function(dfTRR, FocusAt = "Start") {

  # Build Cumulative Returns
  mutate_call_cum = lazyeval::interp(~cumprod(1 + ifelse(is.na(x),0,x)), x = as.name("Return"))

  # Set all NA values back to NA
  mutate_call_NA = lazyeval::interp(~ifelse(is.na(y), NA, x), x = as.name("TR_Index"), y = as.name("Return"))

  # Find the first numeric value, and set 1 row down to 1
  mutate_call_1 = lazyeval::interp(~ifelse(is.na(x) & !is.na(lead(x,1)) , 1, x), x = as.name("TR_Index"))

  dfTRR <- dfTRR %>% dplyr::arrange(date) # ensure consistency in date sequence

  # Calculate TRI from Returns
  tblReturn <-  dfTRR %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(Ticker) %>%
    dplyr::mutate("TR_Index" = {{ (mutate_call_cum) }}) %>%
    dplyr::mutate("TR_Index" = {{ (mutate_call_NA) }}) %>%
    dplyr::mutate("TR_Index" = {{ (mutate_call_1) }}) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Return)

  if (toupper(FocusAt) == "END") {

    # *******************************
    #  Divide all by Final Value so the
    # TRI ends at 1
    # *******************************
    Final <-  tblReturn %>%
      dplyr::top_n(1, date) %>%
      dplyr::rename(Divisor = TR_Index) %>%
      dplyr::select(-date)

    tblRescale <- tblReturn %>%
      dplyr::left_join(Final, by = "Ticker") %>%
      dplyr::mutate(TR_Index = TR_Index / Divisor) %>%
      dplyr::select(-Divisor)

    tblReturn <-  tblRescale
  }

  if (is.character(FocusAt) ==  FALSE) {

    # *******************************
    #  Divide all by Value at FocusAt so the
    # TRI focuses at 1 at that point
    # *******************************
    Focus <- tblReturn %>%
      filter(date == FocusAt) %>%
      dplyr::rename(Divisor = TR_Index) %>%
      dplyr::select(-date)

    tblRescale <- tblReturn %>%
      dplyr::left_join(Focus, by = "Ticker") %>%
      dplyr::mutate(TR_Index = TR_Index / Divisor) %>%
      dplyr::select(-Divisor)

    tblReturn <-  tblRescale
  }

  tblReturn

}
