
##' ProcessInstrumentMetaID
#'
#' @description
#' The `ProcessInstrumentMetaID` function filters the input dataset
#' based on the specified source names and source types provided
#' via the `source_vector` and `source_type` arguments.
#' After filtering, it removes duplicate rows based on the `instrument_id` column.
#'
#' @param data_tbl A dataset containing Instrument Meta ID information.
#' @param source_vector A character vector specifying the source names to filter.
#' @param source_type A character vector specifying the source types to filter.
#'
#' @return A tibble containing the filtered data.
#' @export
#'
#' @examples
#' # Example dataset
#' data_tbl <- tibble::tibble(
#'   instrument_id = c(1, 2, 3, 1),
#'   source_name = c("SourceA", "SourceB", "SourceC", "SourceA"),
#'   source_type = c("Type1", "Type2", "Type1", "Type1"),
#'   source_code = c("A123", "B456", "C789", "A123")
#' )
#'
#' # Specify source names and types to filter
#' source_vector <- c("SourceA", "SourceB")
#' source_type <- c("Type1", "Type2")
#'
#' result <- ProcessInstrumentMetaID(data_tbl, source_vector, source_type)
#' print(result)
ProcessInstrumentMetaID <- function(data_tbl, source_vector, source_type){

  # filter and select select columns
  df <- data_tbl %>%
    filter(source_name %in% source_vector,
           source_type %in% source_type) %>%
    select("instrument_id",
           "source_type",
           "InstrumentCode" = "source_code") %>%
    distinct()

  # Remove duplicated InstrumentIds
  df2 <- df[!duplicated(df[["instrument_id"]]),]

  return(df2)

}

