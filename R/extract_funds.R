
#' Extract Funds
#'
#'#' @description
#' The `ExtractFunds` function processes raw holdings data to extract and format fund information.
#' It creates a data frame with distinct fund names and their descriptions, separating the descriptions
#' into components such as portfolio, benchmark, and differences. Users can choose whether to include
#' benchmark information in the output.
#'
#' @param RawHoldingsData A data frame containing raw holdings data. It should include columns named `name` and `header_report_universes`.
#' @param includeBenchmarks A boolean flag indicating whether to include benchmark information in the output.
#' Default is `TRUE`. If set to `FALSE`, the benchmark column will be excluded from the resulting data frame.
#'
#' @return A data frame with the following columns:
#' - `report_name`: The name of the fund or report.
#' - `port`: The portfolio description extracted from the `header_report_universes`.
#' - `bench`: The benchmark description extracted from the `header_report_universes` (included only if `includeBenchmarks` is `TRUE`).
#' - `diff`: Any additional differences extracted from the `header_report_universes`.
#'
#' @export
#'
#' @import dplyr
#' @import stringr
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' # Extracting fund information with benchmarks included
#' ExtractFunds(RawHoldingsData)
#'
#' # Extracting fund information without benchmarks
#' ExtractFunds(RawHoldingsData, includeBenchmarks = FALSE)
ExtractFunds <- function(RawHoldingsData,includeBenchmarks = T){
  df <- RawHoldingsData %>%
    distinct(report_name = name, description = header_report_universes) %>%
    separate(description,
             sep = "\\(1\\) |\\(2\\) |\\(3\\) ",
             into = c("dropme", "port", "bench", "diff"),
             extra = "warn") %>%
    select(report_name,port,bench,diff)

  if (includeBenchmarks == F) {

    df <- df %>% select(-bench)
  }

  return(df)
}
