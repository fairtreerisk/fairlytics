
#' Execute a SQL query with retry mechanism
#'
#' @description
#' `SendQuery` function attempts to execute a SQL query on a database connection with a retry mechanism.
#' If the query fails, it retries up to a specified number of attempts with a delay between each attempt.
#'
#' @param conn A database connection object. Typically created using `DBI::dbConnect()`.
#' @param query A character string representing the SQL query to be executed.
#' @param max_retries An integer specifying the maximum number of retry attempts if the query fails. Default is 2.
#' @param retry_delay_seconds An integer specifying the delay in seconds between retry attempts. Default is 30.
#'
#' @return A tibble containing the result of the SQL query. Returns `NULL` if the query fails after all retry attempts.
#'
#' @details
#' - The function logs the table name extracted from the query, the number of rows returned upon success,
#'   and any errors encountered during the execution.
#' - The retry mechanism ensures that transient errors, such as temporary database unavailability, are handled gracefully.
#' - The function uses `tryCatch()` for error handling and logs warnings for failed attempts.
#'
#' @examples
#' \dontrun{
#'   conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   query <- "SELECT * FROM my_table"
#'   result <- trySQLQuery(conn, query, max_retries = 3, retry_delay = 10)
#' }
#'
#' @importFrom DBI dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom stringr str_match str_trim
#' @importFrom glue glue
#'
#' @import logger
#'
#' @export
SendQuery <- function(conn, query, max_retries = 2, retry_delay_seconds = 30) {
  result <- NULL
  attempt <- 1
  success <- FALSE

  # Extract table name from query
  table_name <- query %>%
    str_match("from (.+?( withnolock| with (nolock)|\\n))") %>%
    .[2] %>%
    str_trim()

  while (!success && attempt <= max_retries) {
    tryCatch({
      # Execute SQL query and convert to tibble
      result <- dbGetQuery(conn, query) %>%
        as_tibble()

      log_info(glue(
        "trySQLQuery: ({table_name}). Rows returned: {nrow(result)}"
      ))

      success <- TRUE
    },
    error = function(e) {
      log_warn(glue(
        "trySQLQuery: ({table_name}) - Query error on attempt {attempt}: {e$message}"
      ))
    })

    if (!success) {
      Sys.sleep(retry_delay_seconds)
      attempt <- attempt + 1
    }
  }

  if (!success) {
    log_warn(glue("trySQLQuery: ({table_name}) - Query failed after {max_retries} attempts."))
  }

  return(result)
}
