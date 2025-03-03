% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_instrument_metaID.R
\name{ProcessInstrumentMetaID}
\alias{ProcessInstrumentMetaID}
\title{ProcessInstrumentMetaID}
\usage{
ProcessInstrumentMetaID(data_tbl, source_vector, source_type)
}
\arguments{
\item{data_tbl}{A dataset containing Instrument Meta ID information.}

\item{source_vector}{A character vector specifying the source names to filter.}

\item{source_type}{A character vector specifying the source types to filter.}
}
\value{
A tibble containing the filtered data.
}
\description{
The \code{ProcessInstrumentMetaID} function filters the input dataset
based on the specified source names and source types provided
via the \code{source_vector} and \code{source_type} arguments.
After filtering, it removes duplicate rows based on the \code{instrument_id} column.
}
\examples{
# Example dataset
data_tbl <- tibble::tibble(
  instrument_id = c(1, 2, 3, 1),
  source_name = c("SourceA", "SourceB", "SourceC", "SourceA"),
  source_type = c("Type1", "Type2", "Type1", "Type1"),
  source_code = c("A123", "B456", "C789", "A123")
)

# Specify source names and types to filter
source_vector <- c("SourceA", "SourceB")
source_type <- c("Type1", "Type2")

result <- ProcessInstrumentMetaID(data_tbl, source_vector, source_type)
print(result)
}
