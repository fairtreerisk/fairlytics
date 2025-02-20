% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/email_365.R
\name{Send365Mail}
\alias{Send365Mail}
\title{Send365Mail}
\usage{
Send365Mail(receiver, mail_cc, subject, mail_body = "", mail_attachment = NULL)
}
\arguments{
\item{receiver}{A character vector of email addresses to receive the email.}

\item{mail_cc}{A character vector of email addresses to be added as CC recipients.}

\item{subject}{A character string representing the subject of the email.}

\item{mail_body}{A character string representing the body of the email.
Defaults to \code{NULL}, which means no body is set. If provided, it should be in HTML format.}

\item{mail_attachment}{A character vector containing file paths to attachments to be included in the email.
Defaults to \code{NULL}, meaning no attachments are included.}
}
\value{
None. The function sends an email but does not return any values.
}
\description{
The \code{Send365Mail} function sends an email using Microsoft 365's Outlook service via the \code{Microsoft365R} package.
It allows you to specify the recipients, subject, body, and optional attachments.
After each function call, it performs a 5-second pause to avoid excessive email sending.
}
