
#' Send365Mail
#'
#' @description
#' The `Send365Mail` function sends an email using Microsoft 365's Outlook service via the `Microsoft365R` package.
#' It allows you to specify the recipients, subject, body, and optional attachments.
#' After each function call, it performs a 5-second pause to avoid excessive email sending.
#'
#' @param receiver A character vector of email addresses to receive the email.
#' @param mail_cc A character vector of email addresses to be added as CC recipients.
#' @param subject A character string representing the subject of the email.
#' @param mail_body A character string representing the body of the email.
#' Defaults to `NULL`, which means no body is set. If provided, it should be in HTML format.
#' @param mail_attachment A character vector containing file paths to attachments to be included in the email.
#' Defaults to `NULL`, meaning no attachments are included.
#'
#' @return None. The function sends an email but does not return any values.
#'
#' @importFrom Microsoft365R get_business_outlook
#'
#' @export
#'
Send365Mail <- function(receiver, mail_cc, subject, mail_body = NULL, mail_attachment = NULL){

  outl <- get_business_outlook()
  Email <- outl$create_email(content_type = 'html')
  Email$set_recipients(to = receiver, cc = mail_cc)
  Email$set_subject(subject)
  Email$set_body(mail_body)

  if (!is.null(mail_attachment))
    for (attch in mail_attachment)
      Email$add_attachment(attch)

  Email$send()
  Sys.sleep(5)
}
