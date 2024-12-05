
#' SendRDComMail
#'
#' @description
#' The `SendRDComMail` function sends an email using the RDCOMCLIENT's Outlook service.
#' It allows you to set recipients, CCs, subject, body, and optional attachments.
#' Additionally, a flag can be set to mark the email for a reply.
#' The function also includes a 5-second delay after sending each email to avoid rapid successive emails.
#'
#' @param receiver A character vector of email addresses for the primary recipients of the email.
#' @param mail_cc A character vector of email addresses for CC recipients. Defaults to an empty string if not specified.
#' @param subject A character string specifying the subject of the email.
#' @param mail_body A character string specifying the body of the email in HTML format. Defaults to `NULL`.
#' If `NULL`, the email will only contain the default Outlook signature.
#' @param mail_attachment A character vector containing file paths for attachments. Defaults to `NULL`, meaning no attachments are added.
#' @param flag A logical value indicating whether to flag the email for reply. Defaults to `FALSE`.
#'
#' @return None. This function sends an email and does not return any value.
#'
#' @details
#' - **Signature Inclusion**: The email includes the default Outlook signature by appending it to the email body.
#' - **Attachments**: You can include one or multiple attachments by providing a vector of file paths.
#' - **Delay**: A delay of 5 seconds is added after sending the email to prevent excessive calls to the Outlook API.
#'
#' @importFrom RDCOMClient COMCreate
#'
#' @export
SendRDComMail <- function(receiver, mail_cc, subject, mail_body = NULL, mail_attachment = NULL, flag = F){

  Outlook <- COMCreate("Outlook.Application")

  # Create a new message
  Email = Outlook$CreateItem(0)

  Email$GetInspector()
  signature <-  Email[["htmlbody"]]

  # Set the recipient, subject, and body
  Email[["to"]] = receiver
  Email[["cc"]] = mail_cc
  Email[["bcc"]] = ""
  Email[["subject"]] = subject
  Email[["htmlbody"]] = paste0(mail_body, signature)

  if (flag == T) {
    Email[["flagrequest"]] = "Reply"
  }

  #Add Attachment
  if (is.null(mail_attachment) == FALSE ) {
    for (i in 1:length(mail_attachment)) {
      Email[["attachments"]]$Add(mail_attachment[i])
    }
  }

  Email$Display()
  Email$Send()
  Sys.sleep(5)

  # Close Outlook, clear the message
  rm(Outlook, Email)

}

