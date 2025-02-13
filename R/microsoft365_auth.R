
#' SetupMicrosoft365Auth
#'
#' @description
#' The `SetupMicrosoft365Auth` function initializes and creates an authentication token for interacting with Microsoft 365 products.
#' This token is essential when using functionality such as accessing user information within Microsoft 365.
#'
#' **Note:** When `SetupMicrosoft365Auth` is run for the first time, it prompts for authorization via the Authenticator app.
#'
#' @param tenant A string specifying the Azure AD tenant name or ID.
#' @param app_id A string specifying the client ID (application ID) of your registered Azure AD app.
#'
#' @return A Microsoft 365 authentication token object.
#'
#' @import Microsoft365R
#' @import AzureAuth
#' @import AzureGraph
#'
#' @export
SetupMicrosoft365Auth <- function(tenant = "fairtree.com", app_id="d44a05d5-c6a5-4bbb-82d2-443123722380") {

  # Microsoft 365 scopes
  scopes <- c(
    "https://graph.microsoft.com/Files.ReadWrite.All",
    "https://graph.microsoft.com/User.Read",
    "openid",
    "offline_access"
  )

  # remove existing tokens and sessions
  clean_token_directory()
  delete_graph_login(tenant = tenant)

  tryCatch({
    # Azure token
    token <- AzureAuth::get_azure_token(
      resource = scopes,
      tenant = tenant,
      app = app_id,
      version = 2
    )

    # Create a new graph login session
    graph_login <- AzureGraph::create_graph_login(
      tenant = tenant,
      auth_type = "authorization_code"
    )

    # graph logins
    AzureGraph::list_graph_logins()

    message("Microsoft 365 token created successfully.")
    return(graph_login)

  }, error = function(e) {
    message("An error occurred during authentication: ", e$message)
  }, finally = {
    # remove expired tokens remain
    AzureAuth::delete_azure_token("offline_access", tenant, app_id)
  })
}
