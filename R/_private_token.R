library(gh)

save_github_token <- function(){
  AUTH_PAT_TOKEN <- gh_token()

  # Path to the user's .Renviron file
  renviron_path <- normalizePath("~/.Renviron", mustWork = FALSE)

  if (!file.exists(renviron_path)) {
    # Create a new .Renviron file if it doesn't exist
    file.create(renviron_path)
  }

  # Check if GITHUB_PAT is already set
  renviron_contents <- readLines(renviron_path)
  if (any(grepl("^GITHUB_PAT=", renviron_contents))) {
    renviron_contents <- renviron_contents[!grepl("^GITHUB_PAT=", renviron_contents)]
  }

  # Append the new token
  renviron_contents <- c(renviron_contents, paste0("GITHUB_PAT=", AUTH_PAT_TOKEN))
  writeLines(renviron_contents, renviron_path)

  message("GitHub token saved to .Renviron. Restart your R session to apply changes.")

}
