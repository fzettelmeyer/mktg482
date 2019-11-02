#' A function to update the mkgt482 package
#'
#' This function allows you to easily update the mkgt482 package from github
#' @param none
#' @keywords update
#' @export
#' @examples
#' update_mktg482()

update_mktg482 <- function() {
  devtools::install_github("fzettelmeyer/mktg482", upgrade = "never", force = TRUE)
  }
