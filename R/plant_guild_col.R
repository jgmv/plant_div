#' Color Key for Plant Ecological Groups
#'
#' Provides standard colors for plant ecological groups used in the diversity experiment.
#' @param x an optional vector with the guilds to be included.
#' @return A vector with colors.
#' @keywords color
#' @export
#' @examples
#' plant_guild_col()
plant_guild_col <- function(x = NULL) {
  gcol <- c("#87de87ff", "#8787deff", "#d3bc5fff")
  names(gcol) <- c("grass", "legume", "forb")
  if(is.null(x)) x <- gcol
  x <- as.character(x)
  return(gcol[x])
}
