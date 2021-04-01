#' Import Raw Data from WinRhizo
#'
#' Import raw scan data from WinRhizo.
#' @param file path to file with raw data.
#' @param classes whether to include root size classes.
#' @param skip number of lines at the beginning of file to skip, to remove data headers.
#' @return A data frame with selected root traits.
#' @keywords WinRhizo
#' @export
#' @examples
#' import_winrhizo()
import_winrhizo <- function(file, classes = F, skip = 5) {
  data <- read.csv(file, h = T, sep = "\t")
  cols <- c("RHIZO.2015a", "Length.cm.", "ProjArea.cm2.", "SurfArea.cm2.",
    "AvgDiam.mm.", "LenPerVol.cm.m3.", "RootVolume.cm3.", "Tips", "Forks",
    "Crossings")
  clss <- colnames(data)[53:102]
  if(classes) {
    data <- data[, c(cols, clss)]
  } else {
    data <- data[, cols]
  }
  data <- data[-(1:skip), ]
  rownames(data) <- gsub(".tif$", "", data[, "RHIZO.2015a"])
  data <- data[, -which(colnames(data) == "RHIZO.2015a")]
  return(data)
}  
