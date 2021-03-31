#' Plots qPCR Amplification Curves
#'
#' Plots amplification curves from qPCR raw data files
#' @param data object witht he raw data file.
#' @param treat data frame with sample description, including columns 'Content', 'Sample', and 'col'.
#' @param fixed_col whether to use fixed colors for standard, unknown, and NTC samples.   
#' @return A plot with qPCR amplification curves.
#' @keywords qPCR
#' @export
#' @examples
#' qpcr_plot()
qpcr_plot <- function(data, treat, fixed_col = F) {
  plot_par()
  plot(0, 0, type = "n", xlim = c(0, nrow(reads)), ylim = c(0, max(data)),
    xlab = "Cycle", ylab = "Fluorescence")
  grid()
  
  if(fixed_col) {
    for(i in rownames(treat[treat$Content == "Unkn", ])) {
      lines(data[, i], col = "black")
    }
    for(i in rownames(treat[treat$Content == "Std", ])) {
      lines(data[, i], col = "blue", lwd = 2)
    }
    for(i in rownames(treat[treat$Content == "NTC", ])) {
      lines(data[, i], col = "red", lwd = 2)
    }
    legend("topleft", legend = c("Std", "Unk", "NTC"),
      col = c("blue", 1, "red"), bty = "n", lty = 1, lwd = c(2, 1, 2))
  } else {
    for(i in rownames(treat)) {
      lines(data[, i], col = treat[i, "col"])
    }
    legend("topleft", legend = unique(treat$Sample), col = unique(treat$col),
      bty = "n", lty = 1)
  }
}
