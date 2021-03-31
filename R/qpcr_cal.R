#' qPCR Calibration
#'
#' Plots calibration curves for qPCR data, calculates amplification efficiency, and quantifies unknown samples based on standards.
#' @param sample name of samples in column 'Sample' of 'treat' to use as standards.
#' @param treat data frame with sample description.   
#' @return A phylogenetic tree in APE format.
#' @keywords phylogeny
#' @export
#' @examples
#' qpcr_cal()
qpcr_cal <- function(sample, data) { 
  plot_par(xaxs = "r", yaxs = "r")
  x <- data[data$Sample == sample, ]
  plot(x$Cq ~ log10(x$SQ), xlab = "log10(ng DNA)", ylab = "Cq")
  mod <- lm(x$Cq ~ log10(x$SQ))
  mods <- summary(mod)
  cd <- round(mods$adj.r.squared, 2)
  slp <- mods$coefficients[2, 1]
  eff <- round((-1 + 10^(-1 / slp)) * 100, 1)
  abline(lm(x$Cq ~ log10(x$SQ)))
  text(par("usr")[1], par("usr")[3],
    paste0("R2 = ", round(cd, 2), ", E = ", eff, " %"), adj = c(-0.1, -1))

  cq <- data[data$Content == "Unkn", "Cq"]
  names(cq) <- data[data$Content == "Unkn", "Sample"]
  qty <- rep(NA, length(cq))
  names(qty) <- names(cq)
  for(i in 1:length(cq)) {
    qty[i] <- 10^((cq[i] - mod[[1]][1]) / mod[[1]][2])
  }
  qty[is.na(qty)] <- 0
  result <-  data.frame(sample = names(qty), qty)
  result$r2 <- rep(cd, nrow(result))
  result$efficiency <- rep(eff, nrow(result))
  return(result)
}

