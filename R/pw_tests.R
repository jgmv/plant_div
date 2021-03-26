
#' Performs Pairwise Multiple Tests
#'
#' Performs pairwise multiple tests with either t or Wilcoxon tests
#' @param x data frame with the input data. One column named 'species' must contain the plant abbreviated names, and a column 'fungus' must contain the fungal treatment.
#' @param var name of column in 'x' where the variable to be plotted is stored.
#' @param stest test to be done: t for t tests, w for Wilcoxon tests. 
#' @param adjust_method p adjust method as in p.adjust.
#' @param alpha significance level, to draw asterisks in the output.        
#' @return A data frame with pairwise test results.
#' @export
pw_tests <- function(x, var = "weight", stest = "t", adjust_method = "BH",
  alpha = 0.05) {
  if(stest == "t") {
    message("Multiple t tests") 
    tstat <- "t"
  } else {
    message("Multiple Wilcoxon tests") 
    tstat <- "W"
  }
  r <- matrix(NA, nrow = length(unique(x$species)), ncol = 4,
    dimnames = list(unique(x$species), c(tstat, "p", "padj", "sig")))
  r <- as.data.frame(r)
  for(i in unique(x$species)) {
    y <- x[x$species == i, ]
    if(stest == "t") {
      tr <- t.test(y[, var] ~ y$fungus)
    } else {
      tr <- wilcox.test(y[, var] ~ y$fungus)
    }
   
    attributes(tr)
    r[i, 1] <- tr$statistic
    r[i, "p"] <- tr$p.value
  }
  r$padj <- p.adjust(r$p, method = adjust_method)
  for(i in 1:nrow(r)) {
    if(r[i, "padj"] <= alpha) {
      r[i, "sig"] <- "*"
    } else {
      r[i, "sig"] <- ""
    }
  }
  return(r)
}
