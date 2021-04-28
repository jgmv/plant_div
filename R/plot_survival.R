#' Plots Probability of Survival over Time
#'
#' Plots lines showing the probability of survival over time
#' @param data data frame with the input data.
#' @param plant abbreviated name of the plant species to be plotted.
#' @return A plot with probability of survival over time.
#' @export
plot_survival <- function(data, plant) {
  require(ggplot2)
  x <- droplevels(data[data$species == plant, ])
  x <- na.omit(x)
  inoc_date <- min(as.Date(x$date))
  x$dpi <- abs(inoc_date - as.Date(x$date))
  x[x$symptoms > 0, "symptoms"] <- 1
  fit <- survival::survfit(survival::Surv(dpi, symptoms) ~ fungus, data = x)
  survminer::ggsurvplot(fit, data = x, conf.int = T, main = plant,
    legend = "right", legend.labs = unique(x$fungus), xlab = "dpi",
    ylab = "survival prob.")
}
