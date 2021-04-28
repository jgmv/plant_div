#' Plots Plant Symptoms over Time
#'
#' Plots lines showing the symptoms rate over time
#' @param data data frame with the input data.
#' @param plant abbreviated name of the plant species to be plotted.
#' @param ind_val plot individual values or means with standard deviations.   
#' @return A plot with symptoms rates over time.
#' @export
plot_symptoms <- function(data, plant, ind_val = F) {
  require(ggplot2)
  inoc_date <- min(as.Date(data$date))
  data$dpi <- abs(inoc_date - as.Date(data$date))

  x <- droplevels(data[data$species == plant, ])
  x$treat <- paste(x$fungus, x$plant, sep = "_")
  x.mean <- as.table(tapply(x$symptoms, list(x$fungus, x$dpi), FUN = mean))
  x.mean <- as.data.frame(x.mean)
  colnames(x.mean) <- c("fungus", "dpi", "symptoms")
  x.sd <- as.table(tapply(x$symptoms, list(x$fungus, x$dpi), FUN = sd))
  x.sd <- as.data.frame(x.sd)
  colnames(x.sd) <- c("fungus", "dpi", "symptoms")
  x.mean$sd <- x.sd$symptoms
  x.mean$sd_min <- x.mean$symptoms - x.mean$sd
  x.mean$sd_max <- x.mean$symptoms + x.mean$sd
  x.mean$sd_max[x.mean$sd_max > 4] <- 4
  x.mean$sd_min[x.mean$sd_min < 0] <- 0

  if(ind_val) {
    ggplot(data = x, aes(x = dpi, y = symptoms, group = treat, color = fungus)) +
    geom_step() + theme_classic() +
    ylab("symptoms rate") +
    xlab("dpi") +
    ggtitle(plant) +
    ylim(0, 4) +
    theme(axis.text = element_text(colour = "black", size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))
  } else {
    ggplot(data = x.mean, mapping = aes(x = dpi, y = symptoms, group = fungus)) +
    geom_step(aes(color = fungus), size = 1) + theme_classic() +
    utile.visuals::geom_stepconfint(mapping = aes(ymin = sd_min, ymax = sd_max,
      fill = fungus), alpha = 0.5) +
    ylab("symptoms rate") +
    xlab("dpi") +
    ggtitle(plant) +
    ylim(0, 4) +
    theme(axis.text = element_text(colour = "black", size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))
  }
}
