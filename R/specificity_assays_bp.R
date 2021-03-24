#' Plots Boxplots of Specificity Assays Next to a Host Plant Phylogeny
#'
#' Plots boxplots of fungus-inoculated vs uninoculated plants, next to a phylogeny of the host plant
#' @param x data frame with the input data. One column named 'species' must contain the plant abbreviated names, and a column 'fungus' must contain the fungal treatment.
#' @param var name of column in 'x' where the variable to be plotted is stored.
#' @param x_lab optional label for the x axis.   
#' @return A stacked plot with a phylogeny and a boxplot.
#' @export
specificity_assays_bp <- function(x, var = "weight", x_lab = "var") {
  require(patchwork)
  x$var <- x[, var]
  tree <- phy_plants(sel = unique(x$species))

  scale_y_tree <- function(expand = expansion(0, 0.6), ...){
    scale_y_continuous(expand = expand, ...)
  }
  tr <- ggtree(tree) + scale_y_tree()

  p <- x %>%
    dplyr::mutate(species = forcats::fct_relevel(species, tree$tip.label)) %>%
    ggplot(aes(y = species, x = var, fill = fungus)) + 
    geom_boxplot(outlier.shape = NA, notch = F) + theme_classic() +
    scale_fill_manual(values = c("white", "grey")) +
    scale_color_manual(values = c("grey", "black")) +
    theme(axis.text = element_text(colour = "black", size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)) +
    geom_point(aes(color = fungus), position = position_jitterdodge()) +
    xlab(x_lab) + ylab("")

  tr + p #+ plot_annotation(tag_levels = "A")
}
