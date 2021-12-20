#' Phylogeny of Host Plants
#'
#' Generates and plots a phylogenetic tree with plant species from the diversity experiment. The phylogeny is based on Zanne et al. 2014.
#' @param sel an optional vector with the plant abbreviations to be used.
#' @param tree_tips format of the tree tips in the plot: full, full species name; abr, species abbreviation; both, both full and abbreviated species names.   
#' @return A phylogenetic tree in APE format.
#' @keywords phylogeny
#' @export
#' @examples
#' phy_plants()
#' phy_plants(sel = c("LEU", "PLA", "ARR", "ANT"))
phy_plants <- function(sel = NULL, tree_tips = "both") {
  species <- c("Achillea millefolium",
    "Agrostis stolonifera",
    "Anthoxanthum odoratum",
    "Arrhenatherum elatius",
    "Briza media",
    "Centaurea jacea",
    "Festuca pratensis",
    "Festuca rubra",
    "Galium mollugo",
    "Holcus lanatus",
    "Leontodon hispidus",
    "Leucanthemum vulgare",
    "Plantago lanceolata",
    "Phleum pratense",
    "Prunella vulgaris",
    "Ranunculus repens",
    "Sanguisorba officinalis",
    "Trisetum flavescens")
  abrv <- c("ACH",
    "AGR",
    "ANT",
    "ARR",
    "BRI",
    "CEN",
    "FPR",
    "FRU",
    "GAL",
    "HOL",
    "LEO",
    "LEU",
    "PLA",
    "PHL",
    "PRU",
    "RAN",
    "SAN",
    "TRI")
  guild <- c("forb",
    "grass",
    "grass",
    "grass",
    "grass",
    "forb",
    "grass",
    "grass",
    "forb",
    "grass",
    "forb",
    "forb",
    "forb",
    "grass",
    "forb",
    "forb",
    "forb",
    "grass")
  plants <- data.frame(species = species, guild = guild, row.names = abrv)
  plants$col <- plant_guild_col(plants$guild)
  if(!is.null(sel)) {
    sel <- as.character(sel)
    plants <- plants[sel, ]
  }
  tree <- comecol::phylomatic_like(plants$species, dataset = "plants",
    sep = " ")
  # manually addd Festuca rubra, not in Zanne et al. 2014
  tree$tip.label[tree$tip.label == "Festuca pratensis"][2] <- "Festuca rubra"
  plants <- plants[order(match(plants$species, tree$tip.label)), ]
  tree_out <- tree
  tree_out$tip.label <- rownames(plants)
  if(tree_tips == "full") {
    tree$tip.label <- misctools::format_string(tree$tip.label, i = 1:2,
      spt = " ")
  } else if(tree_tips == "abr") {
    tree$tip.label <- rownames(plants)
    tree$tip.label <- misctools::format_string(tree$tip.label)
  } else {
    tree$tip.label <- paste0(plants$species, " (", rownames(plants), ")")
    tree$tip.label <- misctools::format_string(tree$tip.label, i = 1:2,
      spt = " ")
  }
  plot(tree, no.margin = T, label.offset = 10, edge.width = 2)
  ape::tiplabels(pch = 15, offset = 5, cex = 1.5, col = plants$col)
  legend("topleft", legend = unique(plants$guild), pch = 15, col = unique(plants$col),
    pt.cex = 2.5, bty = "n", inset = c(0.05, 0.025))
  print(plants)
  return(tree_out)
}
