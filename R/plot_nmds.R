#' @title Plot an NMDS object with ggplot2
#'
#' @description Create a custom NMDS plot using ggplot2
#'
#' @param NMDS A `metaMDS` or `monoMDS` object from the vegan package
#' @param group Numeric. A vector indicating group membership for each observation
#' @param use_palette Logical. TRUE to use colorblind-safe Okabe–Ito color palette
#'
#' @seealso
#' - [ggplot2 website](https://ggplot2.tidyverse.org/) - Official ggplot2 documentation
#' - [vegan reference manual](https://cran.r-project.org/web/packages/vegan/vegan.pdf)
#' - [Okabe and Ito (2008)]()
#'
#' @references
#'  Ichihara et al. (2008). Color Universal Design -The Selection of Four Easily Distinguishable Colors
#'  for all Color Vision Types. https://jfly.uni-koeln.de/color/ichihara_etal_2008.pdf
#'
#'  Oksanen, J. et al. (2026). _vegan: Community Ecology Package_.
#'  doi:10.32614/CRAN.package.vegan.
#'
#'  Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis.
#'  Springer-Verlag New York.
#'
#' @return A ggplot2 object
#'
#' @examples
#' library(ggplot2)
#' library(vegan)
#'
#' comm.df <- data.frame(group = rep(c("control", "treatment"), each = 10),
#'                      matrix(rpois(20*10, lambda = 3), nrow = 20, ncol = 10))
#'
#' nmds_result <- vegan::metaMDS(comm.df[,2:11], k = 2, trymax = 50)
#'
#' plot_nmds(nmds_result, group = comm.df$group)
#'
#' @export
plot_nmds <- function(nmds,
                      group = NULL,
                      use_palette = FALSE) {

  #Define Okabe–Ito color palette
  pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7", "#000000")

  #Get scores from NMDS
  nmds_scores <- as.data.frame(
    vegan::scores(nmds, display = "sites"))

  #Attatch group
  nmds_scores$group <- group

  #Get stress value from NMDS
  nmds_stress <- nmds$stress

  #Get convex hull
  nmds_hull <- nmds_scores |>
    dplyr::group_by(group) |>
    dplyr::slice(chull(NMDS1, NMDS2))

  #Get centroid for each gropu
  nmds_centroid <- nmds_scores |>
    dplyr::group_by(group) |>
    dplyr::summarize(g1 = mean(NMDS1),
                     g2 = mean(NMDS2))


  #Add group column if not NULL
  if (!is.null(group)) {
    nmds_scores$group <- group
  }

  #Create plot
  plot <-
    ggplot2::ggplot(nmds_scores,
    ggplot2::aes(x = NMDS1, y = NMDS2, color = group)) +

    #Add points
    ggplot2::geom_point() +

    #Add convex hull
    ggplot2::geom_polygon(data = nmds_hull,
                 ggplot2::aes(x = NMDS1, y = NMDS2, fill = group, group = group),
                 alpha = 0.3) +

    #Add centroid
    ggplot2::geom_point(data = nmds_centroid,
                        ggplot2::aes(x = g1, y = g2), size = 5) +

    #Add stress value
    ggplot2::annotate("text", x = Inf, y = Inf,
                      label = paste("Stress value =", round(nmds_stress, 2)),
                      hjust = 1.1, vjust = 1.5) +

    #Edit theme
    ggplot2::labs(x = "NMDS1", y = "NMDS2") +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_rect(color = "black",
                                               fill = NA, linewidth = .5),
                   axis.line = ggplot2::element_line(color = "black"),
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.key.size = grid::unit(.25, "cm"))

  # Use Okabe–Ito color palette if TRUE
  if (use_palette) {
    plot <- plot +
                ggplot2::scale_color_manual(values = pal) +
                ggplot2::scale_fill_manual(values = pal)
  }
  return(plot)
}
