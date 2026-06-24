#' @title Plot an NMDS object with ggplot2
#'
#' @description Create a custom NMDS plot using ggplot2
#'
#' @param NMDS A `metaMDS` or `monoMDS` object from the vegan package
#' @param group A vector indicating group membership for each observation
#'
#' #' @seealso
#' - [ggplot2 website](https://ggplot2.tidyverse.org/) - Official ggplot2 documentation
#' - [vegan reference manual](https://cran.r-project.org/web/packages/vegan/vegan.pdf)
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
plot_nmds <- function(nmds, group = NULL) {

  #Get scores from NMDS
  nmds_scores <- as.data.frame(
    vegan::scores(nmds, display = "sites")
  )

  #Attatch group
  nmds_scores$group <- group

  #Get stress value from NMDS
  nmds_stress <- nmds$stress

  #Get convex hull
  nmds_hull <- nmds_scores |>
    dplyr::group_by(group) |>
    dplyr::slice(chull(NMDS1, NMDS2))

  if (!is.null(group)) {
    nmds_scores$group <- group
  }

  ggplot2::ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = group)) +
    ggplot2::geom_point() +

    #Add convex hull
    geom_polygon(data = nmds_hull,
                 aes(x = NMDS1, y = NMDS2, fill = group, group = group),
                 alpha = 0.3) +

    #Add stress value
    ggplot2::annotate("text", x = Inf, y = Inf,
                      label = paste("Stress value =", round(nmds_stress, 3)),
                      hjust = 1.1, vjust = 1.5) +

    #Edit theme
    ggplot2::labs(x = "NMDS1", y = "NMDS2") +
    ggplot2::theme(axis.text = element_blank(), axis.ticks = element_blank(),
                   panel.background = element_rect(fill = "white"),
                   panel.border = element_rect(color = "black",
                                               fill = NA, linewidth = .5),
                   axis.line = element_line(color = "black"),
                   plot.title = element_text(hjust = 0.5),
                   legend.key.size = unit(.25, "cm"))
}
