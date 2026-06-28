#' Box plot with the data points overlaid
#'
#' @description Box plots can sometimes mask the distribution of data. Create a boxplot with the individual data points overlaid. Note that the outliers are turned off since the data points are all visible.
#'
#' @param data A data frame containing the groups and corresponding values
#' @param group A vector containing the grouping variables
#' @param value The corresponding values for each group
#' @param highlight_group Logical. TRUE to emphasize a group in the plot by alpha
#' @param add_mean Add the mean value as a triangle to the plot
#' @param use_palette Logical. TRUE to use colorblind-safe Okabe–Ito color palette
#'
#' @return A ggplot2 object
#'
#' @examples
#' data <- data.frame(group = rep(c("a", "b", "c")),
#'                    value = c(rnorm(30, mean = 6, sd = 5),
#'                              rnorm(30, mean = 2, sd = 4),
#'                              rnorm(30, mean = 12, sd = 7)))
#'
#' boxplot_lb(data = data, group = group, value = value,
#'            use_palette = TRUE, add_mean = TRUE, highlight_group = "b")
#'
#'
#' @export
#'
boxplot_lb <- function(data,
                       group,
                       value,
                       highlight_group = NULL,
                       add_mean = FALSE,
                       use_palette = FALSE) {

  #Define Okabe–Ito color palette
  pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7", "#000000")

  #Get col names as strings
  group_var <- deparse(substitute(group))
  value_var <- deparse(substitute(value))

  #If highlight_group, make that group alpha = 1 and others 0.2
  if (!is.null(highlight_group) && is.character(highlight_group)) {
    data$.alpha <- ifelse(data[[group_var]] == highlight_group, 1, 0.2)
  } else {
    #else, alpha is 0.8
    data$.alpha <- 0.8
  }

  #Create plot
  plot <-
    ggplot2::ggplot(data,
                    ggplot2::aes(x = .data[[group_var]],
                                 y = .data[[value_var]],
                                 color = .data[[group_var]],
                                 alpha = .data[[".alpha"]])) +

    #Box plot
    ggplot2::geom_boxplot(ggplot2::aes(alpha = .data[[".alpha"]]),
                          outlier.shape = NA, width = 0.6) +


    #Add points over boxplot
    ggplot2::geom_jitter() +

    ggplot2::scale_alpha_identity() +

    #Edit theme
    ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.7))

    # Use Okabe–Ito color palette if TRUE
    if (isTRUE(use_palette)) {
      plot <- plot +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::scale_fill_manual(values = pal)
    }

  # Add mean if TRUE
  if(isTRUE(add_mean)) {
    plot <- plot +
      ggplot2::stat_summary(
        ggplot2::aes(group = group),
        fun = mean,
        geom = "point",
        shape = 17,
        size = 2,
        color = "gray30")

  }
  return(plot)
}
