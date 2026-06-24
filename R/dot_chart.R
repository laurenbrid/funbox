#' Dot Chart
#'
#' Create a Cleveland dot plot using ggplot2. This is useful for visualizing categorical data.
#'
#' @param data a data frame containing the classifying variables and the corresponding counts for each combination of the classifying variables. This kind of data frame is easily created using \code{as.data.frame.table()} on a table object created with \code{xtabs()}. (See examples below.) The minimum number of columns is two. The maximum number of columns is four.
#' @param formula a formula with the left-hand side containing the counts and the right-hand side containing the cross-classifying variables. The order of the classifying variables determines their role in the plot. The first variable will be on the y-axis. The second variable will form the rows in a call to \code{facet_grid()}. If the second variable is also the last variable, it will be the faceting variable in a call to \code{facet_wrap}. The third variable will form the columns in a call to \code{facet_grid()}. If no formula is provided, the function assumes the data frame was created with \code{as.data.frame.table()} and looks for counts in the last column.
#' @param segments Add segments from axis to dots. Default is TRUE.
#'
#' @returns Returns a Cleveland dot chart.
#'
#' @seealso [dotchart()] for base R version.
#'
#' @references \url{https://hbiostat.org/bbr/descript#categorical-variables-1}
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' # 3 dimensions
#' dot_chart(Freq ~ Dept + Gender + Admit, data = UCBAdmissions)
#'
#' # 2 dimensions
#' xtabs(Freq ~ Dept + Gender, data = UCBAdmissions) |>
#'   as.data.frame() |>
#'   dot_chart()

# 1 dimension
#' xtabs(Freq ~ Dept, data = UCBAdmissions) |>
#'   as.data.frame() |>
#'   dot_chart()
#'
#' # example with proportions
#' proportions(UCBAdmissions, margin = c(2,3)) |>
#'   as.data.frame(responseName = "Proportion") |>
#'   dot_chart(formula = Proportion ~ Dept + Gender + Admit)
#'
#' # turn off segments
#' proportions(UCBAdmissions, margin = c(2,3)) |>
#'   as.data.frame(responseName = "Proportion") |>
#'   dot_chart(formula = Proportion ~ Dept + Gender + Admit,
#'             segments = FALSE)
dot_chart <- function(data, formula = NULL, segments = TRUE){
  if(is.null(formula)){
    i <- c(ncol(data), 1:(ncol(data) - 1))
    yx <- data[,i]
  } else yx <- stats::model.frame(formula, data = data)
  if (ncol(yx) < 2)
    stop("fewer than two variables")
  if (ncol(yx) > 4)
    stop("more than four variables")
  vs <- names(yx)
  if(ncol(yx) == 3){
    fg <- ggplot2::facet_wrap(ggplot2::vars(.data[[vs[3]]]))
  } else if(ncol(yx) == 4){
    fg <- ggplot2::facet_grid(rows = ggplot2::vars(.data[[vs[3]]]),
                              cols = ggplot2::vars(.data[[vs[4]]]))
  } else fg <- ggplot2::ylab(vs[2])
  if(segments){
    seg <- ggplot2::geom_segment(ggplot2::aes(yend = .data[[vs[2]]]),
                                 xend = 0)
  } else seg <- ggplot2::geom_segment(ggplot2::aes(xend = .data[[vs[1]]],
                                                   yend = .data[[vs[2]]]))
  ggplot2::ggplot(yx, ggplot2::aes(x = .data[[vs[1]]],
                                   y=.data[[vs[2]]])) +
    ggplot2::geom_point() +
    seg +
    fg +
    ggplot2::xlab(vs[1])
}

