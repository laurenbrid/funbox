#' Dot plot and violin plot
#'
#' Using ggplot2, create a Wilkinson dot plot overlaying a violin plot with the median indicated.
#'
#' The Wilkinson dot plot shows each observation as a dot. Each observation is placed in a bin and stacked on one another. The violin plot provides a distribution density estimate. This function basically implements \href{https://hbiostat.org/bbr/descript#fig-descript-vplot}{Fig 4.26} from Frank Harrell's \href{https://hbiostat.org/bbr/}{Biostatistics for Biomedical Research book}, which was created with the following code:
#'
#'```r
#' library(Hmisc)
#' getHdata(FEV); set.seed(13)
#' FEV <- subset(FEV, runif(nrow(FEV)) < 1/8)   # 1/8 sample
#' require(ggplot2)
#' ggplot(FEV, aes(x=sex, y=fev)) +
#'   geom_violin(width=.6, col='lightblue') +
#'   geom_dotplot(binaxis='y', stackdir='center', position='dodge', alpha=.4) +
#'   stat_summary(fun.y=median, geom='point', color='blue', shape='+', size=12) +
#'   facet_grid(~ smoke) + xlab('') +
#'   ylab(expression(FEV[1])) + coord_flip()
#' ```
#'
#' Harrell recommends this plot replace "dynamite" plots (i.e., bar plots with error bars).
#'
#' Wilkinson dot plots are ideal for small to moderate sized data sets (n < 200). For n larger than about 200, the binwidth or dotsize arguments should be adjusted. The dot size is automatically adjusted relative to the binwidth. To adjust dot size, it probably makes sense to do it via the binwidth argument. See examples.
#'
#'
#'
#' @param formula a model formula. Two types supported: (1) y ~ x where y is numeric and x is a grouping variable. (2) y ~ x | z where y is numeric, x and z are grouping variables, and z is used as a faceting variable. If x or z are not a factor, the function converts them to a factor.
#' @param data data frame within which to evaluate the formula.
#' @param dotsize The diameter of the dots \emph{relative} to binwidth, default 1.
#' @param binwidth specifies maximum bin width. Defaults to 1/30 of the range of the data (i.e., if y is the data, then binwidth = diff(range(y))/30)
#' @returns Returns a Wilkinson dot chart overlaying a violin plot with the median indicated.
#' @export
#'
#' @references Wilkinson, L. (1999) Dot plots. The American Statistician, 53(3), 276-281.
#'
#' Hintze, J. L., Nelson, R. D. (1998) Violin Plots: A Box Plot-Density Trace Synergism. The American Statistician 52, 181-184.
#'
#' @examples
#' vdot_plot(mpg ~ am, mtcars)
#' vdot_plot(mpg ~ am | vs, mtcars)
#'
#' # example of adjusting dotsize via binwidth
#' # simulate 500 observations
#' n <- 500
#' g <- sample(c("a", "b", "c"), size = n, replace = TRUE)
#' y <- rnorm(n = n, mean = (g=="a")*5 + (g=="b")*7 + (g=="c")*8, sd = 1.2)
#'
#' # dot sizes are too big; binwidth is too large
#' vdot_plot(y ~ g)
#'
#' # default binwidth = diff(range(y))/30
#' # adjust binwidth to be smaller (1/60 of the range of the data)
#' vdot_plot(y ~ g, binwidth = diff(range(y))/60)
#'
vdot_plot <- function(formula, data,
                      dotsize = 1,
                      binwidth = NULL){
  if (!inherits(formula, "formula") | length(formula) != 3)
    stop("invalid formula")
  formula <- as.character(c(formula))
  formula <- stats::as.formula(sub("\\|", "+", formula))
  yx <- if (missing(data))
    stats::model.frame(formula)
  else stats::model.frame(formula, data = data)
  if(ncol(yx) > 3 | ncol(yx) < 2)
    stop("Invalid formula. Acceptable formulas are either y ~ x or y ~ x | z.")
  vs <- names(yx)
  if(any(!sapply(yx[,-1], is.factor)))
    yx[,-1] <- lapply(yx[,-1, drop = FALSE], as.factor)
  if(ncol(yx) == 3){
    fg <- ggplot2::facet_wrap(ggplot2::vars(.data[[vs[3]]]),
                              labeller = "label_both")
  } else fg <- ggplot2::ylab(vs[1])
  ggplot2::ggplot(yx, ggplot2::aes(x = .data[[vs[2]]],
                                   y = .data[[vs[1]]])) +
    ggplot2::geom_violin(width=.6, col='lightblue') +
    ggplot2::geom_dotplot(binaxis='y', stackdir='center',
                          position='dodge', alpha=.4,
                          dotsize = dotsize,
                          binwidth = binwidth) +
    ggplot2::stat_summary(fun=stats::median, geom='point', color='blue',
                          shape='+', size=10) +
    fg +
    ggplot2::coord_flip()
}
