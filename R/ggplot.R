#' Colour facets of ggplot
#'
#' @param pl plot
#' @param pal palette
#' @param side top or side?
#' @examples
#' library(ggplot2)
#' # facet on side:
#' pl <- ggplot(aes(x=Sepal.Length, y=Sepal.Width), data = iris) +
#'   geom_point()+
#'   facet_grid(Species~.)
#'plot(mat_col_facet(pl, pal = c("red", "blue", "pink"), side = "side"))
#'
#'## facet on top:
#'pl2 <- pl +
#'  facet_grid(. ~Species)
#'  plot(mat_col_facet(pl2, pal = c("red", "blue", "pink"), side = "top"))
#' @export
mat_col_facet <-  function(pl, pal = c("#FFD400", "#267000"), side = c("top", "side")) {

  strip_nam <-  switch(match.arg(side),
                       "top" = "strip-t|strip-b",
                       "side" = "strip-r|strip-l")

  g <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(pl))

  strips <- which(grepl(strip_nam, g$layout$name))
  if(length(strips) ==0) stop("Strip not found... change side?")
  if(length(strips) != length(pal)) warning("Not same length?")
  for (i in seq_along(strips)) {
    k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    # l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- pal[i]
    # g$grobs[[strips[i]]]$grobs[[1]]$children[[l]]$children[[1]]$gp$col <- pal[i + 1]
  }
  print("Use 'plot' on me")
  g
}


#' geom_lm: enforce lm without se
#'
#' @param mapping,data,stat,position Unchanged
#' @param formula,na.rm,show.legend,inherit.aes Unchanged
#' @param method,se Arguments changed
#' @param ... As in geom_smooth
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   mat_geom_lm()
#' @export
mat_geom_lm <-  function(mapping = NULL, data = NULL, stat = "smooth", position = "identity",
                         ..., method = "lm", formula = y ~ x, se = FALSE, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE) {
  ggplot2::geom_smooth(mapping = mapping, data = data, stat = stat, position = position,
                       ..., method = method, formula = formula, se = se, na.rm = na.rm,
                       show.legend = show.legend, inherit.aes = inherit.aes)
}




#' ggplot2 helpers
#'
#' Gives  a bunch of different helpers
#' @name gg_helpers
#' @aliases mat_gg_helpers
NULL



#' @rdname gg_helpers
#' @export
mat_gg_abline_01 <-  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2)

#' @rdname gg_helpers
#' @export
mat_gg_hline_0 <-  ggplot2::geom_hline(yintercept = 0, linetype = 2)

#' @rdname gg_helpers
#' @export
mat_gg_vline_0 <-  ggplot2::geom_vline(xintercept = 0, linetype = 2)

#' @rdname gg_helpers
#' @export
mat_gg_legend_bottom <- ggplot2::theme(legend.position="bottom")

#' @rdname gg_helpers
#' @export
mat_gg_legend_none <- ggplot2::theme(legend.position="none")

#' @rdname gg_helpers
#' @export
mat_gg_title_center <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                      plot.subtitle = ggplot2::element_text(hjust = 0.5))

#' @rdname gg_helpers
#' @export
mat_gg_scale_Y_toperc <- ggplot2::scale_y_continuous(labels = scales::percent)

#' @rdname gg_helpers
#' @export
mat_gg_scale_X_toperc <- ggplot2::scale_x_continuous(labels = scales::percent)

#' @rdname gg_helpers
#' @export
mat_gg_scale_Y_isperc <- ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

#' @rdname gg_helpers
#' @export
mat_gg_scale_X_isperc <- ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

gg_th_axisY_thousand  <- ggplot2::scale_y_continuous(labels=function(x) format(x, big.mark = "'", scientific = FALSE))
gg_th_axisX_thousand  <- ggplot2::scale_x_continuous(labels=function(x) format(x, big.mark = "'", scientific = FALSE))


