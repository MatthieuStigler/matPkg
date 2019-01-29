
#' @export
mat_col_facet <-  function(pl, pal = c("#FFD400", "#267000"), side = c("top", "side")) {

  strip_nam <-  switch(match.arg(side),
                       "top" = "strip-t",
                       "side" = "strip-r")

  g <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(pl))

  strips <- which(grepl(strip_nam, g$layout$name))
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


gg_th_title <- ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                              plot.subtitle = ggplot2::element_text(hjust = 0.5))
gg_th_legend <- ggplot2::theme(legend.position="bottom")
gg_th_legend_bottom <- ggplot2::theme(legend.position="bottom")
gg_th_legend_none <- ggplot2::theme(legend.position="none")

gg_th_scale_perc <- ggplot2::scale_x_continuous(labels = scales::percent)
gg_th_scale_isperc <- ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

gg_th_scale_Y_isperc <- ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))
gg_th_scale_X_isperc <- ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"))

gg_th_axisY_thousand  <- ggplot2::scale_y_continuous(labels=function(x) format(x, big.mark = "'", scientific = FALSE))
gg_th_axisX_thousand  <- ggplot2::scale_x_continuous(labels=function(x) format(x, big.mark = "'", scientific = FALSE))

gg_add_abline_01 <-  ggplot2::geom_abline(slope = 1, intercept = 0)
