# Shared setup for ETC5521 Week 10

current_file <- knitr::current_input()
basename <- gsub(".[Rq]md$", "", current_file)

knitr::opts_chunk$set(
  fig.path = sprintf("images/%s/", basename),
  fig.width = 6,
  fig.height = 4,
  fig.align = "center",
  out.width = "100%",
  code.line.numbers = FALSE,
  fig.retina = 4,
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  cache = FALSE,
  dev.args = list(pointsize = 11)
)

options(
  digits = 2,
  width = 60,
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c(
    "#D55E00", "#0072B2", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#F0E442"
  ),
  ggplot2.discrete.fill = c(
    "#D55E00", "#0072B2", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#F0E442"
  )
)

if (requireNamespace("ggplot2", quietly = TRUE)) {

  ggplot2::theme_set(
    ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(
        aspect.ratio = 1,
        plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        plot.title.position = "plot",
        plot.title = ggplot2::element_text(size = 18),
        panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.key = ggplot2::element_rect(fill = "transparent", colour = NA)
      )
  )
}
