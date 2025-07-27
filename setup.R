# Load libraries used everywhere
library("tidyverse")
library("tidymodels")
library("colorspace")
library("patchwork")
library("gridExtra")
library("GGally")
library("geozoo")
library("mulgar")
library("naniar")
library("mvtnorm")
library("palmerpenguins")
library("tourr")
library("ggthemes")
library("plotly")
library("forcats")
library("ggbeeswarm")
library("gt")
library("MASS")
library("kableExtra")
library("fabricerin")
library("DiagrammeR")
library("gganimate")
library("datasauRus")
library("ggplot2movies")
library("ggExtra")
library("here")
library("nullabor")
library("htmltools")
library("crosstalk")
library("cassowaryr")
library("janitor")
library("ggpcp")
library("vcd")
library("UpSetR")
library("tsibble")
library("imputeTS")
library("sugrrants")
library("brolgar")
library("lme4")
library("modelr")
library("feasts")
library("tsibbletalk")
library("shiny")
library("cubble")
# devtools::install_github("maliny12/sugarglider")
library("sugarglider")
library("sp")
library("gstat")
library("sugarbag")
library("rmapshaper")
library("cartogram")
library("mgcv")
remotes::install_github("mgimond/tukeyedar")
library("tukeyedar")
remotes::install_github("runapp-aus/strayr")
library("strayr")
library("DAAG")
library("tsibbledata")
library("chron")
library("summarytools")
library("SmartEDA")
library("DataExplorer")
library("inspectdf")
library("ggplot2movies")
library("bayesm")
library("vcd")
library("bestNormalize")
library("boot")
library("visdat")
library("imputeTS")
library("readabs")
library("VGAM")
library("simputation")
library("validate")
library("datarium")
library("ggcleveland")
library("mixtools")
library("countrycode")
library("VGAMdata")
library("Sleuth2")
library("fastDummies")
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(palmerpenguins::penguins)
conflicts_prefer(tourr::flea)
conflicts_prefer(viridis::viridis_pal)
conflicts_prefer(geozoo::simplex)
conflicts_prefer(readr::col_factor)
conflicts_prefer(tsibble::pedestrian)
conflicts_prefer(nycflights13::flights)

# Locations
current_file <- knitr::current_input()
basename <- gsub(".[Rq]md$", "", current_file)

# Set up chunk for all slides
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
  ggplot2.discrete.colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)
theme_set(theme_bw(base_size = 14) +
   theme(
     aspect.ratio = 1,
     plot.background = element_rect(fill = 'transparent', colour = NA),
     plot.title.position = "plot",
     plot.title = element_text(size = 18),
     panel.background = element_rect(fill = 'transparent', colour = NA),
     legend.background = element_rect(fill = 'transparent', colour = NA),
     legend.key = element_rect(fill = 'transparent', colour = NA)
   )
)

