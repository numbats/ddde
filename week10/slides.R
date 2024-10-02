## ----include = FALSE, echo=FALSE-------------------------------------------------
source("../setup.R")


## ----nasa------------------------------------------------------------------------
#| label: nasa
#| echo: false
data(nasa)
glimpse(nasa)


## --------------------------------------------------------------------------------
#| code-fold: true
nasa_cb <- as_cubble(as_tibble(nasa), 
                     key=id, 
                     index=time, 
                     coords=c(long, lat))
nasa_cb


## --------------------------------------------------------------------------------
#| label: spatial
#| fig-width: 6
#| fig-height: 6
#| out-width: 90%
#| code-fold: true
ggplot() + 
  geom_point(data=nasa_cb, aes(x=long, y=lat)) +
  geom_point(data=dplyr::filter(nasa_cb, 
       id == "5-20"),
       aes(x=long, y=lat),
       colour="orange", size=4) +
  geom_point(data=dplyr::filter(nasa_cb, 
       id == "20-2"),
       aes(x=long, y=lat),
       colour="turquoise", size=4)


## --------------------------------------------------------------------------------
#| label: temporal
#| fig-width: 8
#| fig-height: 4
#| out-width: 90%
#| code-fold: true
nasa_cb_f <- nasa_cb |> 
  face_temporal() 
ggplot(nasa_cb_f) + 
  geom_line(aes(x=date, 
                 y=surftemp, 
                 group=id), alpha=0.2) +
  geom_line(data=filter(nasa_cb_f , 
       id=="5-20"),
       aes(x=date, 
                 y=surftemp, 
                 group=id),
       colour="orange", linewidth=2) +
  geom_line(data=filter(nasa_cb_f , 
       id=="20-2"),
       aes(x=date, 
                 y=surftemp, 
                 group=id),
       colour="turquoise", linewidth=2) +
  theme(aspect.ratio = 0.5)


## --------------------------------------------------------------------------------
#| label: raster
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
#| echo: false
# Get the map
sth_america <- map_data("world") |>
  filter(between(long, -115, -53), between(lat, -20.5, 41))

nasa_cb |> 
  face_temporal() |>
  filter(month == "Jan", year == 1995) |>
  select(id, time, surftemp) |>
  unfold(long, lat) |>
  ggplot() + 
  geom_tile(aes(x=long, y=lat, fill=surftemp)) +
  geom_path(data=sth_america, 
            aes(x=long, y=lat, group=group), 
            colour="white", linewidth=1) +
  scale_fill_viridis_c("", option = "magma") +
  ggtitle("January 1995") +
  theme_map() +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 24)) 


## --------------------------------------------------------------------------------
#| label: raster
#| eval: false
#| echo: true
## # Get the map
## sth_america <- map_data("world") |>
##   filter(between(long, -115, -53), between(lat, -20.5, 41))
## 
## nasa_cb |>
##   face_temporal() |>
##   filter(month == "Jan", year == 1995) |>
##   select(id, time, surftemp) |>
##   unfold(long, lat) |>
##   ggplot() +
##   geom_tile(aes(x=long, y=lat, fill=surftemp)) +
##   geom_path(data=sth_america,
##             aes(x=long, y=lat, group=group),
##             colour="white", linewidth=1) +
##   scale_fill_viridis_c("", option = "magma") +
##   ggtitle("January 1995") +
##   theme_map() +
##   theme(legend.position = "bottom",
##         plot.title = element_text(size = 24))


## --------------------------------------------------------------------------------
#| label: space-time
#| fig-width: 10
#| fig-height: 6
#| out-width: 75%
#| echo: false
nasa_cb |> face_temporal() |>
  select(id, time, month, year, surftemp) |>
  unfold(long, lat) |>
  ggplot() + 
  geom_tile(aes(x=long, y=lat, fill=surftemp)) +
  facet_grid(year~month) +
  scale_fill_viridis_c("", option = "magma") +
  theme_map() +
  theme(legend.position = "bottom") 


## --------------------------------------------------------------------------------
#| label: space-time
#| eval: false
#| echo: true
## nasa_cb |> face_temporal() |>
##   select(id, time, month, year, surftemp) |>
##   unfold(long, lat) |>
##   ggplot() +
##   geom_tile(aes(x=long, y=lat, fill=surftemp)) +
##   facet_grid(year~month) +
##   scale_fill_viridis_c("", option = "magma") +
##   theme_map() +
##   theme(legend.position = "bottom")


## --------------------------------------------------------------------------------
#| label: time-space1
#| fig-width: 8
#| fig-height: 8
#| out-width: 90%
#| code-fold: true
nasa_cb |> face_temporal() |>
  select(id, time, month, year, surftemp) |>
  unfold(long, lat) |>
  ggplot() +
    geom_polygon(data=sth_america, 
            aes(x=long, y=lat, group=group), 
            fill="#014221", alpha=0.2, colour="#ffffff") +
    cubble::geom_glyph_box(data=nasa, 
                           aes(x_major = long, 
                               x_minor = date,
                               y_major = lat, 
                               y_minor = surftemp), fill=NA) +
    cubble::geom_glyph(data=nasa, 
                       aes(x_major = long, 
                           x_minor = date,
                           y_major = lat, 
                           y_minor = surftemp)) +
    theme_map() 



## --------------------------------------------------------------------------------
#| label: time-space2
#| fig-width: 8
#| fig-height: 8
#| out-width: 90%
#| code-fold: true
nasa_cb |> face_temporal() |>
  select(id, time, month, year, surftemp) |>
  unfold(long, lat) |>
  ggplot() +
    geom_polygon(data=sth_america, 
            aes(x=long, y=lat, group=group), 
            fill="#014221", alpha=0.2, colour="#ffffff") +
    cubble::geom_glyph_box(data=nasa, 
                           aes(x_major = long, 
                               x_minor = date,
                               y_major = lat, 
                               y_minor = surftemp), fill=NA) +
    cubble::geom_glyph(data=nasa, 
                       aes(x_major = long, 
                           x_minor = date,
                           y_major = lat, 
                           y_minor = surftemp), 
                       global_rescale = FALSE) +
    theme_map() 


## --------------------------------------------------------------------------------
#| label: time-space3
#| fig-width: 8
#| fig-height: 8
#| out-width: 90%
#| code-fold: true
nasa_cb |> face_temporal() |>
  select(id, time, month, year, surftemp) |>
  unfold(long, lat) |>
  ggplot() +
    geom_polygon(data=sth_america, 
            aes(x=long, y=lat, group=group), 
            fill="#014221", alpha=0.2, colour="#ffffff") +
    cubble::geom_glyph(data=nasa, 
                       aes(x_major = long, 
                           x_minor = date,
                           y_major = lat, 
                           y_minor = surftemp), 
                       global_rescale = FALSE,
                       polar = TRUE) +
    theme_map() 


## --------------------------------------------------------------------------------
#| label: time-space4
#| fig-width: 6
#| fig-height: 6
#| out-width: 90%
#| code-fold: true
nasa_mth <- nasa_cb |> 
  face_temporal() |>
  select(id, time, month, year, surftemp) |>
  unfold(long, lat) |>
  as_tibble() |>
  group_by(id, month) |>
  dplyr::summarise(tmin = min(surftemp),
            tmax = max(surftemp), 
            long = min(long),
            lat = min(lat)) |>
  ungroup() |>
  mutate(month = as.numeric(month))
ggplot() +
    geom_polygon(data=sth_america, 
            aes(x=long, y=lat, group=group), 
            fill="#014221", alpha=0.2, colour="#ffffff") +
    geom_glyph_ribbon(data = nasa_mth, 
                      aes(x_major = long, 
                          x_minor = month,
                          y_major = lat, 
                          ymin_minor = tmin,
                          ymax_minor = tmax), 
                          width = 2) +
    theme_map() 


## --------------------------------------------------------------------------------
#| eval: false
#| code-fold: true
#| code-summary: DEMO
## library(tsibble)
## library(tsibbletalk)
## library(lubridate)
## library(plotly)
## nasa_shared <- nasa |>
##   mutate(date = ymd(date)) |>
##   select(long, lat, date, surftemp, id) |>
##   as_tsibble(index=date, key=id) |>
##   as_shared_tsibble()
## p1 <- ggplot() +
##   geom_polygon(data=sth_america,
##             aes(x=long, y=lat, group=group),
##             colour="#ffffff", alpha=0.2, fill="#014221") +
##   geom_point(data=nasa_shared, aes(x = long,
##          y = lat, group = id))
## p2 <- nasa_shared |>
##   ggplot(aes(x = date, y = surftemp)) +
##   geom_line(aes(group = id), alpha = 0.5)
## subplot(
##     ggplotly(p1, tooltip = "Region"),
##     ggplotly(p2, tooltip = "Region"),
##     nrows = 1, widths=c(0.3, 0.7)) |>
##   highlight(dynamic = TRUE)


## --------------------------------------------------------------------------------
#| code-fold: true
#| eval: false
#| echo: false
#| results: hide
## library(gstat)
## nasa_jan95 <- nasa |>
##   filter(year == 1995, month == "Jan") |>
##   select(id, long, lat, surftemp, cloudlow, cloudmid, cloudhigh, ozone)
## row.names(nasa_jan95) <- nasa_jan95[,1]
## nasa_jan95_sf <- SpatialPointsDataFrame(nasa_jan95[,2:3],
##                    nasa_jan95[,4:8])
## g <- gstat(formula = surftemp~1,
##            data=nasa_jan95_sf)
## plot(variogram(g))
## vgm1 <- variogram(surftemp~1, nasa_jan95_sf, cloud=TRUE)
## plot(vgm1)
## # Set up model
## vgm_mod <- vgm(psill=10, model = "Sph", range=20, nmax=60)
## g_dummy <- gstat(formula = surftemp~1, dummy=TRUE, beta=295,
##            data=nasa_jan95_sf, model=vgm_mod)
## g_null <- predict(g_dummy, nasa_jan95_sf, nsim=11)
## g_null_df1 <- tibble(long = g_null@coords[,1],
##                      lat = g_null@coords[,2],
##                      surftemp1 = g_null@data$sim1,
##                      surftemp2 = g_null@data$sim2,
##                      surftemp3 = g_null@data$sim3)
## p_data <- nasa_cb |>
##   face_temporal() |>
##   filter(month == "Jan", year == 1995) |>
##   select(id, time, surftemp) |>
##   unfold(long, lat) |>
##   ggplot() +
##   geom_tile(aes(x=long, y=lat, fill=surftemp)) +
##   geom_path(data=sth_america,
##             aes(x=long, y=lat, group=group),
##             colour="white", linewidth=1) +
##   scale_fill_viridis_c("", option = "magma") +
##   #ggtitle("January 1995") +
##   theme_map() +
##   theme(legend.position = "bottom",
##         plot.title = element_text(size = 24))
## p_null1 <- ggplot() +
##   geom_tile(data=g_null_df1,
##             aes(x=long, y=lat, fill=surftemp1)) +
##   geom_path(data=sth_america,
##             aes(x=long, y=lat, group=group),
##             colour="white", linewidth=1) +
##   scale_fill_viridis_c("", option = "magma") +
##   #ggtitle("January 1995") +
##   theme_map() +
##   theme(legend.position = "bottom",
##         plot.title = element_text(size = 24))
## p_null2 <- ggplot() +
##   geom_tile(data=g_null_df1,
##             aes(x=long, y=lat, fill=surftemp2)) +
##   geom_path(data=sth_america,
##             aes(x=long, y=lat, group=group),
##             colour="white", linewidth=1) +
##   scale_fill_viridis_c("", option = "magma") +
##   #ggtitle("January 1995") +
##   theme_map() +
##   theme(legend.position = "bottom",
##         plot.title = element_text(size = 24))
## p_null3 <- ggplot() +
##   geom_tile(data=g_null_df1,
##             aes(x=long, y=lat, fill=surftemp3)) +
##   geom_path(data=sth_america,
##             aes(x=long, y=lat, group=group),
##             colour="white", linewidth=1) +
##   scale_fill_viridis_c("", option = "magma") +
##   #ggtitle("January 1995") +
##   theme_map() +
##   theme(legend.position = "bottom",
##         plot.title = element_text(size = 24))
## p_data + p_null1 + p_null2 + p_null3 + plot_layout(ncol=2)


## ----toy-spatial, out.width = "80%"----------------------------------------------
#| code-fold: true
#| code-summary: generate-data
#| results: hide
# Set up a simple example
set.seed(945)
x <- 1:24
y <- 1:24
xy <- expand.grid(x, y)
d <- tibble(x=xy$Var1, y=xy$Var2) |>
  mutate(v = x+2*y) 
d_sf <- SpatialPointsDataFrame(d[,1:2],
                   data.frame(d[,3]))
vgm_mod <- vgm(psill=5, model = "Sph", range=20, nmax=30)
d_dummy <- gstat(formula = v~1, dummy=TRUE, beta=0,
           model=vgm_mod)
d_err <- predict(d_dummy, d_sf, nsim=1)
d <- d |>
  mutate(e = d_err@data$sim1*3) |>
  mutate(ve = v+e)


## --------------------------------------------------------------------------------
#| label: plot-simple example
#| code-fold: true
#| code-summary: plot
#| fig-width: 12
#| fig-height: 4
#| out-width: 100%
obs <- ggplot(d, aes(x, y, fill = ve)) +
  geom_tile() +
  scale_fill_viridis_c("") +
  theme(aspect.ratio = 1) +
  ggtitle("Observed") +
  theme(legend.position = "none",
              axis.text = element_blank(),
              axis.title = element_blank())
trend <- ggplot(d, aes(x, y, fill = v)) +
  geom_tile() +
  scale_fill_viridis_c("", option = "magma") +
  theme(aspect.ratio = 1) +
  ggtitle("Trend") +
  theme(legend.position = "none",
              axis.text = element_blank(),
              axis.title = element_blank())
err <- ggplot(d, aes(x, y, fill = e)) +
  geom_tile() +
  scale_fill_distiller("", palette = "PRGn") +
  theme(aspect.ratio = 1) +
  ggtitle("Residual") +
  theme(legend.position = "none",
              axis.text = element_blank(),
              axis.title = element_blank())
obs + trend + err + plot_layout(ncol=3)


## --------------------------------------------------------------------------------
#| label: gen-nulls
#| code-fold: true
#| code-summary: generate-nulls
#| results: hide
#| fig-width: 9
#| fig-height: 6
#| out-width: 100%
set.seed(953)
d_null <- predict(d_dummy, d_sf, nsim=5)
pos <- sample(1:6, 1)
lineup_plots <- list()
j <- 1
for (i in 1:6) {
  if (pos == i) { # plot data
    p <- ggplot(d, aes(x, y, fill = scale(ve))) +
           geom_tile() 
  } 
  else { # plot nulls
    null_df <- tibble(x=d$x, y=d$y, v=d_null@data[,j])
    p <- ggplot(null_df, aes(x, y, fill = scale(v))) +
           geom_tile() 
   j <- j + 1
  }
  p <- p +
        scale_fill_viridis_c("", option = "magma") +
        theme(legend.position = "none",
              axis.text = element_blank(),
              axis.title = element_blank())
    
  lineup_plots[[paste(i)]] <- p
}
wrap_plots(lineup_plots, ncol = 3)


## --------------------------------------------------------------------------------
#| label: plot-margins
#| code-fold: true
#| code-summary: plot
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
long <- ggplot(d, aes(x, y=ve)) +
  geom_point() +
  geom_smooth(se=F) +
  ylab("obs") +
  theme(aspect.ratio = 1)
lat <- ggplot(d, aes(y, y=ve)) +
  geom_point() +
  ylab("obs") +
  geom_smooth(se=F) +
  theme(aspect.ratio = 1)
long + lat + plot_layout(ncol=2)


## --------------------------------------------------------------------------------
#| echo: false
fabric_drawing(cid = "twoway", 
               cwidth = 700, 
               cheight = 700, 
               cfill = "whitesmoke", 
               drawingWidth = 3, 
               gumSize = 10)
fabric_text_add(cid = "twoway", textId = "txt1",
                text = " 1  2  3",
                left = 10, top = 10, 
                fontFamily = "Courier", fontSize = 18)
fabric_text_add(cid = "twoway", textId = "txt2",
                text = " 4  5  6",
                left = 10, top = 40, 
                fontFamily = "Courier", fontSize = 18)
fabric_text_add(cid = "twoway", textId = "txt3",
                text = " 7  8  9",
                left = 10, top = 70, 
                fontFamily = "Courier", fontSize = 18)


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
library(tukeyedar)
d_pol <- eda_pol(d, row = x, col = y, val = ve, plot=FALSE)
long <- ggplot(d, aes(x, y=ve)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_point(data = d_pol$row, aes(x=x, 
                       y=effect + d_pol$global), 
    colour = "#D93F00", size=3) +
  ylab("obs") +
  theme(aspect.ratio = 1)
lat <- ggplot(d, aes(y, y=ve)) +
  geom_point() +
  ylab("obs") +
  geom_smooth(se=F) +
  geom_point(data = d_pol$col, aes(x=y, 
                      y=effect + d_pol$global), 
    colour = "#D93F00", size=3) +
  theme(aspect.ratio = 1)
long + lat + plot_layout(ncol=2)


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
pol_res <- ggplot(d_pol$long, aes(x, y, fill = ve)) +
  geom_tile() +
  scale_fill_distiller("", palette = "PRGn") +
  theme(aspect.ratio = 1) +
  ggtitle("Polish Residuals") +
  theme(legend.position = "none",
              axis.text = element_blank(),
              axis.title = element_blank())
err <- err +
  theme(legend.position = "none",
              axis.text = element_blank(),
              axis.title = element_blank()) 
err + pol_res + plot_layout(ncol=2)


## --------------------------------------------------------------------------------
#| echo: false
world_map <- map_data("world")
world_map |> 
  filter(region %in% c("Australia", "New Zealand")) |> 
      DT::datatable(width=1150, height=100)


## --------------------------------------------------------------------------------
#| label: mappolygon
#| code-fold: true
#| fig-width: 12
#| fig-height: 4
#| out-width: 100%
oz <- world_map |> 
  filter(region == "Australia") |>
  filter(lat > -50)
m1 <- ggplot(oz, aes(x = long, y = lat)) + 
  geom_point(size=0.2) + #<<
  coord_map() +
  ggtitle("Points")
m2 <- ggplot(oz, aes(x = long, y = lat, 
               group = group)) + #<<
  geom_path() + #<<
  coord_map() +
  ggtitle("Path")
m3 <- ggplot(oz, aes(x = long, y = lat, 
               group = group)) + #<<
  geom_polygon(fill = "#607848", colour = "#184848") + #<<
  coord_map() +
  ggtitle("Filled polygon")
m1 + m2 + m3


## --------------------------------------------------------------------------------
#| label: sfobject
#| echo: false
#| results: hide
library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
nc |> slice_head(n=5) 


## --------------------------------------------------------------------------------
#| label: setup-choro
#| echo: false
library(sf)
library(sugarbag)

invthm <- theme_map() + 
  theme(
    panel.background = element_rect(fill = "black", colour = NA), 
    plot.background = element_rect(fill = "black", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    text = element_text(colour = "white"),
    axis.text = element_blank()
  )

# function to allocate colours to regions
aus_colours <- function(sir_p50){
  value <- case_when(
    sir_p50 <  0.74 ~ "#33809d",
    sir_p50 >= 0.74 & sir_p50 < 0.98 ~ "#aec6c7",
    sir_p50 >= 0.98 & sir_p50 < 1.05 ~ "#fff4bc",
    sir_p50 >= 1.05 & sir_p50 < 1.45 ~ "#ff9a64",
    sir_p50 >= 1.45 ~ "#ff3500",
    TRUE ~ "#FFFFFF")
  return(value)
}


## --------------------------------------------------------------------------------
#| label: thyroiddata
#| code-fold: true
#| eval: false
## sa2 <- strayr::read_absmap("sa22011") |>
##   filter(!st_is_empty(geometry)) |>
##   filter(!state_name_2011 == "Other Territories") |>
##   filter(!sa2_name_2011 == "Lord Howe Island")
## sa2 <- sa2 |> rmapshaper::ms_simplify(keep = 0.5, keep_shapes = TRUE) # Simplify the map!!!
## SIR <- read_csv(here::here("data/SIR Downloadable Data.csv")) |>
##   filter(SA2_name %in% sa2$sa2_name_2011) |>
##   dplyr::select(Cancer_name, SA2_name, Sex_name, p50) |>
##   filter(Cancer_name == "Thyroid", Sex_name == "Females")
## ERP <- read_csv(here::here("data/ERP.csv")) |>
##   filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) |>
##   dplyr::select(Region, Value)
## # Alternative maps
## # Join with sa2 sf object
## sa2thyroid_ERP <- SIR |>
##   left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) |>
##   left_join(., ERP |>
##               dplyr::select(Region,
##               Population = Value), by = c("sa2_name_2011"= "Region")) |>
##   filter(!st_is_empty(geometry))
## sa2thyroid_ERP <- sa2thyroid_ERP |>
##   #filter(!is.na(Population)) |>
##   filter(!sa2_name_2011 == "Lord Howe Island") |>
##   mutate(SIR = map_chr(p50, aus_colours)) |>
##   st_as_sf()
## save(sa2, file="data/sa2.rda")
## save(sa2thyroid_ERP, file="data/sa2thyroid_ERP.rda")


## --------------------------------------------------------------------------------
#| label: choro
#| code-fold: true
#| fig-width: 10
#| fig-height: 8
#| out-width: 100%
# Plot the choropleth
load("../data/sa2thyroid_ERP.rda")
aus_ggchoro <- ggplot(sa2thyroid_ERP) + 
  geom_sf(aes(fill = SIR), size = 0.1) + 
  scale_fill_identity() + invthm
aus_ggchoro


## --------------------------------------------------------------------------------
#| label: cartogram
#| code-fold: true
#| fig-width: 6
#| fig-height: 10
#| out-width: 60%
# transform to NAD83 / UTM zone 16N
nc <- nc |>
  mutate(lBIR79 = log(BIR79))
nc_utm <- st_transform(nc, 26916)

orig <- ggplot(nc) + 
  geom_sf(aes(fill = lBIR79)) +
  ggtitle("original") +
  theme_map() +
  theme(legend.position = "none")

nc_utm_carto <- cartogram_cont(nc_utm, weight = "BIR74", itermax = 5)

carto <- ggplot(nc_utm_carto) + 
  geom_sf(aes(fill = lBIR79)) +
  ggtitle("cartogram") +
  theme_map() +
  theme(legend.position = "none")

nc_utm_dorl <- cartogram_dorling(nc_utm, weight = "BIR74")

dorl <- ggplot(nc_utm_dorl) + 
  geom_sf(aes(fill = lBIR79)) +
  ggtitle("dorling") +
  theme_map() +
  theme(legend.position = "none")

orig + carto + dorl + plot_layout(ncol=1)


## --------------------------------------------------------------------------------
#| label: hexmap
#| code-fold: true
#| fig-width: 10
#| fig-height: 8
#| out-width: 100%
if (!file.exists(here::here("data/aus_hexmap.rda"))) {
  
## Create centroids set
centroids <- sa2 |> 
  create_centroids(., "sa2_name_2011")
## Create hexagon grid
grid <- create_grid(centroids = centroids,
                    hex_size = 0.2,
                    buffer_dist = 5)
## Allocate polygon centroids to hexagon grid points
aus_hexmap <- allocate(
  centroids = centroids,
  hex_grid = grid,
  sf_id = "sa2_name_2011",
  ## same column used in create_centroids
  hex_size = 0.2,
  ## same size used in create_grid
  hex_filter = 10,
  focal_points = capital_cities,
  width = 35,
  verbose = FALSE
)
save(aus_hexmap, 
     file = here::here("data/aus_hexmap.rda")) 
}

load(here::here("data/aus_hexmap.rda"))
## Prepare to plot
fort_hex <- fortify_hexagon(data = aus_hexmap,
                            sf_id = "sa2_name_2011",
                            hex_size = 0.2) |> 
            left_join(sa2thyroid_ERP |> select(sa2_name_2011, SIR, p50))
## Make a plot
aus_hexmap_plot <- ggplot() +
  geom_sf(data=sa2thyroid_ERP, fill=NA, colour="grey60", size=0.1) +
  geom_polygon(data = fort_hex, aes(x = long, y = lat, group = hex_id, fill = SIR)) +
  scale_fill_identity() +
  invthm 
aus_hexmap_plot  

