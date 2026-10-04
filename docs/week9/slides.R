## ---------------------------------------------------------
#| label: setup
#| include: false
#| echo: false
source("../setup.R")


## ---------------------------------------------------------
#| label: co2-data-prep
#| eval: false
#| echo: false
# CO2.ptb <- read.table("https://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/merged_in_situ_and_flask/monthly/monthly_merge_co2_ptb.csv", sep=",", skip=69)
# colnames(CO2.ptb) <- c("year", "month", "dateE", "date", "co2_ppm", "sa_co2", "fit", "sa_fit", "co2f", "sa_co2f")
# CO2.ptb$lat <- (-71.3)
# CO2.ptb$lon <- (-156.6)
# CO2.ptb$stn <- "ptb"
# CO2.ptb$co2_ppm <- replace_na(CO2.ptb$co2_ppm, -99.99)
# 
# save(CO2.ptb, file=here::here("data/CO2_ptb.rda"))


## ---------------------------------------------------------
#| label: CO2
#| echo: false
#| fig-width: 4
#| fig-height: 7
#| out-width: 50%
load(here::here("data/CO2_ptb.rda"))
CO2.ptb <- CO2.ptb |>
  filter(year > 2015) |>
  filter(co2_ppm > 100) # handle missing values
p1 <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) + 
  geom_line(size=2, colour="#D93F00") + xlab("") + ylab("CO2 (ppm)")
p2 <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) + 
  geom_smooth(se=FALSE, colour="#D93F00", size=2) + 
  xlab("") + ylab("CO2 (ppm)")
p1 + p2 + plot_layout(ncol=1)


## ---------------------------------------------------------
#| label: ped-reg
options(width=55)
pedestrian 


## ---------------------------------------------------------
#| label: nycflights
options(width=55)
library(nycflights13)
flights_ts <- flights |>
  mutate(dt = ymd_hm(paste(paste(year, month, day, sep="-"), 
                           paste(hour, minute, sep=":")))) |>
  as_tsibble(index = dt, key = c(origin, dest, carrier, tailnum), regular = FALSE)
flights_ts 


## ---------------------------------------------------------
#| label: dep-delay-month
flights_mth <- flights_ts |> 
  as_tibble() |>
  group_by(month, origin) |>
  summarise(dep_delay = mean(dep_delay, na.rm=TRUE)) |>
  as_tsibble(key=origin, index=month)
ggplot(flights_mth, aes(x=month, y=dep_delay, colour=origin)) +
  geom_point() +
  geom_smooth(se=F) +
  scale_x_continuous("", breaks = seq(1, 12, 1), 
                     labels=c("J","F","M","A","M","J",
                              "J","A","S","O","N","D")) +
  scale_y_continuous("av dep delay (mins)", limits=c(0, 25)) +
  theme(aspect.ratio = 0.5)


## ---------------------------------------------------------
#| label: arr-delay-month
flights_mth_arr <- flights_ts |> 
  as_tibble() |>
  group_by(month, origin) |>
  summarise(arr_delay = mean(arr_delay, na.rm=TRUE)) |>
  as_tsibble(key=origin, index=month)
ggplot(flights_mth_arr, aes(x=month, y=arr_delay, colour=origin)) +
  geom_point() +
  geom_smooth(se=F) +
  scale_x_continuous("", breaks = seq(1, 12, 1), 
                     labels=c("J","F","M","A","M","J",
                              "J","A","S","O","N","D")) +
  scale_y_continuous("av arr delay (mins)", limits=c(0, 25)) +
  theme(aspect.ratio = 0.5)


## ---------------------------------------------------------
#| label: weekday-delay
#| fig-height: 8
#| fig-width: 5
#| out-width: 48%
flights_wk <- flights_ts |> 
  as_tibble() |>
  mutate(wday = wday(dt, label=TRUE, week_start = 1)) |>
  group_by(wday, origin) |>
  summarise(dep_delay = mean(dep_delay, na.rm=TRUE)) |>
  mutate(weekend = ifelse(wday %in% c("Sat", "Sun"), "yes", "no")) |>
  as_tsibble(key=origin, index=wday)
ggplot(flights_wk, aes(x=wday, y=dep_delay, fill=weekend)) +
  geom_col() +
  facet_wrap(~origin, ncol=1, scales="free_y") +
  xlab("") +
  ylab("av dep delay (mins)") +
  theme(aspect.ratio = 0.5, legend.position = "none")


## ---------------------------------------------------------
#| label: airtime-check
flights_airtm <- flights |>
  mutate(dep_min = dep_time %% 100,
         dep_hr = dep_time %/% 100,
         arr_min = arr_time %% 100,
         arr_hr = arr_time %/% 100) |>
  mutate(dep_dt = ymd_hm(paste(paste(year, month, day, sep="-"), 
                           paste(dep_hr, dep_min, sep=":")))) |>
  mutate(arr_dt = ymd_hm(paste(paste(year, month, day, sep="-"), 
                           paste(arr_hr, arr_min, sep=":")))) |>
  mutate(air_time2 = as.numeric(difftime(arr_dt, dep_dt)))

fp <- flights_airtm |> 
  sample_n(3000) |>
  mutate(oridst = paste(origin, dest)) |>
  ggplot(aes(x=air_time, y=air_time2, label = oridst)) + 
    geom_abline(intercept=0, slope=1) +
    geom_point()
ggplotly(fp, width=500, height=500)


## ---------------------------------------------------------
#| label: missings-simple
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------------
#| label: missing-gaps
has_gaps(harvest, .full = TRUE) 


## ---------------------------------------------------------
#| label: missings-simple
#| echo: false
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------------
#| label: count-gaps
count_gaps(harvest,  .full=TRUE)


## ---------------------------------------------------------
#| label: missings-simple
#| echo: false
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------------
#| label: fill-gaps
harvest <- fill_gaps(harvest, 
                     .full=TRUE) 
harvest 


## ---------------------------------------------------------
#| label: missings-simple
#| echo: false
set.seed(328)
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 
           2012, 2013),
  fruit = rep(c("kiwi", "cherry"), 
              each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest


## ---------------------------------------------------------
#| label: impute-gaps
harvest_nomiss <- harvest |> 
  group_by(fruit) |> 
  mutate(kilo = 
    na_interpolation(kilo)) |> 
  ungroup()
harvest_nomiss 


## ----fig.width=12, fig.height=7, out.width="80%"----------
#| label: CO2-ratio
#| fig-width: 12
#| fig-height: 7
#| out-width: 70%
#| echo: false
load(here::here("data/CO2_ptb.rda"))
CO2.ptb <- CO2.ptb |> 
  filter(year > 1980) |>
  filter(co2_ppm > 100) # handle missing values
p <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) + 
  geom_line(size=1) + xlab("") + ylab("CO2 (ppm)")
p1 <- p + theme(aspect.ratio = 1) + ggtitle("1 to 1 (may be useless)")
p3 <- p + theme(aspect.ratio = 2) + ggtitle("tall & skinny:  trend")
p2 <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) + 
  annotate("text", x=2000, y=375, label="CO2 at \n Point Barrow,\n Alaska", size=8) + theme_solid()
p4 <- p + 
  scale_x_continuous("", breaks = seq(1980, 2020, 5)) + 
  theme(aspect.ratio = 0.2) + ggtitle("short & wide: seasonality")
grid.arrange(p1, p2, p3, p4, layout_matrix= matrix(c(1,2,3,4,4,4), nrow=2, byrow=T))


## ---------------------------------------------------------
#| label: CO2-ratio
#| echo: true
#| eval: false
# load(here::here("data/CO2_ptb.rda"))
# CO2.ptb <- CO2.ptb |>
#   filter(year > 1980) |>
#   filter(co2_ppm > 100) # handle missing values
# p <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) +
#   geom_line(size=1) + xlab("") + ylab("CO2 (ppm)")
# p1 <- p + theme(aspect.ratio = 1) + ggtitle("1 to 1 (may be useless)")
# p3 <- p + theme(aspect.ratio = 2) + ggtitle("tall & skinny:  trend")
# p2 <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) +
#   annotate("text", x=2000, y=375, label="CO2 at \n Point Barrow,\n Alaska", size=8) + theme_solid()
# p4 <- p +
#   scale_x_continuous("", breaks = seq(1980, 2020, 5)) +
#   theme(aspect.ratio = 0.2) + ggtitle("short & wide: seasonality")
# grid.arrange(p1, p2, p3, p4, layout_matrix= matrix(c(1,2,3,4,4,4), nrow=2, byrow=T))


## ---------------------------------------------------------
#| label: calendar
#| fig-width: 10
#| fig-height: 6
#| out-width: 70%
#| echo: false
flights_hourly <- flights |>
  group_by(time_hour, origin) |> 
  summarise(count = n(), 
    dep_delay = mean(dep_delay, 
                     na.rm = TRUE)) |> 
  ungroup() |>
  as_tsibble(index = time_hour, 
             key = origin) |>
    mutate(dep_delay = 
    na_interpolation(dep_delay)) 
calendar_df <- flights_hourly |> 
  filter(origin == "JFK") |>
  mutate(hour = hour(time_hour), 
         date = as.Date(time_hour)) |>
  filter(year(date) < 2014) |>
  frame_calendar(x=hour, y=count, date=date, nrow=2) 
p1 <- calendar_df |>
  ggplot(aes(x = .hour, y = .count, group = date)) +
  geom_line() + theme(axis.line.x = element_blank(),
                      axis.line.y = element_blank()) +
  theme(aspect.ratio=0.5)
prettify(p1, size = 3, label.padding = unit(0.15, "lines"))


## ---------------------------------------------------------
#| label: calendar
#| echo: true
#| eval: false
# flights_hourly <- flights |>
#   group_by(time_hour, origin) |>
#   summarise(count = n(),
#     dep_delay = mean(dep_delay,
#                      na.rm = TRUE)) |>
#   ungroup() |>
#   as_tsibble(index = time_hour,
#              key = origin) |>
#     mutate(dep_delay =
#     na_interpolation(dep_delay))
# calendar_df <- flights_hourly |>
#   filter(origin == "JFK") |>
#   mutate(hour = hour(time_hour),
#          date = as.Date(time_hour)) |>
#   filter(year(date) < 2014) |>
#   frame_calendar(x=hour, y=count, date=date, nrow=2)
# p1 <- calendar_df |>
#   ggplot(aes(x = .hour, y = .count, group = date)) +
#   geom_line() + theme(axis.line.x = element_blank(),
#                       axis.line.y = element_blank()) +
#   theme(aspect.ratio=0.5)
# prettify(p1, size = 3, label.padding = unit(0.15, "lines"))


## ---------------------------------------------------------
#| label: calendar-delay
#| fig-width: 10
#| fig-height: 6
#| out-width: 70%
#| echo: false
calendar_df <- flights_hourly |> 
  filter(origin == "JFK") |>
  mutate(hour = hour(time_hour), 
         date = as.Date(time_hour)) |>
  filter(year(date) < 2014) |>
  frame_calendar(x=hour, y=dep_delay, date=date, nrow=2) 
p1 <- calendar_df |>
  ggplot(aes(x = .hour, y = .dep_delay, group = date)) +
  geom_line() + theme(axis.line.x = element_blank(),
                      axis.line.y = element_blank()) +
  theme(aspect.ratio=0.5)
prettify(p1, size = 3, label.padding = unit(0.15, "lines"))


## ---------------------------------------------------------
#| label: calendar-delay
#| echo: true
#| eval: false
# calendar_df <- flights_hourly |>
#   filter(origin == "JFK") |>
#   mutate(hour = hour(time_hour),
#          date = as.Date(time_hour)) |>
#   filter(year(date) < 2014) |>
#   frame_calendar(x=hour, y=dep_delay, date=date, nrow=2)
# p1 <- calendar_df |>
#   ggplot(aes(x = .hour, y = .dep_delay, group = date)) +
#   geom_line() + theme(axis.line.x = element_blank(),
#                       axis.line.y = element_blank()) +
#   theme(aspect.ratio=0.5)
# prettify(p1, size = 3, label.padding = unit(0.15, "lines"))


## ---------------------------------------------------------
#| label: lm-lineup
#| fig-width: 10
#| fig-height: 5
#| out-width: 100%
p_bourke <- pedestrian |>
  as_tibble() |>
  filter(Sensor == "Bourke Street Mall (North)",
         Date >= ymd("2015-05-03"), Date <= ymd("2015-05-16")) |>
  mutate(date_num = 
    as.numeric(difftime(Date_Time,ymd_hms("2015-05-03 00:00:00"),
       units="hours"))+11) |> # UTC to AEST
  mutate(day = wday(Date, label=TRUE, week_start=1)) |>
  select(date_num, Time, day, Count) |>
  rename(time = date_num, hour=Time, count = Count)
# Fit a linear model with categorical hour variable
p_bourke_lm <- glm(count~day+factor(hour), family="poisson", 
  data=p_bourke)
# Function to simulate from a Poisson
simulate_poisson <- function(model, newdata) {
  lambda_pred <- predict(model, newdata, type = "response")
  rpois(length(lambda_pred), lambda = lambda_pred)
}

set.seed(436)
pos <- sample(1:12)
p_bourke_lineup <- bind_cols(.sample = rep(pos[1], 
  nrow(p_bourke)), p_bourke[,-2])
for (i in 1:11) {
  new <- simulate_poisson(p_bourke_lm, p_bourke)
  x <- tibble(time=p_bourke$time, count=new)
  x <- bind_cols(.sample = rep(pos[i+1], 
         nrow(p_bourke)), x)
  p_bourke_lineup <- bind_rows(p_bourke_lineup, x)
}

ggplot(p_bourke_lineup,
  aes(x=time, y=count)) + 
  geom_line() +
  facet_wrap(~.sample, ncol=4) +
  theme(aspect.ratio=0.5, 
        axis.text = element_blank(),
        axis.title = element_blank())


## ---------------------------------------------------------
#| echo: false
countdown::countdown(8, 06)


## ---------------------------------------------------------
#| label: NYC-lineup
#| fig-width: 14
#| fig-height: 3
#| out-width: 100%
set.seed(514)
ggplot(lineup(null_permute("origin"), true=flights_mth, n=14), 
       aes(x=month, y=dep_delay, colour=origin)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~.sample, ncol=7) +
  theme(aspect.ratio = 0.5, 
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())


## ---------------------------------------------------------
#| label: NYC-lineup-answer
#| fig-width: 14
#| fig-height: 3
#| out-width: 100%
# Correct code: permute origin on the raw flights, within each month,
# then re-aggregate -- build the lineup by hand, one panel is the real data
set.seed(514)
pos <- sample(1:14)
flights_lineup <- bind_cols(.sample = rep(pos[1], nrow(flights_mth)), 
                             as_tibble(flights_mth))
for (i in 1:13) {
  flights_mth_s <- flights_ts |> 
    as_tibble() |>
    group_by(month) |>
    mutate(origin = sample(origin)) |>
    ungroup() |>
    group_by(month, origin) |>
    summarise(dep_delay = mean(dep_delay, na.rm=TRUE))
  x <- bind_cols(.sample = rep(pos[i + 1], nrow(flights_mth_s)), 
                 flights_mth_s)
  flights_lineup <- bind_rows(flights_lineup, x)
}

ggplot(flights_lineup, 
       aes(x=month, y=dep_delay, colour=origin)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~.sample, ncol=7) +
  theme(aspect.ratio = 0.5, 
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())


## ---------------------------------------------------------
#| label: ts-features
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
tourism_feat <- tourism |>
  mutate(sTrips = (Trips - mean(Trips))/sd(Trips)) |>
  features(sTrips, feat_stl)
tourism_feat |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point()  


## ---------------------------------------------------------
#| label: tsibbletalk1
#| fig-height: 5
#| fig-width: 10
#| out-width: 100%
# Click on lines or points to highlight
tourism_shared <- tourism |>
  as_shared_tsibble(spec = (State / Region) * Purpose)

tourism_feat <- tourism_shared |>
  mutate(sTrips = (Trips - mean(Trips))/sd(Trips)) |>
  features(sTrips, feat_stl)

p1 <- tourism_shared |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  geom_point(size = 0.01, alpha = 0) +
  ylab("Trips") +
  facet_wrap(~ Purpose, scales = "free_y") 
p2 <- tourism_feat |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region)) 
  
subplot(
    ggplotly(p1, tooltip = "Region", width = 1400, height = 700),
    ggplotly(p2, tooltip = "Region", width = 1200, height = 600),
    nrows = 1, widths=c(0.5, 0.5), heights=1,
    titleX = TRUE, titleY = TRUE) |>
  highlight(dynamic = FALSE)
  


## ---------------------------------------------------------
#| label: tsibbletalk3
#| eval: false
# pp <- p_bourke |>
#         as_tsibble(index = time) |>
#         ggplot(aes(x=time, y=count)) +
#           geom_line() +
#           theme(aspect.ratio=0.5)
# 
# 
# ui <- fluidPage(tsibbleWrapUI("tswrap"))
# server <- function(input, output, session) {
#   tsibbleWrapServer("tswrap", pp, period = "1 day")
# }
# 
# shinyApp(ui, server)


## ---------------------------------------------------------
#| label: tsibbletalk4
#| eval: false
# lynx_tsb <- as_tsibble(lynx) |>
#   rename(count = value)
# pl <- ggplot(lynx_tsb,
#   aes(x = index, y = count)) +
#   geom_line(size = .2)
# 
# ui <- fluidPage(
#   tsibbleWrapUI("tswrap"))
# server <- function(input, output,
#                    session) {
#   tsibbleWrapServer("tswrap", pl,
#        period = "1 year")
# }
# shinyApp(ui, server)


## ---------------------------------------------------------
#| label: ts-visual
#| fig-width: 12
#| fig-height: 4
#| out-width: 100%
#| echo: false
pts <- pedestrian |>
  filter(Sensor == "Southern Cross Station") |>
  filter(between(Date, ymd("2015-07-06"), ymd("2015-07-13"))) |> ggplot() +
  geom_line(aes(x=Date_Time, y=Count)) +
  xlab("") +
  ggtitle("Time series") +
  theme(aspect.ratio=0.5)
plong <- wages |>
  sample_n_keys(size = 10) |>
  ggplot() +
  geom_line(aes(x=xp, y=ln_wages, group=id, colour=factor(id))) +
  xlab("Years") + ylab("Wages (log)") +
  ggtitle("Longitudinal") + 
  theme(aspect.ratio=0.5, legend.position="none") 
 pts + plong          


## ---------------------------------------------------------
#| label: wages-trend1  
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
wages |>
  ggplot() +
    geom_line(aes(x = xp, y = ln_wages, group = id), alpha=0.1) +
    geom_smooth(aes(x = xp, y = ln_wages), se=F) +
    xlab("years of experience") +
    ylab("wages (log)") +
  theme(aspect.ratio = 0.6)


## ---------------------------------------------------------
#| label: wages-trend2  
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
wages |>
  ggplot() +
    geom_line(aes(x = xp, y = ln_wages, group = id), alpha=0.1) +
    geom_smooth(aes(x = xp, y = ln_wages, 
      group = high_grade, colour = high_grade), se=F) +
    xlab("years of experience") +
    ylab("wages (log)") +
  scale_colour_viridis_c("education") +
  theme(aspect.ratio = 0.6)


## ---------------------------------------------------------
#| label: sample-n1
set.seed(753)
wages |>
  sample_n_keys(size = 10) |> 
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = as.factor(id))) + 
  geom_line() +
  xlim(c(0,13)) + ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: sample-n2
set.seed(749)
wages |>
  sample_n_keys(size = 10) |> 
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = as.factor(id))) + 
  geom_line() +
  xlim(c(0,13)) + ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: sample-n3
set.seed(757)
wages |>
  sample_n_keys(size = 10) |> 
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = as.factor(id))) + 
  geom_line() +
  xlim(c(0,13)) + ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: increasing
wages_slope <- wages |>   
  add_n_obs() |>
  filter(n_obs > 4) |>
  add_key_slope(ln_wages ~ xp) |> 
  as_tsibble(key = id, index = xp) 
wages_spread <- wages |>
  features(ln_wages, feat_spread) |>
  right_join(wages_slope, by="id")

wages_slope |> 
  filter(.slope_xp > 0.3) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 4.5)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: decreasing
wages_slope |> 
  filter(.slope_xp < (-0.4)) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 4.5)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: small-sigma
wages_spread |> 
  filter(sd < 0.1) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 12)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: large-sigma
wages_spread |> 
  filter(sd > 0.8) |> 
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id,
             colour = factor(id))) + 
  geom_line() +
  xlim(c(0, 12)) +
  ylim(c(0, 4.5)) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: five-number
#| fig-width: 8
#| fig-height: 5
wages_fivenum <- wages |>   
  add_n_obs() |>
  filter(n_obs > 6) |>
  key_slope(ln_wages ~ xp) |>
  keys_near(key = id,
            var = .slope_xp,
            funs = l_five_num) |> 
  left_join(wages, by = "id") |>
  as_tsibble(key = id, index = xp) 
  
wages_fivenum |>
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id)) + 
  geom_line() + 
  ylim(c(0, 4.5)) +
  facet_wrap(~stat, ncol=3) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6, legend.position = "none")


## ---------------------------------------------------------
#| label: model-fit
#| fig-width: 8
#| fig-height: 4
wages_fit_int <- 
  lmer(ln_wages ~ xp + high_grade + 
         (xp |id), data = wages) 
wages_aug <- wages |>
  add_predictions(wages_fit_int, 
                  var = "pred_int") |>
  add_residuals(wages_fit_int, 
                var = "res_int")
  
m1 <- ggplot(wages_aug,
       aes(x = xp,
           y = pred_int,
           group = id)) + 
  geom_line(alpha = 0.2) +
  xlab("years of experience") +
  ylab("wages (log)") +
  theme(aspect.ratio = 0.6)
  
m2 <- ggplot(wages_aug,
       aes(x = pred_int,
           y = res_int,
           group = id)) + 
  geom_point(alpha = 0.5) +
  xlab("fitted values") + ylab("residuals")  

m1 + m2 + plot_layout(ncol=2) 
    


## ---------------------------------------------------------
#| label: model-diag
#| fig-width: 8
#| fig-height: 7
#| out-width: 80%
wages_aug |> add_n_obs() |> filter(n_obs > 4) |>
  sample_n_keys(size = 12) |>
  ggplot() + 
  geom_line(aes(x = xp, y = pred_int, group = id, 
             colour = factor(id))) + 
  geom_point(aes(x = xp, y = ln_wages, 
                 colour = factor(id))) + 
  facet_wrap(~id, ncol=3)  +
  xlab("Years of experience") + ylab("Log wages") +
  theme(aspect.ratio = 0.6, legend.position = "none")
  


## ---------------------------------------------------------
#| label: nasa
#| echo: false
data(nasa)
glimpse(nasa)


## ---------------------------------------------------------
#| label: cubble-object
nasa_cb <- as_cubble(as_tibble(nasa), 
                     key=id, 
                     index=time, 
                     coords=c(long, lat))
nasa_cb


## ---------------------------------------------------------
#| label: spatial
#| fig-width: 6
#| fig-height: 6
#| out-width: 90%
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


## ---------------------------------------------------------
#| label: temporal
#| fig-width: 8
#| fig-height: 4
#| out-width: 90%
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


## ---------------------------------------------------------
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


## ---------------------------------------------------------
#| label: raster
#| eval: false
#| echo: true
# # Get the map
# sth_america <- map_data("world") |>
#   filter(between(long, -115, -53), between(lat, -20.5, 41))
# 
# nasa_cb |>
#   face_temporal() |>
#   filter(month == "Jan", year == 1995) |>
#   select(id, time, surftemp) |>
#   unfold(long, lat) |>
#   ggplot() +
#   geom_tile(aes(x=long, y=lat, fill=surftemp)) +
#   geom_path(data=sth_america,
#             aes(x=long, y=lat, group=group),
#             colour="white", linewidth=1) +
#   scale_fill_viridis_c("", option = "magma") +
#   ggtitle("January 1995") +
#   theme_map() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(size = 24))


## ---------------------------------------------------------
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


## ---------------------------------------------------------
#| label: space-time
#| eval: false
#| echo: true
# nasa_cb |> face_temporal() |>
#   select(id, time, month, year, surftemp) |>
#   unfold(long, lat) |>
#   ggplot() +
#   geom_tile(aes(x=long, y=lat, fill=surftemp)) +
#   facet_grid(year~month) +
#   scale_fill_viridis_c("", option = "magma") +
#   theme_map() +
#   theme(legend.position = "bottom")


## ---------------------------------------------------------
#| label: time-space1
#| fig-width: 8
#| fig-height: 8
#| out-width: 90%
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



## ---------------------------------------------------------
#| label: time-space2
#| fig-width: 8
#| fig-height: 8
#| out-width: 90%
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


## ---------------------------------------------------------
#| label: time-space3
#| fig-width: 8
#| fig-height: 8
#| out-width: 90%
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


## ---------------------------------------------------------
#| label: time-space4
#| fig-width: 6
#| fig-height: 6
#| out-width: 90%
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


## ---------------------------------------------------------
#| label: nasa-tsibbletalk-demo
#| eval: false
library(dplyr)
library(ggplot2)
library(tsibble)
library(tsibbletalk)
library(lubridate)
library(plotly)
sth_america <- map_data("world") |>
  filter(between(long, -115, -53), between(lat, -20.5, 41))

nasa_shared <- nasa |>
  mutate(date = ymd(date)) |>
  select(long, lat, date, surftemp, id) |>
  as_tsibble(index=date, key=id) |>
  as_shared_tsibble()
sp1 <- ggplot() +
  geom_polygon(data=sth_america,
            aes(x=long, y=lat, group=group),
            colour="#ffffff", alpha=0.2, fill="#014221") +
  geom_point(data=nasa_shared, aes(x = long,
         y = lat, group = id))
sp2 <- nasa_shared |>
  ggplot(aes(x = date, y = surftemp)) +
  geom_line(aes(group = id), alpha = 0.5) +
  geom_point(size = 0.01, alpha = 0)
subplot(
    ggplotly(sp1, tooltip = "Region"),
    ggplotly(sp2, tooltip = "Region"),
    nrows = 1, widths=c(0.3, 0.7)) |>
  highlight(dynamic = TRUE)


## ---------------------------------------------------------
#| echo: false
countdown::countdown(3, 28)


## ---------------------------------------------------------
#| echo: false
countdown::countdown(3, 7)


## ---------------------------------------------------------
#| label: nasa-trough-activity
#| eval: false
# nasa_feat <- nasa |>
#   mutate(yrmth = yearmonth(paste(year, month))) |>
#   as_tsibble(index = yrmth, key = id) |>
#   features(surftemp, feat_stl) |>
#   select(id, seasonal_trough_year)
# 
# nasa_trough <- nasa |>
#   distinct(id, long, lat) |>
#   left_join(nasa_feat, by = "id")
# 
# ggplot() +
#   geom_polygon(data = sth_america,
#             aes(x = long, y = lat, group = group),
#             fill = "#014221", alpha = 0.2, colour = "#ffffff") +
#   geom_point(data = nasa_trough,
#              aes(x = long, y = lat, colour = factor(seasonal_trough_year))) +
#   scale_colour_viridis_d("month of\nseasonal trough") +
#   theme_map()


## ---------------------------------------------------------
#| label: spatial-inference-demo
#| eval: false
#| echo: false
#| results: hide
# library(gstat)
# nasa_jan95 <- nasa |>
#   filter(year == 1995, month == "Jan") |>
#   select(id, long, lat, surftemp, cloudlow, cloudmid, cloudhigh, ozone)
# row.names(nasa_jan95) <- nasa_jan95[,1]
# nasa_jan95_sf <- SpatialPointsDataFrame(nasa_jan95[,2:3],
#                    nasa_jan95[,4:8])
# g <- gstat(formula = surftemp~1,
#            data=nasa_jan95_sf)
# plot(variogram(g))
# vgm1 <- variogram(surftemp~1, nasa_jan95_sf, cloud=TRUE)
# plot(vgm1)
# # Set up model
# vgm_mod <- vgm(psill=10, model = "Sph", range=20, nmax=60)
# g_dummy <- gstat(formula = surftemp~1, dummy=TRUE, beta=295,
#            data=nasa_jan95_sf, model=vgm_mod)
# g_null <- predict(g_dummy, nasa_jan95_sf, nsim=11)
# g_null_df1 <- tibble(long = g_null@coords[,1],
#                      lat = g_null@coords[,2],
#                      surftemp1 = g_null@data$sim1,
#                      surftemp2 = g_null@data$sim2,
#                      surftemp3 = g_null@data$sim3)
# p_data <- nasa_cb |>
#   face_temporal() |>
#   filter(month == "Jan", year == 1995) |>
#   select(id, time, surftemp) |>
#   unfold(long, lat) |>
#   ggplot() +
#   geom_tile(aes(x=long, y=lat, fill=surftemp)) +
#   geom_path(data=sth_america,
#             aes(x=long, y=lat, group=group),
#             colour="white", linewidth=1) +
#   scale_fill_viridis_c("", option = "magma") +
#   #ggtitle("January 1995") +
#   theme_map() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(size = 24))
# p_null1 <- ggplot() +
#   geom_tile(data=g_null_df1,
#             aes(x=long, y=lat, fill=surftemp1)) +
#   geom_path(data=sth_america,
#             aes(x=long, y=lat, group=group),
#             colour="white", linewidth=1) +
#   scale_fill_viridis_c("", option = "magma") +
#   #ggtitle("January 1995") +
#   theme_map() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(size = 24))
# p_null2 <- ggplot() +
#   geom_tile(data=g_null_df1,
#             aes(x=long, y=lat, fill=surftemp2)) +
#   geom_path(data=sth_america,
#             aes(x=long, y=lat, group=group),
#             colour="white", linewidth=1) +
#   scale_fill_viridis_c("", option = "magma") +
#   #ggtitle("January 1995") +
#   theme_map() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(size = 24))
# p_null3 <- ggplot() +
#   geom_tile(data=g_null_df1,
#             aes(x=long, y=lat, fill=surftemp3)) +
#   geom_path(data=sth_america,
#             aes(x=long, y=lat, group=group),
#             colour="white", linewidth=1) +
#   scale_fill_viridis_c("", option = "magma") +
#   #ggtitle("January 1995") +
#   theme_map() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(size = 24))
# p_data + p_null1 + p_null2 + p_null3 + plot_layout(ncol=2)


## ----toy-spatial, out.width = "80%"-----------------------
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


## ---------------------------------------------------------
#| label: plot-simple example
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


## ---------------------------------------------------------
#| label: gen-nulls
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


## ---------------------------------------------------------
#| label: world-map-table
#| echo: false
world_map <- map_data("world")
world_map |> 
  filter(region %in% c("Australia", "New Zealand")) |> 
      DT::datatable(width=1150, height=100)


## ---------------------------------------------------------
#| label: mappolygon
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


## ---------------------------------------------------------
#| label: sfobject
#| echo: false
#| results: hide
library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
nc |> slice_head(n=5) 


## ---------------------------------------------------------
#| echo: false
countdown::countdown(5, 45)


## ---------------------------------------------------------
#| label: thin-nsw-postcode
#| eval: false
# # 1. Read data
# poa <- st_read("data/POA_2021_AUST_GDA2020_SHP/POA_2021_AUST_GDA2020.shp")
# poa_nsw <- poa |>
#   filter(!is.na(suppressWarnings(as.numeric(POA_CODE21)))) |>
#   filter(as.numeric(POA_CODE21) >= 1000 & as.numeric(POA_CODE21) <= 2999) |>
#   filter(!POA_CODE21 %in% c("2898", "2899")) # drop Lord Howe & Norfolk Island
# nrow(poa_nsw)          # sanity check on feature count
# object.size(poa_nsw)   # size of the full-resolution object
# 
# # 2. Simplify with rmapshaper ------------------------------------------
# # keep = fraction of vertices to retain (0.05 = keep 5%, aggressive simplification)
# poa_nsw_simplified <- ms_simplify(poa_nsw, keep = 0.01, keep_shapes = TRUE)
# 
# object.size(poa_nsw_simplified)  # compare size
# 
# # 3. Time the plotting ---------------------------------------------------
# # Use a fresh graphics device each time so rendering isn't cached/reused
# time_full <- system.time({
#   ggplot(poa_nsw) +
#     geom_sf(colour = "white", fill = "grey70") +
#     theme_map()
# })
# time_full
# 
# time_simplified <- system.time({
#   ggplot(poa_nsw_simplified) +
#     geom_sf(colour = "white", fill = "grey70") +
#     theme_map()
# })
# 
# time_simplified
# 


## ---------------------------------------------------------
#| label: plot-nsw-postcode
#| eval: false
#| fig-width: 8
#| fig-height: 8

# mp1 <- ggplot(poa_nsw) +
#     geom_sf(colour = "white", fill = "grey70") +
#     ggtitle("Full") +
#     theme_map()
# 
# mp2 <- ggplot(poa_nsw_simplified) +
#     geom_sf(colour = "white", fill = "grey70") +
#     ggtitle("Simplified") +
#     theme_map()
# 
# mp1 + mp2 + plot_layout(ncol=2)


## ---------------------------------------------------------
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


## ---------------------------------------------------------
#| label: thyroiddata
#| eval: false
# sa2 <- strayr::read_absmap("sa22011") |>
#   filter(!st_is_empty(geometry)) |>
#   filter(!state_name_2011 == "Other Territories") |>
#   filter(!sa2_name_2011 == "Lord Howe Island")
# sa2 <- sa2 |> rmapshaper::ms_simplify(keep = 0.5, keep_shapes = TRUE) # Simplify the map!!!
# SIR <- read_csv(here::here("data/SIR Downloadable Data.csv")) |>
#   filter(SA2_name %in% sa2$sa2_name_2011) |>
#   dplyr::select(Cancer_name, SA2_name, Sex_name, p50) |>
#   filter(Cancer_name == "Thyroid", Sex_name == "Females")
# ERP <- read_csv(here::here("data/ERP.csv")) |>
#   filter(REGIONTYPE == "SA2", Time == 2011, Region %in% SIR$SA2_name) |>
#   dplyr::select(Region, Value)
# # Alternative maps
# # Join with sa2 sf object
# sa2thyroid_ERP <- SIR |>
#   left_join(sa2, ., by = c("sa2_name_2011" = "SA2_name")) |>
#   left_join(., ERP |>
#               dplyr::select(Region,
#               Population = Value), by = c("sa2_name_2011"= "Region")) |>
#   filter(!st_is_empty(geometry))
# sa2thyroid_ERP <- sa2thyroid_ERP |>
#   #filter(!is.na(Population)) |>
#   filter(!sa2_name_2011 == "Lord Howe Island") |>
#   mutate(SIR = map_chr(p50, aus_colours)) |>
#   st_as_sf()
# save(sa2, file="data/sa2.rda")
# save(sa2thyroid_ERP, file="data/sa2thyroid_ERP.rda")


## ---------------------------------------------------------
#| label: choro
#| fig-width: 10
#| fig-height: 8
#| out-width: 100%
# Plot the choropleth
load("../data/sa2thyroid_ERP.rda")
aus_ggchoro <- ggplot(sa2thyroid_ERP) + 
  geom_sf(aes(fill = SIR), size = 0.1) + 
  scale_fill_identity() + invthm
aus_ggchoro


## ---------------------------------------------------------
#| label: cartogram
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


## ---------------------------------------------------------
#| label: hexmap
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

