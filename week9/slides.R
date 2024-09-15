## ----include = FALSE, echo=FALSE----------------------------------------
source("../setup.R")


## -----------------------------------------------------------------------
#| eval: false
#| echo: false
## CO2.ptb <- read.table("https://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/merged_in_situ_and_flask/monthly/monthly_merge_co2_ptb.csv", sep=",", skip=69)
## colnames(CO2.ptb) <- c("year", "month", "dateE", "date", "co2_ppm", "sa_co2", "fit", "sa_fit", "co2f", "sa_co2f")
## CO2.ptb$lat <- (-71.3)
## CO2.ptb$lon <- (-156.6)
## CO2.ptb$stn <- "ptb"
## CO2.ptb$co2_ppm <- replace_na(CO2.ptb$co2_ppm, -99.99)
## 
## save(CO2.ptb, file=here::here("data/CO2_ptb.rda"))


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: ped-reg
options(width=55)
pedestrian 


## -----------------------------------------------------------------------
#| label: nycflights
options(width=55)
library(nycflights13)
flights_ts <- flights |>
  mutate(dt = ymd_hm(paste(paste(year, month, day, sep="-"), 
                           paste(hour, minute, sep=":")))) |>
  as_tsibble(index = dt, key = c(origin, dest, carrier, tailnum), regular = FALSE)
flights_ts 


## -----------------------------------------------------------------------
#| code-fold: true
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


## -----------------------------------------------------------------------
#| code-fold: true
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


## -----------------------------------------------------------------------
#| code-fold: true
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


## -----------------------------------------------------------------------
#| code-fold: true
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
  ggplot(aes(x=air_time, y=air_time2, label = paste(origin, dest))) + 
    geom_abline(intercept=0, slope=1) +
    geom_point()
ggplotly(fp, width=500, height=500)


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: missing-gaps
has_gaps(harvest, .full = TRUE) 


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: count-gaps
count_gaps(harvest,  .full=TRUE)


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: fill-gaps
harvest <- fill_gaps(harvest, 
                     .full=TRUE) 
harvest 


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: impute-gaps
harvest_nomiss <- harvest |> 
  group_by(fruit) |> 
  mutate(kilo = 
    na_interpolation(kilo)) |> 
  ungroup()
harvest_nomiss 


## ----CO2_ratio, fig.width=12, fig.height=7, out.width="80%"-------------
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


## -----------------------------------------------------------------------
#| label: CO2-ratio
#| echo: true
#| eval: false
## load(here::here("data/CO2_ptb.rda"))
## CO2.ptb <- CO2.ptb |>
##   filter(year > 1980) |>
##   filter(co2_ppm > 100) # handle missing values
## p <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) +
##   geom_line(size=1) + xlab("") + ylab("CO2 (ppm)")
## p1 <- p + theme(aspect.ratio = 1) + ggtitle("1 to 1 (may be useless)")
## p3 <- p + theme(aspect.ratio = 2) + ggtitle("tall & skinny:  trend")
## p2 <- ggplot(CO2.ptb, aes(x=date, y=co2_ppm)) +
##   annotate("text", x=2000, y=375, label="CO2 at \n Point Barrow,\n Alaska", size=8) + theme_solid()
## p4 <- p +
##   scale_x_continuous("", breaks = seq(1980, 2020, 5)) +
##   theme(aspect.ratio = 0.2) + ggtitle("short & wide: seasonality")
## grid.arrange(p1, p2, p3, p4, layout_matrix= matrix(c(1,2,3,4,4,4), nrow=2, byrow=T))


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: calendar
#| echo: true
#| eval: false
## flights_hourly <- flights |>
##   group_by(time_hour, origin) |>
##   summarise(count = n(),
##     dep_delay = mean(dep_delay,
##                      na.rm = TRUE)) |>
##   ungroup() |>
##   as_tsibble(index = time_hour,
##              key = origin) |>
##     mutate(dep_delay =
##     na_interpolation(dep_delay))
## calendar_df <- flights_hourly |>
##   filter(origin == "JFK") |>
##   mutate(hour = hour(time_hour),
##          date = as.Date(time_hour)) |>
##   filter(year(date) < 2014) |>
##   frame_calendar(x=hour, y=count, date=date, nrow=2)
## p1 <- calendar_df |>
##   ggplot(aes(x = .hour, y = .count, group = date)) +
##   geom_line() + theme(axis.line.x = element_blank(),
##                       axis.line.y = element_blank()) +
##   theme(aspect.ratio=0.5)
## prettify(p1, size = 3, label.padding = unit(0.15, "lines"))


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: calendar-delay
#| echo: true
#| eval: false
## calendar_df <- flights_hourly |>
##   filter(origin == "JFK") |>
##   mutate(hour = hour(time_hour),
##          date = as.Date(time_hour)) |>
##   filter(year(date) < 2014) |>
##   frame_calendar(x=hour, y=dep_delay, date=date, nrow=2)
## p1 <- calendar_df |>
##   ggplot(aes(x = .hour, y = .dep_delay, group = date)) +
##   geom_line() + theme(axis.line.x = element_blank(),
##                       axis.line.y = element_blank()) +
##   theme(aspect.ratio=0.5)
## prettify(p1, size = 3, label.padding = unit(0.15, "lines"))


## -----------------------------------------------------------------------
#| label: lm-lineup
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: NYC-lineup
#| code-fold: true
#| fig-width: 10
#| fig-height: 5
#| out-width: 100%
set.seed(514)
ggplot(lineup(null_permute("origin"), true=flights_mth, n=12), 
       aes(x=month, y=dep_delay, colour=origin)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~.sample, ncol=4) +
  theme(aspect.ratio = 0.5, 
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())


## -----------------------------------------------------------------------
#| label: ts-features
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
#| code-fold: true
tourism_feat <- tourism |>
  features(Trips, feat_stl)
tourism_feat |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point()  


## -----------------------------------------------------------------------
#| label: tsibbletalk1
#| code-fold: true
#| fig-height: 5
#| fig-width: 10
#| out-width: 100%
tourism_shared <- tourism |>
  as_shared_tsibble(spec = (State / Region) * Purpose)

tourism_feat <- tourism_shared |>
  features(Trips, feat_stl)

p1 <- tourism_shared |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line(aes(group = Region), alpha = 0.5) +
  facet_wrap(~ Purpose, scales = "free_y") 
p2 <- tourism_feat |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region))
  
subplot(
    ggplotly(p1, tooltip = "Region", width = 1400, height = 700),
    ggplotly(p2, tooltip = "Region", width = 1200, height = 600),
    nrows = 1, widths=c(0.5, 0.5), heights=1) |>
  highlight(dynamic = FALSE)
  


## -----------------------------------------------------------------------
#| label: tsibbletalk3
#| eval: false
## pp <- p_bourke |>
##         as_tsibble(index = time) |>
##         ggplot(aes(x=time, y=count)) +
##           geom_line() +
##           theme(aspect.ratio=0.5)
## 
## 
## ui <- fluidPage(tsibbleWrapUI("tswrap"))
## server <- function(input, output, session) {
##   tsibbleWrapServer("tswrap", pp, period = "1 day")
## }
## 
## shinyApp(ui, server)


## -----------------------------------------------------------------------
#| label: tsibbletalk4
#| eval: false
## lynx_tsb <- as_tsibble(lynx) |>
##   rename(count = value)
## pl <- ggplot(lynx_tsb,
##   aes(x = index, y = count)) +
##   geom_line(size = .2)
## 
## ui <- fluidPage(
##   tsibbleWrapUI("tswrap"))
## server <- function(input, output,
##                    session) {
##   tsibbleWrapServer("tswrap", pl,
##        period = "1 year")
## }
## shinyApp(ui, server)


## -----------------------------------------------------------------------
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


## -----------------------------------------------------------------------
#| label: wages-trend1
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: wages-trend2
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: sample-n1
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: sample-n2
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: sample-n3
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: increasing
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: decreasing
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: small-sigma
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: large-sigma
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: five-number
#| code-fold: true
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


## -----------------------------------------------------------------------
#| label: model-fit
#| code-fold: true
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
    


## -----------------------------------------------------------------------
#| label: model-diag
#| code-fold: true
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
  

