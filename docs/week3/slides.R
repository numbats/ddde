## -----------------------------------------------------------------------------
#| label: setup
#| message: false
#| warning: false
source("../setup.R")


## ----echo = TRUE--------------------------------------------------------------
library(readxl)
library(here)
df <- read_excel(here("data/lecture3-example.xlsx"))
df


## ----echo = TRUE--------------------------------------------------------------
library(lubridate)
df <- read_excel(here("data/lecture3-example.xlsx"), 
                 col_types = c("text", 
                               "date", 
                               "text",
                               "numeric"))

df |> 
  mutate(id = as.factor(id),
         date = ydm(date)) |>
  mutate(
         day = day(date),
         month = month(date),
         year = year(date)) 


## ----echo = TRUE--------------------------------------------------------------
xlsx_df <- read_excel(here("data/lecture3-example.xlsx"),
                 col_types = c("text", "date", "text", "numeric"))  |> 
  mutate(id = as.factor(id), 
         date = as.character(date),
         date = as.Date(date, format = "%Y-%d-%m"))


## ----echo = TRUE--------------------------------------------------------------
csv_df <- read_csv(here::here("data/lecture3-example.csv"),
                 col_types = cols(
                      id = col_factor(),
                      date = col_date(format = "%m/%d/%y"),
                      loc = col_character(),
                      temp = col_double()))


## ----echo = TRUE--------------------------------------------------------------
dplyr::glimpse(xlsx_df)
dplyr::glimpse(csv_df)


## ----echo = TRUE--------------------------------------------------------------
library(visdat)
vis_dat(xlsx_df)


## -----------------------------------------------------------------------------
#| label: example2-check
ex2 <- read_csv(here::here("data/lecture3-example2.csv"))
glimpse(ex2)


## -----------------------------------------------------------------------------
options(width=80)
raw_dat <- read_csv(here("data/world-development-indicators.csv"), 
                    na = "..", n_max = 11935)
glimpse(raw_dat)


## -----------------------------------------------------------------------------
country_code_df <- raw_dat  |>
  distinct(`Country Name`, `Country Code`)  |>
  rename_all(janitor::make_clean_names)  |>
  left_join(
    countrycode::codelist |> select(iso3c, region, continent),
    by = c("country_code" = "iso3c")
  )  |>
  arrange(continent, region) 


## -----------------------------------------------------------------------------
options(width=80)
glimpse(country_code_df)

country_code_df |> count(continent)
country_code_df |> count(region)



## -----------------------------------------------------------------------------
country_code_df |> filter(is.na(continent))


## -----------------------------------------------------------------------------
wdi_vars <- raw_dat  |>
  select(`Series Name`, `Series Code`) |>
  distinct() |>
  rename_all(janitor::make_clean_names) 


## -----------------------------------------------------------------------------
wdi_vars |> gt::gt()


## -----------------------------------------------------------------------------
wdi <- raw_dat  |>
  select(`Country Code`, `Series Code`, `1969 [YR1969]`:`2018 [YR2018]`) |>
  rename_all(janitor::make_clean_names) |>
  pivot_longer(x1969_yr1969:x2018_yr2018,
               names_to = "year", 
               values_to = "value") |>
  mutate(year = as.numeric(str_sub(year, 2, 5)) ) |>
  pivot_wider(names_from = series_code,
              values_from = value)

wdi2017 <- wdi  |> filter(year == 2017)


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
vis_miss(wdi, sort_miss = TRUE)


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
gg_miss_var(wdi, show_pct=TRUE)


## -----------------------------------------------------------------------------
#| fig-width: 2
#| fig-height: 8
#| out-width: 100%
wdi_cnt_miss <- wdi |> 
  filter(!is.na(country_code)) |>
  bind_shadow() |>
  select(country_code, year,
         SP.ADO.TFRT_NA:SP.URB.GROW_NA) |>
  pivot_longer(SP.ADO.TFRT_NA:SP.URB.GROW_NA,
               names_to="var",
               values_to="value") |>
  group_by(country_code) |>
  count(value) |>
  mutate(value = fct_recode(value, 
                            miss="NA",
                            not="!NA")) |>
  pivot_wider(names_from = value, values_from = n) |>
  mutate(p_miss = miss/(miss+not)) |>
  select(country_code, p_miss)
wdi_cnt_p <- wdi_cnt_miss |> 
  ggplot(aes(x=1, y=p_miss, 
             label=country_code)) +
  geom_quasirandom() +
  ylim(c(0,1)) + ylab("Prop miss") 
ggplotly(wdi_cnt_p)


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
wdi_cri <- wdi |>
  filter(country_code == "CRI")
vis_miss(wdi_cri, sort_miss=TRUE)


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 3
wdi_cri_p <- wdi_cri |>
  ggplot(aes(x=year, y=SE.PRM.CMPT.ZS)) +
  geom_miss_point() +
  theme(aspect.ratio=0.5, 
        legend.position = "none") 
wdi_cri_p


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 3
library(imputeTS)

wdi_cri_v1 <- wdi_cri |>
  mutate(SE.PRM.CMPT.ZS = na_ma(SE.PRM.CMPT.ZS))

wdi_cri_v1  |>
  ggplot(aes(x=year, y=SE.PRM.CMPT.ZS)) +
  geom_point() +
  geom_smooth(se=F, colour="#D55E00") +
  theme(aspect.ratio=0.5) 


## -----------------------------------------------------------------------------
#| echo: false
countdown::countdown(8, 34)


## -----------------------------------------------------------------------------
#| label: wdi-country-check
#| fig-width: 8
#| fig-height: 6
wdi |>
  filter(country_code == "AUS") |>
  vis_miss(sort_miss = TRUE)


## -----------------------------------------------------------------------------
DiagrammeR::grViz("digraph {
  graph [layout = circo, rankdir = TB]
  
  node [shape = rectangle, 
  fontname = 'Helvetica']        
  rec1 [label = 'Input data', 
  fontname = 'Helvetica']
  rec2 [label = 'Tidy data', 
  fontname = 'Helvetica']
  rec3 [label = 'Data screening', 
  fontname = 'Helvetica']
  rec4 [label = 'Data cleaning', 
  fontname = 'Helvetica']
  rec5 [label = 'Impute missings', 
  fontname = 'Helvetica']
  rec6 [label = 'Valid data', 
  fontname = 'Helvetica']

  # edge definitions with the node IDs
  rec1 -> rec2 -> rec3 -> rec4 -> rec5  
  rec5 -> rec3 
  rec3 -> rec6
  }",
  height = 300,
  width = 500
  )


## -----------------------------------------------------------------------------
#| eval: false
# # Note: because this downloads data
# # run it once, and save the result so
# # data server isn't overloaded
# library(readabs)
# employed <- read_abs(series_id = "A84423085A")  |>
#   mutate(month = lubridate::month(date),
#          year = lubridate::year(date))  |>
#   #filter(year != "2020")  |>
#   select(date, month, year, value)
# save(employed, file="../data/employed.rda")


## ----echo = TRUE--------------------------------------------------------------
load(here::here("data/employed.rda"))

glimpse(employed)


## -----------------------------------------------------------------------------
#| label: unemp
#| fig-width: 5
#| fig-height: 8
#| out-width: 70%
employed_2000 <- employed |> 
  filter(between(year, 2005, 2014)) |>
  mutate(year = factor(year)) |>
  ungroup() 
labels_2000 <- employed_2000 |>
  group_by(year) |>
  slice_tail(n = 1) |>
  ungroup() 

ggplot() + 
  geom_line(data = employed_2000, 
    aes(x = month, y = value/1000, color = year)) + 
  geom_label_repel(
    data = labels_2000,
    aes(x = month, y = value/1000, 
        label = year, color = year),
    fill = alpha("white", 0.6),
    direction = "y",
    hjust = 0,
    nudge_x = 0.5,
    segment.color = "grey70"
  ) +
  ylab("employed (mil)") +
  scale_x_continuous("month", breaks=seq(1, 12, 1)) +
  scale_color_viridis_d("", end = 0.8) +
  theme(aspect.ratio = 1.5,
        legend.position = "none")


## -----------------------------------------------------------------------------
employed_2000  |> 
  filter(month %in% 8:9)  |> 
  pivot_wider(id_cols = year, 
    names_from = month)  |> 
  mutate(diff = `9` - `8`)  |> 
  ggplot(aes(x = diff, y = 1)) + 
  geom_boxplot() 


## -----------------------------------------------------------------------------
#| echo: false
countdown::countdown(5, 22)


## -----------------------------------------------------------------------------
#| label: employment-month-check
employed_diffs <- employed_2000 |> 
  arrange(date) |> 
  mutate(diff = value - dplyr::lag(value)) |>
  mutate(month_c = factor(month, levels = c(1:12), 
    labels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))) |>
  group_by(month_c) |>
  mutate(diff_m = diff - 
    median(diff, na.rm = TRUE)) |>
  ungroup()


## -----------------------------------------------------------------------------
#| label: employment-month-plot
#| out-width: 30%
employed_diffs |> 
  filter(!is.na(diff)) |>
  ggplot(aes(x = month_c, y = diff_m)) + 
    geom_boxplot()


## -----------------------------------------------------------------------------
options(width=50)
load(here::here("data/french_fries.rda"))
glimpse(french_fries)


## -----------------------------------------------------------------------------
french_fries |> count(subject)


## -----------------------------------------------------------------------------
french_fries |> count(time)
french_fries |> count(treatment)
french_fries |> count(rep)


## -----------------------------------------------------------------------------
#| label: design-check
field <- read_csv(here::here("data/lecture3-example3.csv"))
field |> 
  count(trt, block) |> 
  pivot_wider(names_from = block, values_from = n)


## -----------------------------------------------------------------------------
warranty <- read_csv(here("data/warranty-kaggle.csv"))
warranty |>
  filter(Product_type == "TV") |>
  select(Region:Consumer_profile,
         TV_2001_Issue:TV_2003_Issue,
         Claim_Value:Purpose) |>
  glimpse()


## -----------------------------------------------------------------------------
set.seed(635)
warranty |> 
  filter(Product_type == "TV") |> 
  count(City, sort=TRUE) |> 
  slice_sample(n=2)


## -----------------------------------------------------------------------------
library(VGAM)
data(olym12)
olym12 |> select(country, totalmedal) |>
  arrange(desc(totalmedal)) 


## -----------------------------------------------------------------------------
oceanbuoys |>
  ggplot(aes(x=air_temp_c, y=humidity)) +
  geom_miss_point()


## -----------------------------------------------------------------------------
#| code-fold: true
library(simputation)
ocean_imp_yr <- oceanbuoys %>%
  bind_shadow() %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns + year + longitude + latitude) %>%
  impute_lm(humidity ~  wind_ew + wind_ns + year + longitude + latitude) %>%
  impute_lm(sea_temp_c ~  wind_ew + wind_ns + year + longitude + latitude) %>%
  add_label_shadow()

ggplot(ocean_imp_yr,
       aes(x = air_temp_c,
           y = humidity,
           color = any_missing)) + 
  geom_point() +
  theme(legend.title = element_blank())


## -----------------------------------------------------------------------------
library(validate)
data("SBS2000", package = "validate")
dplyr::glimpse(SBS2000)


## ----echo = TRUE--------------------------------------------------------------
rules <- validator(
          is_complete(id),
          is_complete(id, turnover),
          is_complete(id, turnover, profit))
out <- confront(SBS2000, rules)
summary(out)


## ----echo = TRUE--------------------------------------------------------------
rules <- validator(
    total.rev - profit == total.costs,
    turnover + other.rev == total.rev,
    profit <= 0.6 * total.rev
)
out <- confront(SBS2000, rules)
summary(out)


## ----sleep--------------------------------------------------------------------
#| code-fold: true
data(sleep)
ggplot(sleep, aes(x=group, y=extra)) + 
  geom_boxplot() +
  geom_point(colour="#D55E00")


## -----------------------------------------------------------------------------
tt <- with(sleep,
     t.test(extra[group == 1],
            extra[group == 2], 
            paired = TRUE))
tt


## -----------------------------------------------------------------------------
#| code-fold: true
InsectSprays  |> 
  ggplot(aes(x=fct_reorder(spray, count), 
             y=count)) + 
  geom_jitter(width=0.1, height=0, colour="#D55E00", size=3, alpha=0.8) +
  xlab("") 


## ----echo=TRUE----------------------------------------------------------------
fm1 <- aov(count ~ spray, data = InsectSprays)
summary(fm1)

