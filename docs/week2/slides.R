## -----------------------------------------------------------------------------
#| label: setup
#| message: false
#| warning: false
source("../setup.R")


## -----------------------------------------------------------------------------
#| label: theme-data
#| message: false
# Theme A: small, right-skewed numeric data used throughout the
# "numerical summaries" pipeline (stem-and-leaf, hinges, box-and-whisker,
# fences, trimean, re-expression).
# Real data: unleaded 91 (U91) price (cents/litre) at NSW service stations,
# most recent report as of end of July 2026, via the NSW FuelCheck open data
# API (data.nsw.gov.au). See data/fuelcheck_u91_sample.csv for the extract,
# and week2/fuelcheck-sample.R for how it was built.
fuel <- read_csv(here::here("data/fuelcheck_u91_sample.csv"))
fuel_main <- fuel |> filter(group == "main")

# Theme A (letter-value plot demo): a much larger real extract -- the most
# recent price per station and fuel type, for the 5 common fuel types,
# across all of NSW in July 2026. See data/fuelcheck_latest_by_fueltype.csv,
# and week2/fuelcheck-sample.R for how it was built.
fuel_by_type <- read_csv(here::here("data/fuelcheck_latest_by_fueltype.csv"))

# Theme B: small categorical data used for the tallying exercise.
transport <- tibble(
  mode = c("Train", "Car", "Bike", "Train", "Walk", "Car", "Train", "Bus",
           "Bike", "Train", "Car", "Walk", "Train", "Bike", "Car", "Train",
           "Bus", "Walk", "Car", "Train", "Bike", "Train", "Car", "Walk")
)


## -----------------------------------------------------------------------------
#| label: petrol-data
options(width = 60)
print(fuel_main$price, digits = 4)


## -----------------------------------------------------------------------------
#| label: stem
stem(fuel_main$price)


## -----------------------------------------------------------------------------
#| label: stem-scale1
stem(fuel_main$price, scale = 2)


## -----------------------------------------------------------------------------
#| label: transport-data
options(width = 60)
transport$mode


## -----------------------------------------------------------------------------
#| label: transport-count
transport |> count(mode)


## -----------------------------------------------------------------------------
#| label: petrol-sorted
options(width = 25)
print(sort(fuel_main$price), digits = 4)


## -----------------------------------------------------------------------------
#| label: stem-repeat
#| echo: false
stem(fuel_main$price, scale=2)


## -----------------------------------------------------------------------------
#| label: fivenum
print(fivenum(fuel_main$price), digits = 4)


## -----------------------------------------------------------------------------
#| label: boxplot
#| out-width: 50%
ggplot(fuel_main, aes(x = "", y = price)) +
  geom_boxplot() +
  xlab("") 


## -----------------------------------------------------------------------------
#| label: boxplot-fences
#| out-width: 25%
fuel |>
  ggplot(aes(x = "", y = price)) +
  geom_boxplot() +
  xlab("")


## -----------------------------------------------------------------------------
#| label: trimean
fn <- fivenum(fuel_main$price)
print((fn[2] + 2 * fn[3] + fn[4]) / 4, digits = 4)
print(mean(fuel_main$price), digits = 4)


## -----------------------------------------------------------------------------
#| label: lvplot
fuel_by_type <- fuel_by_type |>
  mutate(fuel_type = factor(fuel_type, 
    levels = c("DL", "E10", "U91", "P95", "P98")))
ggplot(fuel_by_type, aes(fuel_type, price)) +
  geom_lv(aes(fill = after_stat(LV))) +
  scale_fill_brewer() +
  xlab("fuel type")


## -----------------------------------------------------------------------------
#| label: lvplot-k3
#| out-width: 25%
ggplot(fuel_by_type, aes(fuel_type, price)) +
  geom_lv(aes(fill = after_stat(LV)), k = 3) +
  scale_fill_brewer() +
  xlab("fuel type")


## -----------------------------------------------------------------------------
#| label: lvplot-k6
#| out-width: 25%
ggplot(fuel_by_type, aes(fuel_type, price)) +
  geom_lv(aes(fill = after_stat(LV)), k = 6) +
  scale_fill_brewer() +
  xlab("fuel type")


## -----------------------------------------------------------------------------
#| label: lvplot-k10
#| out-width: 25%
ggplot(fuel_by_type, aes(fuel_type, price)) +
  geom_lv(aes(fill = after_stat(LV)), k = 10) +
  scale_fill_brewer() +
  xlab("fuel type")


## -----------------------------------------------------------------------------
#| label: stem-original
stem(fuel_main$price, scale=2)


## -----------------------------------------------------------------------------
#| label: stem-log
stem(log10(fuel_main$price), scale=2)

