## --------------------------------------------------------
#| label: setup
#| include: false
#| echo: false
source("../setup.R")


## ----DT-options, include = FALSE-------------------------
toggle_select <- DT::JS(
  "table.on('click.dt', 'tbody tr', function() {",
  "$(this).toggleClass('selected');",
  "})"
)
table_options <- function(scrollY, title, csv) {
  list(
    dom = "Bft",
    pageLength = -1,
    searching = TRUE,
    scrollX = TRUE,
    scrollY = scrollY,
    buttons = list(
      list(
        extend = "copy",
        filename = title
      ),
      list(
        extend = "csv",
        filename = csv
      )
    )
  )
}


## --------------------------------------------------------
#| label: temp-data
#| echo: false
#| include: false
melb_temp <- read_csv(here::here("data", "melb_temp.csv")) |>
  janitor::clean_names() |>
  rename(temp = maximum_temperature_degree_c) |>
  filter(!is.na(temp)) |>
  dplyr::select(year, month, day, temp)
skimr::skim(melb_temp)


## --------------------------------------------------------
#| label: temp-plot1
#| echo: false
ggplot(melb_temp, aes(x=month, y=temp)) +
  geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), fill= "#56B4E9") +
  labs(x = "month", y = "max daily temp (°C)") +
  theme(aspect.ratio=0.5)


## ----render = knitr::normal_print------------------------
#| label: temp-data
#| echo: true
#| eval: true
melb_temp <- read_csv(here::here("data", "melb_temp.csv")) |>
  janitor::clean_names() |>
  rename(temp = maximum_temperature_degree_c) |>
  filter(!is.na(temp)) |>
  dplyr::select(year, month, day, temp)
skimr::skim(melb_temp)


## --------------------------------------------------------
#| label: temp-plot1
#| echo: true
#| eval: false
# ggplot(melb_temp, aes(x=month, y=temp)) +
#   geom_violin(draw_quantiles=c(0.25, 0.5, 0.75), fill= "#56B4E9") +
#   labs(x = "month", y = "max daily temp (°C)") +
#   theme(aspect.ratio=0.5)


## --------------------------------------------------------
#| label: temp-year
#| code-fold: true
#| fig-width: 8
#| fig-height: 5
#| out-width: 100%
melb_temp |>
  group_by(year, month) |>
  summarise(temp = mean(temp)) |>
  ggplot(aes(x=year, y=temp)) +
  geom_point(alpha=0.5) +
  geom_smooth(se=F) + 
  facet_wrap(~month, ncol=4, scales="free_y") +
  scale_x_continuous("year", breaks=seq(1970, 2020, 20)) +
  ylab("max daily temp (°C)") +
  theme(aspect.ratio=0.7)


## --------------------------------------------------------
#| echo: false
countdown::countdown(5, 43)


## --------------------------------------------------------
#| label: temp-seasonality-answer
#| code-fold: true
#| eval: false
# melb_temp_month <- melb_temp |>
#   group_by(year, month) |>
#   summarise(temp = mean(temp), .groups = "drop")
# 
# # raw comparison
# ggplot(melb_temp_month,
#     aes(x = month,
#     y = temp,
#     group = year,
#     colour = year)) +
#   geom_line(alpha = 0.4) +
#   geom_point(alpha = 0.4) +
#   ylab("max daily temp (°C)") +
#   scale_x_discrete("", labels = c("J","F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
# 
# # calibrated by each year's average temperature
# melb_temp_month |>
#   group_by(year) |>
#   mutate(temp_cal = temp / mean(temp)) |>
#   ungroup() |>
#   ggplot(aes(x = month,
#              y = temp_cal,
#              group = year,
#              colour = year)) +
#   geom_line(alpha = 0.4) +
#   geom_point(alpha = 0.4) +
#   ylab("temp relative to yearly average") +   scale_x_discrete("", labels = c("J","F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))


## --------------------------------------------------------
#| label: olives-data
#| include: false
data(olives, package = "classifly")
df2 <- olives |>
  mutate(Region = factor(Region, labels = c("South", "Sardinia", "North")))

skimr::skim(df2)


## --------------------------------------------------------
#| label: olives-plot1
#| echo: false
#| fig-height: 10
#| fig-width: 10
#| out-width: 60%
g1 <-
  df2 |>
  mutate(Area = fct_reorder(Area, palmitic)) |>
  ggplot(aes(Area, palmitic, color = Region)) +
  geom_boxplot() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE, x = guide_axis(n.dodge = 2)) +
  theme(aspect.ratio=0.5)

g2 <- ggplot(df2, aes(Region, palmitic, color = Region)) +
  geom_boxplot() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE) +
  theme(axis.text = element_blank())

g3 <- ggplot(df2, aes(palmitic, color = Region)) +
  geom_density() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE) +
  theme(axis.text = element_blank())

g4 <- ggplot(df2, aes(palmitic, color = Region)) +
  stat_ecdf() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE) +
  theme(axis.text = element_blank())

g5 <- g2 + g3 + g4 + plot_layout(ncol=3)
  
g1 + g5 + plot_layout(ncol=1, heights=c(2,1),
  guides = "collect")


## ----render = knitr::normal_print------------------------
#| label: olives-data
#| echo: true
data(olives, package = "classifly")
df2 <- olives |>
  mutate(Region = factor(Region, labels = c("South", "Sardinia", "North")))

skimr::skim(df2)


## --------------------------------------------------------
#| label: olives-plot1
#| echo: true
#| eval: false
# g1 <-
#   df2 |>
#   mutate(Area = fct_reorder(Area, palmitic)) |>
#   ggplot(aes(Area, palmitic, color = Region)) +
#   geom_boxplot() +
#   scale_color_discrete_divergingx(palette="Zissou 1") +
#   guides(color = FALSE, x = guide_axis(n.dodge = 2)) +
#   theme(aspect.ratio=0.5)
# 
# g2 <- ggplot(df2, aes(Region, palmitic, color = Region)) +
#   geom_boxplot() +
#   scale_color_discrete_divergingx(palette="Zissou 1") +
#   guides(color = FALSE) +
#   theme(axis.text = element_blank())
# 
# g3 <- ggplot(df2, aes(palmitic, color = Region)) +
#   geom_density() +
#   scale_color_discrete_divergingx(palette="Zissou 1") +
#   guides(color = FALSE) +
#   theme(axis.text = element_blank())
# 
# g4 <- ggplot(df2, aes(palmitic, color = Region)) +
#   stat_ecdf() +
#   scale_color_discrete_divergingx(palette="Zissou 1") +
#   guides(color = FALSE) +
#   theme(axis.text = element_blank())
# 
# g5 <- g2 + g3 + g4 + plot_layout(ncol=3)
# 
# g1 + g5 + plot_layout(ncol=1, heights=c(2,1),
#   guides = "collect")


## --------------------------------------------------------
#| label: color-olives
#| echo: false
ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") 


## --------------------------------------------------------
#| label: color-olives
#| eval: false
# ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
#   geom_point() +
#   scale_color_discrete_divergingx(palette="Zissou 1")


## --------------------------------------------------------
#| label: no-shadow
#| echo: false
#| fig-width: 7
#| fig-height: 7
#| out-width: 80%
ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
  geom_point() +
  facet_wrap(~Area) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE) 


## --------------------------------------------------------
#| label: no-shadow
#| eval: false
# ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
#   geom_point() +
#   facet_wrap(~Area) +
#   scale_color_discrete_divergingx(palette="Zissou 1") +
#   guides(color = FALSE)


## --------------------------------------------------------
#| label: shadow
#| echo: false
#| fig-width: 7
#| fig-height: 7
#| out-width: 80%
ggplot(olives, aes(palmitoleic, palmitic)) +
  geom_point(data = dplyr::select(olives, -Area), color = "gray") +
  geom_point(aes(color = Area), size=2) +
  facet_wrap(~Area) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE)


## --------------------------------------------------------
#| label: shadow
#| eval: false
# ggplot(olives, aes(palmitoleic, palmitic)) +
#   geom_point(data = dplyr::select(olives, -Area), color = "gray") +
#   geom_point(aes(color = Area), size=2) +
#   facet_wrap(~Area) +
#   scale_color_discrete_divergingx(palette="Zissou 1") +
#   guides(color = FALSE)


## --------------------------------------------------------
#| label: olives-coplot
#| code-fold: true
#| fig-width: 10
#| fig-height: 6
#| out-width: 100%
#| eval: false
# # Sizing of figure is difficult, so save it
# library(ggcleveland)
# olives_sard <- df2 |>
#   filter(Region == "Sardinia")
# p <- gg_coplot(olives_sard,
#   x=arachidic, y=oleic,
#   faceting = linoleic,
#   number_bins = 6,
#   overlap = 1/4) +
#   theme(aspect.ratio=0.5)


## --------------------------------------------------------
#| label: generate-data
set.seed(42)

# Simulate a small employee-engagement survey
# 5 questions, 5-point Likert scale, 200 respondents

levels_5pt <- c(
  "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"
)

survey_questions <- c(
  "My manager gives useful feedback",
  "I have the tools I need to do my job",
  "I understand how my work connects to company goals",
  "I feel comfortable raising concerns",
  "I would recommend this company as a place to work"
)

# Give each question a different underlying 
# sentiment so the charts actually show 
# a mix of positive/negative/mixed results
probs <- list(
  c(0.05, 0.10, 0.15, 0.40, 0.30),  # mostly positive
  c(0.20, 0.30, 0.20, 0.20, 0.10),  # skews negative
  c(0.05, 0.15, 0.50, 0.20, 0.10),  # mostly neutral
  c(0.15, 0.20, 0.10, 0.30, 0.25),  # mixed / bimodal-ish
  c(0.10, 0.15, 0.20, 0.30, 0.25)   # mildly positive
)

n_resp <- 200

survey_wide <- as_tibble(
  setNames(
    lapply(seq_along(survey_questions), function(i) {
      factor(
        sample(levels_5pt, n_resp, replace = TRUE, prob = probs[[i]]),
        levels = levels_5pt,
        ordered = TRUE
      )
    }),
    as.character(1:5)
  )
)

survey_long <- survey_wide |>
  pivot_longer(everything(), 
    names_to = "question_num", 
    values_to = "response") |>
  mutate(question = survey_questions[as.numeric(question_num)]) |>
  mutate(response = factor(response, 
    levels = levels_5pt, ordered = TRUE),
    question = factor(question, 
      levels = survey_questions))

stacked_data <- survey_long |>
  count(question_num, response) |>
  group_by(question_num) |>
  mutate(pct = n / sum(n)) |>
  ungroup()



## --------------------------------------------------------
#| label: stacked-bar
#| out-width: 80%
plain_stacked_bar <- ggplot(stacked_data, 
    aes(x = pct, 
        y = question_num, 
        fill = response)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_discrete(limits = rev) +
  labs(
    x = "Percent of respondents", y = NULL, fill = NULL
  ) +
  theme_minimal()
plain_stacked_bar


## --------------------------------------------------------
#| label: likert-plot
#| out-width: 80%
library(ggstats)
likert_plot <- gglikert(
  survey_wide,
  add_labels = FALSE,
  add_totals = FALSE
) +
  scale_fill_brewer(palette = "RdBu", direction = 1) 
likert_plot


## --------------------------------------------------------
#| label: likert-plot-sorted
#| out-width: 80%
library(ggstats)
likert_plot <- gglikert(
  survey_wide,
  add_labels = FALSE,
  add_totals = FALSE,
  sort = "ascending"
) +
  scale_fill_brewer(palette = "RdBu", direction = 1) 
likert_plot


## --------------------------------------------------------
#| label: tb-data
#| echo: false
# https://www.who.int/teams/global-tuberculosis-programme/data
tb <- read_csv(here::here("data/TB_notifications_2023-08-21.csv"))
tb_oz <- tb |>
  filter(iso3 == "AUS", between(year, 1997, 2012)) |>
  select(year, contains("new_sp")) |>
  select(-new_sp, -new_sp_m04, -new_sp_m514,
         -new_sp_m014, -new_sp_f014,
         -new_sp_mu, -new_sp_f04, -new_sp_f514,
         -new_sp_fu) |>
  pivot_longer(new_sp_m1524:new_sp_f65, 
              names_to="var", values_to="count") |>
  mutate(var = str_remove(var, "new_sp_")) |>
  mutate(sex = str_sub(var, 1, 1),
         age = str_sub(var, 2, length(var))) |>
  select(-var)


## --------------------------------------------------------
#| label: tb-plot-sex
#| code-fold: true
#| fig-width: 15
#| fig-height: 3
#| out-width: 100%
tb_oz |>
  ggplot(aes(x=year, y=count, fill=sex)) +
    geom_col(position="fill") +
    scale_fill_discrete_divergingx(palette = "Zissou 1") +
    facet_wrap(~age, ncol=6) +
    xlab("") + ylab("proportion")


## --------------------------------------------------------
#| label: tb-plot-age
#| code-fold: true
#| fig-width: 10
#| fig-height: 5
#| out-width: 70%
tb_oz |>
  ggplot(aes(x=year, y=count, fill=age)) +
    geom_col(position="fill") +
    scale_fill_discrete_divergingx(palette = "Zissou 1") +
    facet_wrap(~sex, ncol=2) +
    xlab("") + ylab("proportion")


## --------------------------------------------------------
#| label: tb-plot-year
#| code-fold: true
#| fig-width: 12
#| fig-height: 4
#| out-width: 80%
tb_oz |>
  ggplot(aes(x=year, y=count, fill=age)) +
    geom_col() +
    scale_fill_discrete_divergingx(palette = "Zissou 1") +
    facet_grid(sex~age, scales="free") +
    xlab("") + ylab("count") +
    theme(legend.position = "none")


## --------------------------------------------------------
#| label: read-hexmap-data
#| code-fold: true
#| out-width: 80%
hstudy <- read_csv("https://raw.githubusercontent.com/srkobakian/experiment/master/data/DAT_HexmapPilotData_V1_20191115.csv")
hstudy |>
  filter(trend == "three cities") |>
  ggplot(aes(x=detect)) + geom_bar() + facet_wrap(~type, ncol=2)


## --------------------------------------------------------
#| label: plot-hexmap-results
#| code-fold: true
#| out-width: 90%
hstudy |>
  filter(trend == "three cities") |>
  select(type, replicate, detect) |>
  group_by(type, replicate) |>
  summarise(pdetect = length(detect[detect == 1])/length(detect)) |>
  ggplot(aes(x=type, y=pdetect)) +
    geom_point() +
    geom_line(aes(group=replicate)) +
    ylim(c(0,1)) +
    xlab("") +
    ylab("Proportion detected")


## --------------------------------------------------------
#| label: trade-data
#| include: false
data(EastIndiesTrade, package = "GDAdata")
skimr::skim(EastIndiesTrade)


## --------------------------------------------------------
#| label: trade-plot1
#| echo: false
#| fig-height: 6
#| fig-width: 7.5
#| out-width: 100%
g1 <- ggplot(EastIndiesTrade, aes(Year, Exports)) +
  annotate("rect",
    xmin = 1701, xmax = 1714,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  annotate("rect",
    xmin = 1756, xmax = 1763,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  annotate("rect",
    xmin = 1775, xmax = 1780,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  geom_line(color = "#339933", size = 2) +
  geom_line(aes(Year, Imports), color = "red", size = 2) +
  geom_ribbon(aes(ymin = Exports, ymax = Imports), fill = "gray") +
  labs(y = "<span style='color:#339933'>Export</span>/<span style='color:red'>Import</span>", tag = "(A)") +
  theme(aspect.ratio=0.7, axis.title.y = ggtext::element_markdown())

g2 <- ggplot(EastIndiesTrade, aes(Year, Imports - Exports)) +
  annotate("rect",
    xmin = 1701, xmax = 1714,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  annotate("rect",
    xmin = 1756, xmax = 1763,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  annotate("rect",
    xmin = 1775, xmax = 1780,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  geom_line(size = 2) +
  labs(tag = "(B)") +
  theme(aspect.ratio=0.7)

g3 <- ggplot(EastIndiesTrade, aes(Year, (Imports - Exports) / (Exports + Imports) * 2)) +
  annotate("rect",
    xmin = 1701, xmax = 1714,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  annotate("rect",
    xmin = 1756, xmax = 1763,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  annotate("rect",
    xmin = 1775, xmax = 1780,
    ymin = -Inf, ymax = Inf,
    fill = "red", alpha = 0.3
  ) +
  geom_line(color = "#001a66", size = 2) +
  labs(y = "Relative difference", tag = "(C)") +
  theme(aspect.ratio=0.7)

g1 + g1 + g2 + g3 + plot_layout(ncol=2)


## ----render = knitr::normal_print------------------------
#| label: trade-data
data(EastIndiesTrade, package = "GDAdata")
skimr::skim(EastIndiesTrade)


## --------------------------------------------------------
#| label: trade-plot1
#| echo: true
#| eval: false
# g1 <- ggplot(EastIndiesTrade, aes(Year, Exports)) +
#   annotate("rect",
#     xmin = 1701, xmax = 1714,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   annotate("rect",
#     xmin = 1756, xmax = 1763,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   annotate("rect",
#     xmin = 1775, xmax = 1780,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   geom_line(color = "#339933", size = 2) +
#   geom_line(aes(Year, Imports), color = "red", size = 2) +
#   geom_ribbon(aes(ymin = Exports, ymax = Imports), fill = "gray") +
#   labs(y = "<span style='color:#339933'>Export</span>/<span style='color:red'>Import</span>", tag = "(A)") +
#   theme(aspect.ratio=0.7, axis.title.y = ggtext::element_markdown())
# 
# g2 <- ggplot(EastIndiesTrade, aes(Year, Imports - Exports)) +
#   annotate("rect",
#     xmin = 1701, xmax = 1714,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   annotate("rect",
#     xmin = 1756, xmax = 1763,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   annotate("rect",
#     xmin = 1775, xmax = 1780,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   geom_line(size = 2) +
#   labs(tag = "(B)") +
#   theme(aspect.ratio=0.7)
# 
# g3 <- ggplot(EastIndiesTrade, aes(Year, (Imports - Exports) / (Exports + Imports) * 2)) +
#   annotate("rect",
#     xmin = 1701, xmax = 1714,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   annotate("rect",
#     xmin = 1756, xmax = 1763,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   annotate("rect",
#     xmin = 1775, xmax = 1780,
#     ymin = -Inf, ymax = Inf,
#     fill = "red", alpha = 0.3
#   ) +
#   geom_line(color = "#001a66", size = 2) +
#   labs(y = "Relative difference", tag = "(C)") +
#   theme(aspect.ratio=0.7)
# 
# g1 + g1 + g2 + g3 + plot_layout(ncol=2)


## --------------------------------------------------------
#| label: height-data
#| echo: false
set.seed(744)
df22 <- tibble(
  sex = c(rep("F", 95), 
          rep("M", 82), 
          rep("U", 16)),
  height = c(rnorm(95, 165, 6.9),
             rnorm(82, 175, 7.9),
             rnorm(16, 170, 14.1)))


## --------------------------------------------------------
#| label: height-hist
#| code-fold: true
#| fig-width: 6
#| fig-height: 4
#| out-width: 100%
ggplot(df22, aes(x=height)) +
  geom_histogram(breaks = seq(132.5, 205, 5),
    colour="white") +
  geom_vline(xintercept = 165, colour="#D55E00",
    linewidth=2)


## --------------------------------------------------------
#| label: height-hist-facet
#| code-fold: true
#| fig-width: 10
#| fig-height: 4
#| out-width: 100%
ggplot(df22, aes(x=height)) +
  geom_histogram(breaks = seq(137.5, 205, 5),
    colour="white") +
  geom_vline(xintercept = 165, colour="#D55E00",
    linewidth=2) +
  facet_wrap(~sex, ncol=3, scales="free_y")


## --------------------------------------------------------
#| label: zscore-calc
df22 <- df22 |>
  group_by(sex) |>
  mutate(zscore = (height -
    mean(height))/sd(height))


## --------------------------------------------------------
#| label: zscore-hist
#| echo: false
#| fig-width: 10
#| fig-height: 4
#| out-width: 100%
ggplot(df22, aes(x=zscore)) +
  geom_histogram(breaks = seq(-3.75, 3.75, 0.5),
    colour="white") +
  facet_wrap(~sex, ncol=3, scales="free_y")


## --------------------------------------------------------
#| label: zscore-summary
#| echo: false
df22_smry <- df22 |>
  group_by(sex) |>
  summarise(m = mean(height), 
            s = sd(height))


## --------------------------------------------------------
#| label: anorexia-scatter
#| code-fold: true
#| fig-width: 10
#| fig-height: 4
#| out-width: 100%
data(anorexia, package="MASS")
ggplot(data=anorexia, 
 aes(x=Prewt, y=Postwt, 
	colour=Treat)) + 
 coord_equal() +
 xlim(c(70, 110)) + ylim(c(70, 110)) +
 xlab("Pre-treatment weight (lbs)") +  
 ylab("Post-treatment weight (lbs)") +
 geom_abline(intercept=0, slope=1,  
   colour="grey80", linewidth=1.25) + 
 geom_density2d() + 
 geom_point(size=3) +
 facet_grid(.~Treat) +
 theme(legend.position = "none")


## --------------------------------------------------------
#| label: anorexia-relative
#| code-fold: true
#| fig-width: 10
#| fig-height: 4
#| out-width: 100%
ggplot(data=anorexia, 
  aes(x=Prewt, colour=Treat,
    y=(Postwt-Prewt)/Prewt*100)) + 
  xlab("Pre-treatment weight (lbs)") +  
  ylab("Percent increase in weight") +
  geom_hline(yintercept=0, linewidth=1.25, 
    colour="grey80") + 
  geom_point(size=3) +   
  facet_grid(.~Treat) +
 theme(legend.position = "none")


## --------------------------------------------------------
#| label: ecodata
library(ecotourism)
data(orchids)        # 302,123 rows: one row PER SIGHTING (presence-only)
data(weather)        # daily weather for every day, 2014-2024, per station
data(top_stations)

orchid_stations <- top_stations |>
  filter(organism == "orchids") |>
  pull(ws_id)

# Sightings by day and station
orchid_obs <- orchids |>
  filter(year == 2024) |>
  filter(ws_id %in% orchid_stations) |>
  summarise(count = n(), .by = c(date, ws_id))

# Weather by day and station
weather_2024 <- weather |>
  filter(year == 2024) |>
  filter(ws_id %in% orchid_stations)


## --------------------------------------------------------
#| label: ignore-missings
orchid_date <- orchid_obs |>
  left_join(weather_2024, by = c("ws_id", "date")) |>
  filter(!is.na(prcp)) |>
  mutate(zero_prcp = if_else(prcp < 0.001, "dry", "rain")) 

ggplot(orchid_date, aes(x=zero_prcp, y=count+0.1)) + 
  geom_lv(aes(fill = after_stat(LV))) +
  scale_fill_lv() +
  scale_y_log10() +
  xlab("")


## --------------------------------------------------------
#| label: include-missings1
weather_2024_tsb <- as_tsibble(weather_2024, index = date, key = ws_id)

weather_2024_tsb |> has_gaps()

orchid_obs_tsb <- as_tsibble(orchid_obs, index = date, key = ws_id)

orchid_obs_tsb |> has_gaps()

ggplot(orchid_obs_tsb, aes(x=ws_id, y=date)) +
  geom_point(shape = "|") +
  xlab("") + ylab("") +
  coord_flip() +
  theme(aspect.ratio = 0.5)


## --------------------------------------------------------
#| label: include-missings2
weather_orchid <- weather_2024 |>
  left_join(orchid_obs, by = c("ws_id", "date")) |>
  filter(!is.na(prcp)) |>
  mutate(zero_prcp = if_else(prcp < 0.001, "dry", "rain")) |>
  mutate(count = replace_na(count, 0))

ggplot(weather_orchid, aes(x=zero_prcp, y=count+0.1)) + 
  geom_lv(aes(fill = after_stat(LV))) +
  scale_fill_lv() +
  scale_y_log10() +
  xlab("")


## --------------------------------------------------------
#| label: plot-missings
weather_orchid |>
  filter(month == 10) |>
  ggplot(aes(x=zero_prcp, y=count+0.1)) + 
  geom_lv(aes(fill = after_stat(LV))) +
  scale_fill_lv() +
  scale_y_log10() +
  xlab("")


## --------------------------------------------------------
#| label: with-ci
#| echo: true
#| fig-width: 4
#| fig-height: 6
#| out-width: 60%

set.seed(1134)
county_name_sample <-
  toy_temp |> 
  select(county_name) |>
  distinct() |>
  sample_frac(0.5) |>
  pull(county_name)

toy_temp_eg <- toy_temp |>
  filter(county_name %in% county_name_sample) |>
  group_by(county_name) |>
  summarise(
    mean_temp = mean(recorded_temp),
    se_temp   = sd(recorded_temp) / sqrt(n())
  )

ggplot(toy_temp_eg, aes(x = fct_reorder(county_name, mean_temp), y = mean_temp)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_temp - 2*se_temp,
                     ymax = mean_temp + 2*se_temp), width = 0.2) +
  labs(y = "Mean recorded temp (°C)", x = NULL) +
  coord_flip() +
  theme_minimal()


## --------------------------------------------------------
#| label: with-distribution
#| echo: true
#| fig-width: 4
#| fig-height: 6
#| out-width: 60%
library(ggdibbler)
library(distributional)

# Same summary, but the estimate becomes a DISTRIBUTION, not a number
toy_temp_dist <- toy_temp |>
  filter(county_name %in% county_name_sample) |>
  group_by(county_name) |>
  summarise(
    temp_dist = 
      dist_normal(mu = mean(recorded_temp),
                             sigma = sd(recorded_temp) / sqrt(n()))
  ) |>
  mutate(county_name = fct_reorder(county_name, temp_dist, .fun = mean))

ggplot(toy_temp_dist, 
    aes(x = county_name, 
    y = temp_dist)) +
  geom_point_sample(times = 50, alpha = 0.4) +
  labs(y = "Mean recorded temp (°C)", x = NULL) +
  coord_flip() +
  theme_minimal()


## --------------------------------------------------------
#| label: boot-hstudy
#| echo: false
#| fig-width: 8
#| fig-height: 9
#| out-width: 70%
hstudy_sub <- hstudy |>
  filter(trend == "three cities") |>
  select(id, type, replicate, detect) 

# Function to compute proportions
prop_func <- function(df) {
  df_smry <- df |>
    group_by(type, replicate) |>
    summarise(pdetect = length(detect[detect == 1])/length(detect)) |> 
    ungroup() |>
    pivot_wider(names_from = c(type, replicate),
               values_from = pdetect)
  df_smry
}

nboots <- 100
set.seed(1023)
bsamps <- tibble(samp="0", prop_func(hstudy_sub))
for (i in 1:nboots) {
  samp_id <- sort(sample(unique(hstudy_sub$id),
    replace=TRUE))
  hs_b <- NULL
  for (j in samp_id) {
    x <- hstudy_sub |>
      filter(id == j)
    hs_b <- bind_rows(hs_b, x)
  }
  bsamps <- bind_rows(bsamps,
    tibble(samp=as.character(i), prop_func(hs_b)))
}

bsamps_long <- bsamps |>
  pivot_longer(Geography_9:Hexagons_12, 
    names_to = "treatments", 
    values_to = "pdetect") |> 
  separate(treatments, into=c("type", "replicate"))

ggplot() +
    geom_line(data=filter(bsamps_long, samp != "0"),
      aes(x=type, 
          y=pdetect, 
          group=samp),
     linewidth=0.5, alpha=0.6, colour="grey70") +
    geom_line(data=filter(bsamps_long, 
                     samp == "0"),
      aes(x=type, 
          y=pdetect, 
          group=samp),
     linewidth=2) +
    facet_wrap(~replicate) +
    ylim(c(0,1)) +
    xlab("") +
    ylab("Proportion detected")


## --------------------------------------------------------
#| label: hstudy-count
hstudy |> filter(trend == "three cities") |> count(type, replicate)


## --------------------------------------------------------
#| label: boot-hstudy
#| echo: true
#| eval: false
# hstudy_sub <- hstudy |>
#   filter(trend == "three cities") |>
#   select(id, type, replicate, detect)
# 
# # Function to compute proportions
# prop_func <- function(df) {
#   df_smry <- df |>
#     group_by(type, replicate) |>
#     summarise(pdetect = length(detect[detect == 1])/length(detect)) |>
#     ungroup() |>
#     pivot_wider(names_from = c(type, replicate),
#                values_from = pdetect)
#   df_smry
# }
# 
# nboots <- 100
# set.seed(1023)
# bsamps <- tibble(samp="0", prop_func(hstudy_sub))
# for (i in 1:nboots) {
#   samp_id <- sort(sample(unique(hstudy_sub$id),
#     replace=TRUE))
#   hs_b <- NULL
#   for (j in samp_id) {
#     x <- hstudy_sub |>
#       filter(id == j)
#     hs_b <- bind_rows(hs_b, x)
#   }
#   bsamps <- bind_rows(bsamps,
#     tibble(samp=as.character(i), prop_func(hs_b)))
# }
# 
# bsamps_long <- bsamps |>
#   pivot_longer(Geography_9:Hexagons_12,
#     names_to = "treatments",
#     values_to = "pdetect") |>
#   separate(treatments, into=c("type", "replicate"))
# 
# ggplot() +
#     geom_line(data=filter(bsamps_long, samp != "0"),
#       aes(x=type,
#           y=pdetect,
#           group=samp),
#      linewidth=0.5, alpha=0.6, colour="grey70") +
#     geom_line(data=filter(bsamps_long,
#                      samp == "0"),
#       aes(x=type,
#           y=pdetect,
#           group=samp),
#      linewidth=2) +
#     facet_wrap(~replicate) +
#     ylim(c(0,1)) +
#     xlab("") +
#     ylab("Proportion detected")


## --------------------------------------------------------
#| label: lineup-plot
#| code-fold: true
#| fig-width: 10
#| fig-height: 8
#| out-width: 100%
n_nulls <- 11
set.seed(1110)
lsamps <- tibble(samp="0", prop_func(hstudy_sub))
for (i in 1:n_nulls) {

  hs_b <- hstudy_sub |>
    group_by(id) |>
    mutate(type = sample(type)) |>
    ungroup()
  lsamps <- bind_rows(lsamps,
    tibble(samp=as.character(i), prop_func(hs_b)))
}

lsamps_long <- lsamps |>
  pivot_longer(Geography_9:Hexagons_12, 
    names_to = "treatments", 
    values_to = "pdetect") |> 
  separate(treatments, into=c("type", "replicate"))

lsamps_long |> ggplot() +
    geom_line(aes(x=type, 
          y=pdetect, 
          group=replicate)) +
    facet_wrap(~samp, ncol=4) +
    ylim(c(0,1)) +
    xlab("") +
    ylab("Proportion detected")



## --------------------------------------------------------
#| echo: false
countdown::countdown(6, 44)

