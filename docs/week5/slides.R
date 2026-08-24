## ----include = FALSE, echo=FALSE-------------------------------------------
source("../setup.R")


## ----DT-options, include = FALSE-------------------------------------------
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

## --------------------------------------------------------------------------
#| label: recode-party-names
#| echo: false
recode_party_names <- c(
  "Australian Labor Party (Northern Territory) Branch" = "Australian Labor Party",
  "Labor" = "Australian Labor Party",
  "The Greens (Vic)" = "The Greens",
  "The Greens (Wa)" = "The Greens",
  "Katter's Australian Party (KAP)" = "Katter's Australian Party",
  "Country Liberals (Nt)" = "Country Liberals (NT)"
)


## ----plots, include = FALSE, fig.width = 4, fig.height = 2-----------------
some_df <- tibble(id = 1:200) |>
  mutate(
    x1 = rexp(n()),
    p1 = rbinom(n(), 1, 0.98),
    x2 = p1 * rnorm(n()) + (1 - p1) * runif(n(), 4, 5),
    p2 = rbinom(n(), 1, 0.6),
    x3 = p2 * rnorm(n()) + (1 - p2) * rnorm(n(), 8, 2),
    x4 = p2 * runif(n(), 1, 2) + (1 - p2) * runif(n(), 4, 10),
    x5 = sample(0:10, size = n(), replace = TRUE, prob = c(0.6, rep(0.04, each = 10))),
    x6 = sample(18:30, size = n(), replace = TRUE),
    p3 = rbinom(n(), 1, 0.98),
    x7 = p3 * rnorm(n()) + (1 - p3) * runif(n(), -10, -3)
  )

ggplot(some_df, aes(x1)) +
  geom_histogram(binwidth = 0.3) +
  theme_void() +
  theme(axis.line.x = element_line(color = "black", size = 2))

ggplot(some_df, aes(x2, "")) +
  geom_boxplot(binwidth = 0.3) +
  theme_void() +
  theme(axis.line.x = element_line(color = "black", size = 2))

ggplot(some_df, aes(x3)) +
  geom_density() +
  theme_void() +
  theme(axis.line.x = element_line(color = "black", size = 2))

ggplot(some_df, aes(x = 1, y = x4)) +
  geom_quasirandom() +
  theme_void() +
  theme(axis.line.x = element_line(color = "black", size = 2)) +
  coord_flip()

ggplot(some_df, aes(x5)) +
  geom_bar() +
  theme_void() +
  theme(axis.line.x = element_line(color = "black", size = 2))

ggplot(some_df, aes(x6)) +
  geom_histogram(binwidth = 0.1) +
  theme_void() +
  theme(axis.line.x = element_line(color = "black", size = 2))

ggplot(some_df, aes(x7)) +
  geom_dotplot() +
  theme_void() +
  annotate("rect", xmin = -Inf, xmax = sort(some_df$x7)[3] + 0.5, ymin = 0, ymax = Inf, fill = "red", alpha = 0.3) +
  theme(axis.line.x = element_line(color = "black", size = 2))

## --------------------------------------------------------------------------
#| label: feature-table
#| echo: false
ftable <- tribble(
  ~Feature, ~Example, ~Description,
  "Asymmetry", "", "The distribution is not symmetrical.",
  "Outliers", "", "Some observations are that are far from the rest.",
  "Multimodality", "", 'There are more than one "peak" in the observations.',
  "Gaps", "", "Some continuous interval that are contained within the range but no observations exists.",
  "Heaping", "", "Some values occur unexpectedly often.",
  "Discretized", "", "Only certain values are found, e.g. due to rounding.",
  "Implausible", "", "Values outside of plausible or likely range."
) 
ftable |>
  knitr::kable(escape = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=30, 
                            full_width=FALSE) |>
  kableExtra::column_spec(2, 
    image=spec_image(
      c("images/plots-1.png",
        "images/plots-2.png",
        "images/plots-3.png",
        "images/plots-4.png",
        "images/plots-5.png",
        "images/plots-6.png",
        "images/plots-7.png"), width=250, height=200))


## --------------------------------------------------------------------------
#| label: aus-greens
df1 <- read_csv(here::here("data/HouseFirstPrefsByCandidateByVoteTypeDownload-24310.csv"),
  skip = 1,
  col_types = cols(
    .default = col_character(),
    OrdinaryVotes = col_double(),
    AbsentVotes = col_double(),
    ProvisionalVotes = col_double(),
    PrePollVotes = col_double(),
    PostalVotes = col_double(),
    TotalVotes = col_double(),
    Swing = col_double()
  )
)
tdf3 <- df1 |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)
tdf3 |>
  ggplot(aes(perc_GRN)) +
  geom_histogram(color = "white", 
    fill = "#00843D",
    breaks = seq(0, 50, 2)) +
  labs(
    x = "First preference votes %",
    y = "Count"
  )


## --------------------------------------------------------------------------
#| label: wordle
wordle <- tibble(rows = c(1:6), count = c(0, 13, 128, 204, 147, 49))
ggplot(wordle, aes(x=rows, y=count)) + 
  geom_col(fill = "#e78c45") +
  #scale_x_discrete(breaks=1:6, labels=1:6) +
  xlab("Number of rows to solve") 


## --------------------------------------------------------------------------
#| label: olive-mixed
olive <- read_csv(here::here("data/olive.csv")) |>
  rename(id = `...1`)
olive |> 
  ggplot(aes(x=eicosenoic, y=1)) +
    geom_quasirandom( 
      colour = "#007BA5", 
      alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    ylab("") +
    theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: melb-air
melb_air <- read_csv(here::here("data/melb_air.csv"))
ggplot(melb_air, aes(x = 1, y = so2)) + 
  geom_quasirandom(colour = "#b9ca4a") +
  xlab("") +
  ylab("SO2 concentration (ppm)") +
  theme(axis.text.x = element_blank())


## --------------------------------------------------------------------------
library(nycflights13)
ggplot(flights, aes(x = 1, y = dep_delay)) + 
  geom_boxplot(fill = "#c397d8", alpha = 0.5) +
  xlab("") +
  ylab("Departure delay (mins)") +
  theme(aspect.ratio = 2,
    axis.text.x = element_blank())


## --------------------------------------------------------------------------
#| label: greens-options
#| fig-width: 12
#| fig-height: 4
gp1 <- tdf3 |>
  ggplot(aes(perc_GRN)) +
  geom_histogram(color = "white", fill = "#00843D") +
  labs(
    x = "First preference votes %",
    y = "Count"
  )
gp2 <- tdf3 |>
  ggplot(aes(perc_GRN)) +
  geom_quasirandom(color = "#00843D") +
  labs(
    x = "First preference votes %",
    y = "Count"
  )
gp2 <- tdf3 |>
  ggplot(aes(x = perc_GRN, y = 1)) +
  geom_quasirandom(color = "#00843D") +
  labs(
    x = "First preference votes %",
    y = "Count"
  )
gp3 <- tdf3 |>
  ggplot(aes(x = perc_GRN, y = 1)) +
  geom_boxplot(fill = "#00843D", alpha=0.5) +
  labs(
    x = "First preference votes %",
    y = "Count"
  )
gp1 + gp2 + gp3 + plot_layout(ncol=3)


## --------------------------------------------------------------------------
#| label: oil-options
#| fig-width: 10
#| fig-height: 4
op1 <- olive |> 
  ggplot(aes(x=eicosenoic, y=1)) +
    geom_quasirandom( 
      colour = "#007BA5", 
      alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    ylab("") +
    theme(axis.text.y = element_blank())
op2 <- olive |> 
  ggplot(aes(x=eicosenoic)) +
    geom_density( 
      colour = "#007BA5", 
      fill = "#007BA5", 
      alpha = 0.5) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    ylab("") +
    theme(axis.text.y = element_blank())
op1 + op2 + plot_layout(ncol=2)


## --------------------------------------------------------------------------
#| label: air-options
#| fig-width: 12
#| fig-height: 4
ap1 <- ggplot(melb_air, aes(x = 1, y = so2)) + 
  geom_quasirandom(colour = "#b9ca4a") +
  xlab("") +
  ylab("SO2 concentration (ppm)") +
  theme(axis.text.x = element_blank())
ap2 <- ggplot(melb_air, aes(x = 1, y = so2)) + 
  geom_violin(
    quantiles = c(0.25, 0.5, 0.75),
    quantile.colour = "white",
    colour = "#b9ca4a", fill = "#b9ca4a", alpha=0.5) +
  xlab("") +
  ylab("SO2 concentration (ppm)") +
  theme(axis.text.x = element_blank())
ap3 <- ggplot(melb_air, aes(x = so2, y = 1)) + 
  stat_halfeye(fill = "#b9ca4a", alpha=0.5) +  
  stat_dots(side = "bottom",
    colour = "#b9ca4a") +
  ylab("") +
  xlab("SO2 concentration (ppm)") +
  theme(axis.text.y = element_blank())
ap1 + ap2 + ap3 + plot_layout(ncol=3)


## --------------------------------------------------------------------------
#| label: flights-refocus
#| fig-width: 12
#| fig-height: 4
fp1 <- ggplot(flights, aes(x = 1, y = dep_delay)) + 
  geom_boxplot(fill = "#c397d8", alpha = 0.5) +
  xlab("") +
  ylab("Departure delay (mins)") +
  theme(aspect.ratio = 2,
    axis.text.x = element_blank())
fp2 <- flights |>
  filter(between(dep_delay, -15, 15)) |>
  ggplot(aes(x = 1, y = dep_delay)) + 
  geom_boxplot(fill = "#c397d8", alpha = 0.5) +
  xlab("") +
  ylab("Departure delay (mins)") +
  theme(aspect.ratio = 2,
    axis.text.x = element_blank())
fp3 <- flights |>
  filter(dep_delay > 15) |>
  ggplot(aes(x = dep_delay)) + 
  geom_density(colour = "#c397d8", 
    fill = "#c397d8", 
    alpha = 0.5) +
  scale_x_log10() +
  xlab("") +
  ylab("Departure delay (mins)") +
  theme(aspect.ratio = 0.8)
fp4 <- flights |>
  filter(dep_delay > 15) |>
  ggplot(aes(x = dep_delay, y = 1)) + 
  geom_quasirandom(colour = "#c397d8", 
    alpha = 0.5) +
  scale_x_log10() +
  xlab("") +
  ylab("Departure delay (mins)") +
  theme(aspect.ratio = 0.8, 
    axis.text.y = element_blank(),
    axis.title.y = element_blank())
fp1 + fp2 + fp3 + fp4 + plot_layout(ncol=4)


## --------------------------------------------------------------------------
#| label: movies-data
#| code-fold: false
data(movies, package = "ggplot2movies")


## --------------------------------------------------------------------------
#| echo: false
countdown::countdown(7, 35)


## --------------------------------------------------------------------------
#| label: analyse-movies
#| echo: false
#| eval: false
# ggplot(movies, aes(length)) +
#   geom_histogram(color = "white") +
#   labs(x = "Length of movie (minutes)", y = "Frequency") +
#   theme(aspect.ratio = 0.6)
# 
# ggplot(movies, aes(length)) +
#   geom_histogram(color = "white") +
#   labs(x = "Length of movie (minutes)", y = "Frequency") +
#   scale_x_log10() +
#   theme(aspect.ratio = 0.6)
# 
# movies |>
#   filter(length < 180) |>
#   ggplot(aes(length)) +
#   geom_histogram(binwidth = 1, colour = "black") +
#   labs(x = "Length of movie (minutes)", y = "Frequency")


## --------------------------------------------------------------------------
#| label: greens-sim1
#| fig-width: 10
#| fig-height: 5
#| out-width: 80%
set.seed(241)
l <- lineup(null_dist("perc_GRN", "exp"), tdf3, n=12, pos=1)
ggplot(l, 
       aes(x=perc_GRN)) +
  geom_histogram(color = "white", 
    fill = "#00843D", 
    breaks = seq(0, 50, 2)) +
  xlim(c(0, 50)) +
  facet_wrap(~.sample, ncol=6) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank())


## --------------------------------------------------------------------------
#| label: greens-sim2
#| fig-width: 10
#| fig-height: 5
#| out-width: 80%
library(fitdistrplus)
fit <- fitdist(tdf3$perc_GRN / 100, "beta")

set.seed(446)
l2 <- tdf3 |>
  select(perc_GRN) |>
  mutate(.sample = 1)
for (i in 1:11) {
  x <- rbeta(nrow(tdf3), fit$estimate[1], fit$estimate[2])*100
  d <- tibble(perc_GRN = x,
     .sample = i+1)
  l2 <- bind_rows(l2, d)
}
ggplot(l2, 
       aes(x=perc_GRN)) +
  geom_histogram(color = "white", 
    fill = "#00843D", 
    breaks = seq(0, 50, 2)) +
  xlim(c(0, 50)) +
  facet_wrap(~.sample, ncol=6, scale="free_y") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank())


## --------------------------------------------------------------------------
#| label: wordle-sim
#| fig-width: 10
#| fig-height: 5
#| out-width: 80%
wordle_uncount <- uncount(wordle, count)
p <- mean(wordle_uncount$rows)/6
set.seed(501)
wl <- wordle |>
  mutate(.sample = 1)
for (i in 1:11) {
  x <- rbinom(nrow(wordle_uncount), 6, p)
  d <- tibble(rows = x) |>
    count(rows) |>
    rename(count = n) |>
    mutate(.sample = i+1)
  wl <- bind_rows(wl, d)
}
ggplot(wl, 
       aes(x=rows, y=count)) +
  geom_col(fill = "#e78c45") +
  facet_wrap(~.sample, ncol=6) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank())


## --------------------------------------------------------------------------
#| label: robust-mean
#| fig-height: 3.5
#| fig-width: 4
#| fig-show: hide
#| echo: false
set.seed(1)
df_list <- imap(c("rnorm(100)", "rt(50, 20)", "rexp(40)", "runif(150, 0, 1)", "rexp(100, 2)", "p <- rbinom(200, 1, 0.5) ; p * rgamma(200, 3) + (1 - p) * rnorm(200, 8, 1)"), ~ {
  data.frame(id = .y, x = eval(parse(text = .x)))
})
iwalk(df_list, ~ {
  g <- ggplot(.x, aes(x)) +
    geom_histogram(color = "white", 
                   bins=15,
                   fill = "grey80") +
    geom_vline(
      xintercept = mean(.x$x),
      color = "#D81B60", size = 1.3
    ) +
    geom_vline(
      xintercept = median(.x$x),
      color = "#1E88E5", size = 1.3
    ) +
    geom_vline(
      xintercept = mean(.x$x, trim = 0.2),
      color = "#FFC107", size = 1.3
    ) +
    geom_vline(
      xintercept = psych::winsor.mean(.x$x, trim = 0.2),
      color = "#004D40", linewidth = 1.3
    ) +
    theme_void() +
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle(.y) +
    theme(
      plot.title = element_text(face = "bold", size = 30),
      axis.line.x = element_line(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.ticks.length.x = unit(1, "mm")
    )
  print(g)
})


## --------------------------------------------------------------------------
#| label: robust-plots
#| results: asis
#| cache: false
#| echo: false
cat(sprintf("<img src='images/robust-mean-%d.png' class='ba pl2' height ='210px'/>", 1:length(df_list)))


## --------------------------------------------------------------------------
#| label: central-measure-table
#| echo: false
df_list |>
  bind_rows() |>
  group_by(id) |>
  summarise(
    mean = mean(x),
    median = median(x),
    trimmed = mean(x, trim = 0.2),
    winsor = psych::winsor.mean(x, trim = 0.2)
  ) |>
  knitr::kable(col.names = c("Plot", "Mean", "Median", "Trimmed Mean", "Winsorized Mean"), digits = 3, escape = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=24, full_width = FALSE) |>
  kableExtra::column_spec(2, color = "#D81B60") |>
  kableExtra::column_spec(3, color = "#1E88E5") |>
  kableExtra::column_spec(4, color = "#FFC107") |>
  kableExtra::column_spec(5, color = "#004D40")


## --------------------------------------------------------------------------
#| label: robust-dispersion
#| fig-height: 3.5
#| fig-width: 4
#| fig-show: hide
#| echo: false
set.seed(1)
df_list <- imap(c("rnorm(100)", "rt(50, 20)", "rexp(40)", "runif(150, 0, 1)", "rexp(100, 2)", "p <- rbinom(200, 1, 0.5) ; p * rgamma(200, 3) + (1 - p) * rnorm(200, 8, 1)"), ~ {
  data.frame(id = .y, x = eval(parse(text = .x)))
})
iwalk(df_list, ~ {
  g <- ggplot(.x, aes(x)) +
    geom_histogram(color = "white", 
                   bins=15,
                   fill = "grey80") +
    geom_vline(
      xintercept = c(mean(.x$x)-sd(.x$x),
                     mean(.x$x)+sd(.x$x)),
      color = "#D81B60", size = 1.3
    ) +
    geom_vline(
      xintercept = quantile(.x$x, probs=c(0.25, 0.75)),
      color = "#1E88E5", size = 1.3
    ) +
    geom_vline(
      xintercept = c(mean(.x$x)-mad(.x$x),
                     mean(.x$x)+mad(.x$x)),
      color = "#FFC107", size = 1.3
    ) +
    theme_void() +
    scale_y_continuous(expand = c(0, 0)) +
    ggtitle(.y) +
    theme(
      plot.title = element_text(face = "bold", size = 30),
      axis.line.x = element_line(color = "black"),
      axis.text.x = element_text(color = "black"),
      axis.ticks.length.x = unit(1, "mm")
    )
  print(g)
})


## --------------------------------------------------------------------------
#| label: robust-disp-plots
#| results: asis
#| cache: false
#| echo: false
cat(sprintf("<img src='images/robust-dispersion-%d.png' class='ba pl2' height ='210px'/>", 1:length(df_list)))


## --------------------------------------------------------------------------
#| label: central-dispersion-table
#| echo: false
df_list |>
  bind_rows() |>
  group_by(id) |>
  summarise(
    sd = sd(x),
    iqr = IQR(x),
    mad = mad(x),
    skew = moments::skewness(x),
    kurtosis = moments::kurtosis(x)
  ) |>
  knitr::kable(col.names = c("Plot", "SD", "IQR", "MAD", "Skewness", "Kurtosis"), digits = 3, escape = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=24, full_width = FALSE) |>
  kableExtra::column_spec(2, color = "#D81B60") |>
  kableExtra::column_spec(3, color = "#1E88E5") |>
  kableExtra::column_spec(4, color = "#FFC107") |>
  kableExtra::add_header_above(c("", "Measure of dispersion" = 3, " " = 2))


## --------------------------------------------------------------------------
#| label: aus-election-table3
#| echo: false
tdf3 |>
 summarise(
    mean = mean(perc_GRN),
    median = median(perc_GRN),
    sd = sd(perc_GRN),
    mad = mad(perc_GRN),
    iqr = IQR(perc_GRN),
    skewness = moments::skewness(perc_GRN),
    kurtosis = moments::kurtosis(perc_GRN)
  ) |>
  knitr::kable(col.names = c("Mean", "Median", "SD", "MAD", "IQR", "Skewness", "Kurtosis"), digits = 3) |>
  kableExtra::kable_classic() |>
  kableExtra::add_header_above(c(" ", "% of first preference for the Greens" = 5, " ")) 


## --------------------------------------------------------------------------
#| label: aus-election-plot-dist
#| echo: false
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
ggplot(tdf3, aes(x = perc_GRN, y = 1)) +
  geom_quasirandom(colour = "#00843D", alpha = 0.5) +
  ylab("") +
  xlab("First preference votes %") +
  theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: aus-election-table3
#| echo: true
#| eval: false
# tdf3 |>
#  summarise(
#     mean = mean(perc_GRN),
#     median = median(perc_GRN),
#     sd = sd(perc_GRN),
#     mad = mad(perc_GRN),
#     iqr = IQR(perc_GRN),
#     skewness = moments::skewness(perc_GRN),
#     kurtosis = moments::kurtosis(perc_GRN)
#   ) |>
#   knitr::kable(col.names = c("Mean", "Median", "SD", "MAD", "IQR", "Skewness", "Kurtosis"), digits = 3) |>
#   kableExtra::kable_classic() |>
#   kableExtra::add_header_above(c(" ", "% of first preference for the Greens" = 5, " "))


## --------------------------------------------------------------------------
#| label: olive-table1
#| echo: false
olive |>
  summarise(
    mean = mean(eicosenoic),
    median = median(eicosenoic),
    sd = sd(eicosenoic),
    mad = mad(eicosenoic),
    iqr = IQR(eicosenoic),
    skewness = moments::skewness(eicosenoic),
    kurtosis = moments::kurtosis(eicosenoic)
  ) |>
  knitr::kable(col.names = c("Mean", "Median", "SD", "MAD", "IQR", "Skewness", "Kurtosis"), digits = 3) |>
  kableExtra::kable_classic() |>
  kableExtra::add_header_above(c("% composition of eicosenoic acid" = 7))


## --------------------------------------------------------------------------
#| label: olive-plot-dist
#| echo: false
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
ggplot(olive, aes(x = eicosenoic, y = 1)) +
  geom_quasirandom(colour = "#007BA5", alpha = 0.5) +
  ylab("") +
  xlab("Eicosenoic acid %") +
  theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: olive-table1
#| echo: true
#| eval: false
# olive |>
#   summarise(
#     mean = mean(eicosenoic),
#     median = median(eicosenoic),
#     sd = sd(eicosenoic),
#     mad = mad(eicosenoic),
#     iqr = IQR(eicosenoic),
#     skewness = moments::skewness(eicosenoic),
#     kurtosis = moments::kurtosis(eicosenoic)
#   ) |>
#   knitr::kable(col.names = c("Mean", "Median", "SD", "MAD", "IQR", "Skewness", "Kurtosis"), digits = 3) |>
#   kableExtra::kable_classic() |>
#   kableExtra::add_header_above(c("% composition of eicosenoic acid" = 7))


## --------------------------------------------------------------------------
#| label: air-table1
#| echo: false
melb_air |>
  summarise(
    mean = mean(so2, na.rm = TRUE),
    median = median(so2, na.rm = TRUE),
    sd = sd(so2, na.rm = TRUE),
    mad = mad(so2, na.rm = TRUE),
    iqr = IQR(so2, na.rm = TRUE),
    skewness = moments::skewness(so2, na.rm = TRUE),
    kurtosis = moments::kurtosis(so2, na.rm = TRUE)
  ) |>
  knitr::kable(col.names = c("Mean", "Median", "SD", "MAD", "IQR", "Skewness", "Kurtosis"), digits = 5) |>
  kableExtra::kable_classic() |>
  kableExtra::add_header_above(c("SO2 concentration (ppm)" = 7))


## --------------------------------------------------------------------------
#| label: air-plot-dist
#| echo: false
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
ggplot(melb_air, aes(x = so2, y = 1)) +
  geom_quasirandom(colour = "#b9ca4a", alpha = 0.5) +
  ylab("") +
  xlab("SO2 concentration (ppm)") +
  theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: air-table1
#| echo: true
#| eval: false
# melb_air |>
#   summarise(
#     mean = mean(so2, na.rm = TRUE),
#     median = median(so2, na.rm = TRUE),
#     sd = sd(so2, na.rm = TRUE),
#     mad = mad(so2, na.rm = TRUE),
#     iqr = IQR(so2, na.rm = TRUE),
#     skewness = moments::skewness(so2, na.rm = TRUE),
#     kurtosis = moments::kurtosis(so2, na.rm = TRUE)
#   ) |>
#   knitr::kable(col.names = c("Mean", "Median", "SD", "MAD", "IQR", "Skewness", "Kurtosis"), digits = 5) |>
#   kableExtra::kable_classic() |>
#   kableExtra::add_header_above(c("SO2 concentration (ppm)" = 7))


## --------------------------------------------------------------------------
#| label: simulate
#| fig-width: 6
#| fig-height: 3
#| out-width: 80%
# Estimate the parameters without the outlier
est_r <- fitdist(tdf3$perc_GRN[tdf3$perc_GRN < 40]/100, "beta")
# Check fit
# ggplot(tdf3, aes(sample=perc_GRN)) + stat_qq(distribution = stats::qexp, dparams = est_r) + stat_qq_line(distribution = stats::qexp, dparams = est_r)
set.seed(912)
samp <- matrix(rbeta(n=151*100, 
  shape1=est_r$estimate[1], 
  shape2=est_r$estimate[2])*100, ncol=100, byrow=TRUE)
samp_max <- apply(samp, 2, max)
samp_max_df <- tibble(m = samp_max)
ggplot(samp_max_df, aes(x=m)) +
  geom_histogram(binwidth=2.5, fill="grey60", 
    colour="white") +
  xlim(c(0, 60)) +
  geom_vline(xintercept=
    tdf3$perc_GRN[tdf3$perc_GRN > 40], colour="#D93F00") +
  annotate("text", x=42, y=13, label="observed", colour="#D93F00") +
  xlab("Simulated maxima") +
  theme(aspect.ratio = 0.5)


## --------------------------------------------------------------------------
#| echo: false
#| eval: true
set.seed(558)
x <- sort(round(runif(8, 1, 10), 0))
x


## --------------------------------------------------------------------------
#| eval: true
sort(sample(x, replace=TRUE))
sort(sample(x, replace=TRUE))


## --------------------------------------------------------------------------
#| echo: false
library(boot)

# Function to compute median
median_func <- function(data, indices) {
  return(median(data[indices]))
}

# Bootstrap function
bootstrap_median_ci <- function(data, R = 1000, conf_level = 0.95) {
  # Perform bootstrap
  boot_result <- boot(data = data, statistic = median_func, R = R)
  
  # Compute confidence interval
  ci <- boot.ci(boot_result, type = "perc", conf = conf_level)
  
  # Return results
  list(
    original_median = median(data),
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5],
    conf_level = conf_level
  )
}

# Example usage:
set.seed(601)  # For reproducibility
example_data <- rexp(100, rate=0.1)
result <- bootstrap_median_ci(example_data)

print(paste("Median:", round(result$original_median, 2)))
print(paste("95% CI: (", round(result$ci_lower, 2), ",", round(result$ci_upper, 2), ")"))



## --------------------------------------------------------------------------
#| echo: false
countdown::countdown(5, 15)


## --------------------------------------------------------------------------
#| label: pm25-activity
#| echo: false
#| eval: false

# ggplot(melb_air, aes(x = pm25, y = 1)) +
#   geom_quasirandom(colour = "#b9ca4a", alpha = 0.5) +
#   ylab("") +
#   xlab("SO2 concentration (ppm)") +
#   theme(axis.text.y = element_blank())
# 
# melb_air |>
#   summarise(
#     mean = mean(pm25, na.rm = TRUE),
#     median = median(pm25, na.rm = TRUE),
#     sd = sd(pm25, na.rm = TRUE),
#     mad = mad(pm25, na.rm = TRUE),
#     IQR = IQR(pm25, na.rm = TRUE))
# 
# pm25 <- melb_air$pm25[!is.na(melb_air$pm25)]
# 
# # Bootstrap CI for the median
# median_func <- function(data, indices)
#   median(data[indices], na.rm = TRUE)
# boot_result <- boot(data = pm25, statistic = median_func, R = 1000)
# boot.ci(boot_result, type = "perc")


## --------------------------------------------------------------------------
#| label: aus-election-plot2
#| fig-height: 6
#| out-width: 80%
tdf3 |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(perc_GRN, State)) +
  geom_boxplot(varwidth = TRUE) +
  labs(
    x = "First preference votes %",
    y = "Count",
    title = "Greens party"
  )


## --------------------------------------------------------------------------
#| label: aus-election-data3
#| echo: true
#| eval: false
# NA


## --------------------------------------------------------------------------
#| label: aus-election-plot2
#| echo: true
#| eval: false
# tdf3 |>
#   mutate(State = fct_reorder(State, perc_GRN)) |>
#   ggplot(aes(perc_GRN, State)) +
#   geom_boxplot(varwidth = TRUE) +
#   labs(
#     x = "First preference votes %",
#     y = "Count",
#     title = "Greens party"
#   )


## --------------------------------------------------------------------------
#| label: aus-election-2019-plot3
#| fig-height: 6
#| out-width: 80%
tdf3 |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(perc_GRN, State)) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
  labs(
    x = "First preference votes %",
    y = "State",
    title = "Greens party"
  )


## --------------------------------------------------------------------------
#| label: aus-election-data3
#| echo: true
#| eval: false
# NA


## ----aus-election-2019-plot3, echo = TRUE, eval = FALSE--------------------
# tdf3 |>
#   mutate(State = fct_reorder(State, perc_GRN)) |>
#   ggplot(aes(perc_GRN, State)) +
#   ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
#   labs(
#     x = "First preference votes %",
#     y = "State",
#     title = "Greens party"
#   )


## --------------------------------------------------------------------------
#| label: boxcox-price
#| echo: false
#| fig-height: 3
#| out-width: 90%
df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
  col_types = cols(
    .default = col_character(),
    Rooms = col_double(),
    Price = col_double(),
    Date = col_date(format = "%d/%m/%Y"),
    Propertycount = col_double(),
    Distance = col_double()
  )
)
bc <- MASS::boxcox(df2$Price[!is.na(df2$Price)] ~ 1, plotit = FALSE)
lambda_opt <- bc$x[which.max(bc$y)]
tibble(lambda = bc$x, loglik = bc$y) |>
  ggplot(aes(x = lambda, y = loglik)) +
  geom_line(colour = "#007BA5") +
  geom_vline(xintercept = lambda_opt, linetype = "dashed", colour = "#D93F00") +
  labs(x = expression(lambda), y = "Log-likelihood") +
  theme(aspect.ratio = 0.5)


## --------------------------------------------------------------------------
#| label: pm25-boxcox-orig
pm25 <- melb_air$pm25[!is.na(melb_air$pm25)]
pm25_shift <- pm25 + 0.1
pm25p1 <- ggplot(tibble(pm25 = pm25), aes(x = pm25, y = 1)) +
  geom_quasirandom(colour = "#b9ca4a", alpha = 0.5) +
  ylab("") + xlab("PM2.5 (µg/m³)") +
  theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: pm25-boxcox-profile
#| fig-height: 2.5
#| fig-width: 3
#| out-width: 60%
bc_pm25 <- MASS::boxcox(pm25_shift ~ 1, plotit = FALSE)
lambda_pm25 <- bc_pm25$x[which.max(bc_pm25$y)]
tibble(lambda = bc_pm25$x, loglik = bc_pm25$y) |>
  ggplot(aes(x = lambda, y = loglik)) +
  geom_line(colour = "#b9ca4a") +
  geom_vline(xintercept = lambda_pm25, linetype = "dashed", colour = "#D93F00") +
  labs(x = expression(lambda), y = "Log-likelihood") +
  theme(aspect.ratio = 0.4)


## --------------------------------------------------------------------------
#| label: pm25-boxcox-transformed
#| fig-height: 4
#| fig-width: 8
#| out-width: 80%
pm25_bc <- (pm25_shift^lambda_pm25 - 1) / lambda_pm25
pm25p2 <- ggplot(tibble(pm25_bc = pm25_bc), aes(x = pm25_bc, y = 1)) +
  geom_quasirandom(colour = "#b9ca4a", alpha = 0.5) +
  ylab("") + xlab("Box-Cox transformed PM2.5") +
  theme(axis.text.y = element_blank())
pm25p1  + pm25p2 + plot_layout(ncol=2)


## --------------------------------------------------------------------------
#| label: pm25-boxcox-data
#| echo: true
#| eval: false

# NA


## --------------------------------------------------------------------------
#| label: pm25-boxcox-orig
#| echo: true
#| eval: false
# pm25 <- melb_air$pm25[!is.na(melb_air$pm25)]
# pm25_shift <- pm25 + 0.1
# pm25p1 <- ggplot(tibble(pm25 = pm25), aes(x = pm25, y = 1)) +
#   geom_quasirandom(colour = "#b9ca4a", alpha = 0.5) +
#   ylab("") + xlab("PM2.5 (µg/m³)") +
#   theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: pm25-boxcox-profile
#| echo: true
#| eval: false
# bc_pm25 <- MASS::boxcox(pm25_shift ~ 1, plotit = FALSE)
# lambda_pm25 <- bc_pm25$x[which.max(bc_pm25$y)]
# tibble(lambda = bc_pm25$x, loglik = bc_pm25$y) |>
#   ggplot(aes(x = lambda, y = loglik)) +
#   geom_line(colour = "#b9ca4a") +
#   geom_vline(xintercept = lambda_pm25, linetype = "dashed", colour = "#D93F00") +
#   labs(x = expression(lambda), y = "Log-likelihood") +
#   theme(aspect.ratio = 0.4)


## --------------------------------------------------------------------------
#| label: pm25-boxcox-transformed
#| echo: true
#| eval: false
# pm25_bc <- (pm25_shift^lambda_pm25 - 1) / lambda_pm25
# pm25p2 <- ggplot(tibble(pm25_bc = pm25_bc), aes(x = pm25_bc, y = 1)) +
#   geom_quasirandom(colour = "#b9ca4a", alpha = 0.5) +
#   ylab("") + xlab("Box-Cox transformed PM2.5") +
#   theme(axis.text.y = element_blank())
# pm25p1  + pm25p2 + plot_layout(ncol=2)


## --------------------------------------------------------------------------
#| label: olive-clusters
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
ggplot(olive, aes(x = eicosenoic, y = 1)) +
  geom_quasirandom(colour = "#007BA5", alpha = 0.5) +
  ylab("") +
  xlab("Eicosenoic acid %") +
  theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: olive-region
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
ggplot(olive, aes(x = eicosenoic, y = 1)) +
  geom_quasirandom(colour = "#007BA5", alpha = 0.5) +
  facet_wrap(~region, ncol=3) +
  ylab("") +
  xlab("Eicosenoic acid %") +
  theme(axis.text.y = element_blank())


## --------------------------------------------------------------------------
#| label: olive-mixture
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
#olive <- read_csv("http://ggobi.org/book/data/olive.csv") |>
#  rename(id = `...1`)
olive <- read_csv(here::here("data/olive.csv")) |>
  rename(id = `...1`)
op1 <- olive |>
  ggplot(aes(x=arachidic, y=1)) +
    geom_quasirandom() +
    theme(aspect.ratio=0.5,
    axis.text.y = element_blank(),
    axis.title.y = element_blank())
op2 <- olive |>
  ggplot(aes(x=arachidic)) +
    geom_histogram(binwidth=2.5, fill="grey60", colour="white") +
    theme(aspect.ratio=0.5)
op1 + op2 + plot_layout(ncol=1)


## --------------------------------------------------------------------------
#| label: olive-mixture-area
#| echo: false
#| fig-width: 10
#| fig-height: 8
#| out-width: 100%
olive |>
  ggplot(aes(x=arachidic, y=1)) +
    geom_quasirandom() +
    scale_x_continuous("", breaks=seq(0, 110, 10)) +
    facet_wrap(~area, ncol=3, scales="free") +
    theme(aspect.ratio=0.5, 
      axis.text.y=element_blank(),
      axis.title.y=element_blank())


## --------------------------------------------------------------------------
#| echo: false
countdown::countdown(2, 01)


## --------------------------------------------------------------------------
#| label: melb-house-data
#| echo: false
df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
  col_types = cols(
    .default = col_character(),
    Rooms = col_double(),
    Price = col_double(),
    Date = col_date(format = "%d/%m/%Y"),
    Propertycount = col_double(),
    Distance = col_double()
  )
)


## --------------------------------------------------------------------------
#| label: melb-house-data-display
df2 |>
  head(20) |>
  select(Suburb, Rooms, Type, Price, Date) |>
  mutate(
    Price = scales::comma(Price),
    Type = fct_recode(Type,
      "Home" = "h",
      "Townhouse" = "t",
      "Unit" = "u"
    )
  ) |>
  knitr::kable(
    col.names = c("Suburb", "Rooms", "Type", "Price ($)", "Date"),
    align = "lrlr"
  ) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size = 24,
    full_width=FALSE)


## --------------------------------------------------------------------------
#| label: melb-house-plot-miss
#| echo: false
#| eval: true
#| fig-height: 6
#| out-width: 80%

df2 |>
  select(Suburb, Rooms, Type, Price, Date) |>
  arrange(Suburb, Date) |>
  visdat::vis_miss()


## --------------------------------------------------------------------------
#| label: melb-house-plot-room-miss
#| echo: false
#| eval: true
#| fig-width: 6
#| fig-height: 4
#| out-width: 60%

df2 |>
  mutate(miss = ifelse(is.na(Price), 
    "Missing", "Recorded")) |>
  count(Rooms, miss) |>
  filter(Rooms < 8) |>
  group_by(miss) |>
  mutate(perc = n / sum(n) * 100) |>
  ggplot(aes(as.factor(Rooms), perc, fill = miss)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d(begin=0.3, end=0.7) +
    labs(x = "Rooms", y = "Percentage", fill = "Price") +
    theme(aspect.ratio = 0.8)


## --------------------------------------------------------------------------
#| label: melb-house-data
#| echo: true
#| eval: false
# df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
#   col_types = cols(
#     .default = col_character(),
#     Rooms = col_double(),
#     Price = col_double(),
#     Date = col_date(format = "%d/%m/%Y"),
#     Propertycount = col_double(),
#     Distance = col_double()
#   )
# )


## ----melb-house-data-skim, echo = TRUE, render = knitr::normal_print-------
skimr::skim(df2)


## --------------------------------------------------------------------------
#| label: melb-house-plot-miss
#| echo: true
#| eval: false
# df2 |>
#   select(Suburb, Rooms, Type, Price, Date) |>
#   arrange(Suburb, Date) |>
#   visdat::vis_miss()


## --------------------------------------------------------------------------
#| label: melb-house-plot-room-miss
#| echo: true
#| eval: false
# df2 |>
#   mutate(miss = ifelse(is.na(Price),
#     "Missing", "Recorded")) |>
#   count(Rooms, miss) |>
#   filter(Rooms < 8) |>
#   group_by(miss) |>
#   mutate(perc = n / sum(n) * 100) |>
#   ggplot(aes(as.factor(Rooms), perc, fill = miss)) +
#     geom_col(position = "dodge") +
#     scale_fill_viridis_d(begin=0.3, end=0.7) +
#     labs(x = "Rooms", y = "Percentage", fill = "Price") +
#     theme(aspect.ratio = 0.8)


## --------------------------------------------------------------------------
#| label: melb-house-lineup
#| echo: false
#| eval: true
#| fig-width: 8
#| fig-height: 4
#| out-width: 70%

library(nullabor)
df2_d <- df2 |>
  mutate(miss = ifelse(is.na(Price), "Missing", "Recorded")) |>
  select(Rooms, miss) |>
  filter(Rooms < 8)
df2_l <- lineup(null_permute("miss"), df2_d, n=10, pos=7) 
df2_l_agg <- df2_l |>
  group_by(.sample) |>
  count(Rooms, miss) |>
  ungroup() |>
  group_by(miss) |>
  mutate(perc = n / sum(n) * 100) |>
  mutate(Rooms = as.factor(Rooms))
ggplot(df2_l_agg, aes(x=Rooms, y=perc, fill = miss)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(begin=0.3, end=0.7) +
  facet_wrap(~.sample, ncol=5) +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank())


## --------------------------------------------------------------------------
#| label: melb-house-lineup
#| echo: true
#| eval: false

# library(nullabor)
# df2_d <- df2 |>
#   mutate(miss = ifelse(is.na(Price), "Missing", "Recorded")) |>
#   select(Rooms, miss) |>
#   filter(Rooms < 8)
# df2_l <- lineup(null_permute("miss"), df2_d, n=10, pos=7)
# df2_l_agg <- df2_l |>
#   group_by(.sample) |>
#   count(Rooms, miss) |>
#   ungroup() |>
#   group_by(miss) |>
#   mutate(perc = n / sum(n) * 100) |>
#   mutate(Rooms = as.factor(Rooms))
# ggplot(df2_l_agg, aes(x=Rooms, y=perc, fill = miss)) +
#   geom_col(position = "dodge") +
#   scale_fill_viridis_d(begin=0.3, end=0.7) +
#   facet_wrap(~.sample, ncol=5) +
#   theme(legend.position = "none",
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         panel.grid.major.x = element_blank())


## --------------------------------------------------------------------------
#| label: melb-house-price-plot1
#| echo: false
#| eval: true

df2 |>
  ggplot(aes(Price / 1e6)) +
  geom_histogram(color = "white") +
  labs(
    x = "Price (mil)",
    y = "Count"
  )


## --------------------------------------------------------------------------
#| label: melb-house-data
#| echo: true
#| eval: false

# df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
#   col_types = cols(
#     .default = col_character(),
#     Rooms = col_double(),
#     Price = col_double(),
#     Date = col_date(format = "%d/%m/%Y"),
#     Propertycount = col_double(),
#     Distance = col_double()
#   )
# )


## --------------------------------------------------------------------------
#| label: melb-house-price-plot1
#| echo: true
#| eval: false

# df2 |>
#   ggplot(aes(Price / 1e6)) +
#   geom_histogram(color = "white") +
#   labs(
#     x = "Price (mil)",
#     y = "Count"
#   )


## --------------------------------------------------------------------------
#| label: melb-house-price-plot2
#| echo: false
#| eval: true

df2 |>
  ggplot(aes(Price / 1e6)) +
  geom_histogram(color = "white") +
  labs(
    x = "Price (mil)",
    y = "Count"
  ) +
  scale_x_log10()


## --------------------------------------------------------------------------
#| label: melb-house-data
#| echo: true
#| eval: false

# df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
#   col_types = cols(
#     .default = col_character(),
#     Rooms = col_double(),
#     Price = col_double(),
#     Date = col_date(format = "%d/%m/%Y"),
#     Propertycount = col_double(),
#     Distance = col_double()
#   )
# )


## --------------------------------------------------------------------------
#| label: melb-house-price-plot2
#| echo: true
#| eval: false
# df2 |>
#   ggplot(aes(Price / 1e6)) +
#   geom_histogram(color = "white") +
#   labs(
#     x = "Price (mil)",
#     y = "Count"
#   ) +
#   scale_x_log10()


## --------------------------------------------------------------------------
#| label: melb-house-stats
#| echo: false
#| eval: true

df2 |>
  filter(!is.na(Price)) |>
  summarise(
    Mean = scales::dollar(mean(Price)),
    Median = scales::dollar(median(Price)),
    `Trimmed Mean` = scales::dollar(mean(Price, trim = 0.2)),
    `Winsorised Mean` = scales::dollar(psych::winsor.mean(Price))
  ) |>
  knitr::kable(align = "r") |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(full_width=FALSE)


## --------------------------------------------------------------------------
#| label: melb-house-stats-tranformed
#| echo: false
#| eval: true

df2 |>
  filter(!is.na(Price)) |>
  mutate(lPrice = log10(Price)) |>
  summarise(
    Mean = scales::dollar(10^mean(lPrice)),
    Median = scales::dollar(10^median(lPrice)),
    `Trimmed Mean` = scales::dollar(10^mean(lPrice, trim = 0.2)),
    `Winsorised Mean` = scales::dollar(10^psych::winsor.mean(lPrice))
  ) |>
  knitr::kable(align = "r") |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(full_width=FALSE)


## --------------------------------------------------------------------------
#| label: melb-house-stats
#| echo: true
#| eval: false
# df2 |>
#   filter(!is.na(Price)) |>
#   summarise(
#     Mean = scales::dollar(mean(Price)),
#     Median = scales::dollar(median(Price)),
#     `Trimmed Mean` = scales::dollar(mean(Price, trim = 0.2)),
#     `Winsorised Mean` = scales::dollar(psych::winsor.mean(Price))
#   ) |>
#   knitr::kable(align = "r") |>
#   kableExtra::kable_classic() |>
#   kableExtra::kable_styling(full_width=FALSE)


## --------------------------------------------------------------------------
#| label: melb-house-stats-tranformed
#| echo: true
#| eval: false
# df2 |>
#   filter(!is.na(Price)) |>
#   mutate(lPrice = log10(Price)) |>
#   summarise(
#     Mean = scales::dollar(10^mean(lPrice)),
#     Median = scales::dollar(10^median(lPrice)),
#     `Trimmed Mean` = scales::dollar(10^mean(lPrice, trim = 0.2)),
#     `Winsorised Mean` = scales::dollar(10^psych::winsor.mean(lPrice))
#   ) |>
#   knitr::kable(align = "r") |>
#   kableExtra::kable_classic() |>
#   kableExtra::kable_styling(full_width=FALSE)


## ----factors, echo = TRUE--------------------------------------------------
data <- c(2, 2, 1, 1, 3, 3, 3, 1)
factor(data)


## ----factor-labels, echo = TRUE--------------------------------------------
factor(data, labels = c("I", "II", "III"))


## ----factor-input, echo = TRUE---------------------------------------------
# numerical input are ordered in increasing order 
factor(c(1, 3, 10))
# character input are ordered by first char, alphabetically 
factor(c("1", "3", "10"))
# you can specify order of levels explicitly 
factor(c("1", "3", "10"),
  levels = c("1", "3", "10")
)


## --------------------------------------------------------------------------
#| code-fold: false
#| eval: false
# stats::reorder(factor, value, mean)
# forcats::fct_reorder(factor, value, median)
# forcats::fct_reorder2(factor, value1, value2, func)


## --------------------------------------------------------------------------
#| echo: false
# https://www.who.int/teams/global-tuberculosis-programme/data
tb <- read_csv(here::here("data/TB_notifications_2023-08-21.csv"))
tb_tot <- tb |> 
  rowwise() |>
  mutate(count = sum(c(new_sp, new_sn, new_su, new_ep, new_oth), na.rm=T)) |>
  select(country, iso3, year, count) |>
  ungroup()
tb_oz <- tb_tot |>
  filter(iso3 == "AUS")


## --------------------------------------------------------------------------
#| eval: true
#| code-fold: true
options(digits=2)
tb_oz |>
  filter(year >= 2000) |>
  mutate(p = count/sum(count),
         pct = p*100, 
         odds = count/count[year==2000]) |>
  print(n=100)


## --------------------------------------------------------------------------
#| label: cat-plots
#| fig-width: 7
#| fig-height: 6
#| out-width: 100%
wordle2 <- wordle |>
  mutate(attempts = factor(rows))

p_bar <- ggplot(wordle2, aes(x = attempts, y = count)) +
  geom_col(aes(fill = attempts)) +
  scale_fill_viridis_d() +
  labs(x = "Number of attempts", y = "Count") +
  theme(legend.position = "none") +
  ggtitle("Bar chart")

p_pie <- ggplot(wordle2, aes(x = "", y = count, fill = attempts)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  scale_fill_viridis_d() +
  labs(fill = "Attempts", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Pie chart")

p_rose <- ggplot(wordle2, aes(x = attempts, y = count, fill = attempts)) +
  geom_col(colour = "white") +
  coord_polar(theta = "x") +
  scale_fill_viridis_d() +
  labs(x = NULL, y = "Count", fill = "Attempts") +
  theme(axis.text.y = element_blank(), legend.position = "none") +
  ggtitle("Rose plot")

p_spine <- ggplot(wordle2, aes(x = "", y = count, fill = attempts)) +
  geom_col(position = "fill", colour = "white") +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(x = NULL, y = "Proportion", fill = "Attempts") +
  theme(axis.text.y = element_blank()) +
  ggtitle("Spine plot")

p_bar + p_pie + p_rose + p_spine + plot_layout(ncol=2)


## --------------------------------------------------------------------------
#| echo: false
countdown::countdown(5, 52)


## --------------------------------------------------------------------------
#| label: housing-missing
#| echo: false
#| eval: false
# # This code is repeated from chunk melb-house-lineup
# df2_d <- df2 |>
#   mutate(miss = ifelse(is.na(Price), "Missing", "Recorded")) |>
#   select(Rooms, miss) |>
#   filter(Rooms < 8)
# df2_l <- lineup(null_permute("miss"), df2_d, n=10, pos=7)
# df2_l_agg <- df2_l |>
#   group_by(.sample) |>
#   count(Rooms, miss) |>
#   ungroup() |>
#   group_by(miss) |>
#   mutate(perc = n / sum(n) * 100) |>
#   mutate(Rooms = as.factor(Rooms))
# library(ggmosaic)
# ggplot(df2_l_agg) +
#   geom_mosaic(aes(x = product(Rooms), fill = miss, weight = perc)) +
#   facet_wrap(~.sample, ncol=5) +
#   theme(legend.position = "none",
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         panel.grid.major.x = element_blank())


## --------------------------------------------------------------------------
#| label: price-impute
df2 <- df2 |>
  mutate(lPrice = log10(Price),
         price_miss = ifelse(is.na(Price), "yes", "no"))

df2 <- df2 |>
  filter(Rooms < 20) # remove one extreme
df2_fit <- lm(lPrice~Rooms, df2) 
coefs <- tidy(df2_fit)
fitstats <- glance(df2_fit)

set.seed(1003)  
df2_nomiss <- df2 |>
  filter(price_miss == "no")
df2_miss <- df2 |>
  filter(price_miss == "yes")
df2_miss <- df2_miss |>
  rowwise() |>
  mutate(lPrice = 
    coefs$estimate[1]+coefs$estimate[2]*Rooms +
    rnorm(1, 0, fitstats$sigma)) |>
  mutate(Price = ifelse(price_miss == "yes", 10^lPrice, Price))
df2_nomiss <- bind_rows(df2_nomiss, df2_miss)
df2_nomiss <- df2_nomiss |>
  mutate(price_miss = factor(price_miss, levels = c("yes", "no")))


## --------------------------------------------------------------------------
#| label: price-impute-plot1
#| fig-width: 9
#| fig-height: 4
#| out-width: 100%

mp1 <- ggplot(df2, aes(x=Rooms, y=lPrice)) + 
  geom_miss_point() 

mp2 <- ggplot(df2_nomiss, aes(x=Rooms, 
                               y=lPrice, 
                               colour=price_miss)) +
  geom_jitter(width=0.5, alpha=0.1)

mp3 <- df2_nomiss |>
  ggplot() +
  geom_histogram(aes(x=Price / 1e6, 
                     y=after_stat(density)), 
                 color = "white") +
  facet_wrap(~price_miss, ncol=2, 
              scales="free_y") +
  labs(
    x = "Price (mil)",
    y = "Count"
  ) +
  scale_x_log10()
mp1 + mp2 + plot_layout(ncol=2)


## --------------------------------------------------------------------------
#| label: price-impute-plot2
#| fig-width: 12
#| fig-height: 4
#| out-width: 60%
mp3 


## --------------------------------------------------------------------------
#| label: cat-impute
#| echo: true
tb_oz_age <- tb |> 
  filter(iso3 == "AUS", year == 2012) |>
  select(contains("new_sp_f")) |>
  select(-new_sp_f04, -new_sp_f514, -new_sp_f014) |>
  pivot_longer(new_sp_f1524:new_sp_fu, 
    names_to="age", 
    values_to="count") |>
  mutate(age = str_remove(age, "new_sp_f"))
# Add some missing count
tb_oz_age$count[7] <- 12
tb_oz_age
tb_oz_age_long <- tb_oz_age |>
  uncount(count) |>
  mutate(age = ifelse(age == "u", NA, age))
set.seed(153)
fill_miss <- rbinom(tb_oz_age$count[7], size=5,
  prob=tb_oz_age$count[1:6]/sum(tb_oz_age$count[1:6]))+1
tb_oz_age_impute <- tb_oz_age 
for (i in 1:length(fill_miss)) 
  tb_oz_age_impute$count[fill_miss[i]] <-
    tb_oz_age_impute$count[fill_miss[i]] + 1
fill_miss
tb_oz_age_impute

