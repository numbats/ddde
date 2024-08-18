## ----include = FALSE, echo=FALSE------------------------------------------
source("../setup.R")


## ----DT-options, include = FALSE------------------------------------------
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

## -------------------------------------------------------------------------
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


## ----plots, include = FALSE, fig.width = 4, fig.height = 2----------------
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

## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| label: example-plot
#| echo: false
#| fig-height: 1.3
ggplot(some_df, aes(x1)) +
  geom_histogram(binwidth = 0.2, fill = "pink", color = "black") +
  theme_void() +
  theme(axis.line.x = element_line(color = "black", size = 2))


## -------------------------------------------------------------------------
#| label: aus-election-data
#| echo: false
#| eval: true

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


## -------------------------------------------------------------------------
#| label: aus-election-table
#| echo: false
#| eval: true
df1 |>
  DT::datatable(
    rownames = FALSE,
    escape = FALSE,
    width = "1350px",
    options = table_options(
      scrollY = "450px",
      title = "Australian Federal Election 2019",
      csv = "aus-election-2019"
    ),
    extensions = "Buttons",
    elementId = "tab1A",
    callback = toggle_select
  ) |>
  DT::formatRound(c("OrdinaryVotes", "AbsentVotes", "ProvisionalVotes", "PrePollVotes", "PostalVotes", "TotalVotes"), digits = 0) |>
  DT::formatRound("Swing", digits = 2)


## -------------------------------------------------------------------------
#| label: aus-election-data1
#| echo: false
#| eval: true
tdf1 <- df1 |>
  filter(Elected == "Y") |>
  mutate(
    PartyNm = str_to_title(PartyNm),
    PartyNm = recode(PartyNm, !!!recode_party_names)
  ) |>
  count(PartyNm, sort = TRUE) |>
  slice(2:4, 1, 8, 6, 7, 5)


## -------------------------------------------------------------------------
#| label: aus-election-table1
#| echo: false
#| eval: true
data.frame(PartyNm = "Coalition", n = sum(tdf1$n[1:3])) |>
  rbind(tdf1) |>
  knitr::kable(col.names = c("Party", "# of seats")) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(    
    full_width = FALSE,
    font_size = 24
  ) |>
  kableExtra::add_indent(2:4) |>
  kableExtra::row_spec(2:4, color = "#C8C8C8") 


## ----aus-election-data-skim, echo = TRUE, render = knitr::normal_print----
skimr::skim(df1)


## -------------------------------------------------------------------------
#| label: aus-election-data
#| echo: true
#| eval: false
## df1 <- read_csv(here::here("data/HouseFirstPrefsByCandidateByVoteTypeDownload-24310.csv"),
##   skip = 1,
##   col_types = cols(
##     .default = col_character(),
##     OrdinaryVotes = col_double(),
##     AbsentVotes = col_double(),
##     ProvisionalVotes = col_double(),
##     PrePollVotes = col_double(),
##     PostalVotes = col_double(),
##     TotalVotes = col_double(),
##     Swing = col_double()
##   )
## )


## -------------------------------------------------------------------------
#| label: recode-party-names
#| echo: true
#| eval: false
## recode_party_names <- c(
##   "Australian Labor Party (Northern Territory) Branch" = "Australian Labor Party",
##   "Labor" = "Australian Labor Party",
##   "The Greens (Vic)" = "The Greens",
##   "The Greens (Wa)" = "The Greens",
##   "Katter's Australian Party (KAP)" = "Katter's Australian Party",
##   "Country Liberals (Nt)" = "Country Liberals (NT)"
## )


## -------------------------------------------------------------------------
#| label: aus-election-data1
#| echo: true
#| eval: false
## tdf1 <- df1 |>
##   filter(Elected == "Y") |>
##   mutate(
##     PartyNm = str_to_title(PartyNm),
##     PartyNm = recode(PartyNm, !!!recode_party_names)
##   ) |>
##   count(PartyNm, sort = TRUE) |>
##   slice(2:4, 1, 8, 6, 7, 5)


## ----aus-election-table1, echo = TRUE, eval = FALSE-----------------------
## data.frame(PartyNm = "Coalition", n = sum(tdf1$n[1:3])) |>
##   rbind(tdf1) |>
##   knitr::kable(col.names = c("Party", "# of seats")) |>
##   kableExtra::kable_classic() |>
##   kableExtra::kable_styling(
##     full_width = FALSE,
##     font_size = 24
##   ) |>
##   kableExtra::add_indent(2:4) |>
##   kableExtra::row_spec(2:4, color = "#C8C8C8")


## -------------------------------------------------------------------------
#| label: aus-election-data2
#| echo: FALSE
#| eval: TRUE
tdf2 <- df1 |>
  mutate(
    PartyNm = str_to_title(PartyNm),
    PartyNm = recode(PartyNm, !!!recode_party_names)
  ) |>
  count(PartyNm, sort = TRUE)


## ----aus-election-table2--------------------------------------------------
#| label: aus-election-table2
#| echo: FALSE
#| eval: TRUE
tdf2 |>
  DT::datatable(
    rownames = FALSE,
    escape = FALSE,
    width = "900px",
    options = table_options(
      scrollY = "400px",
      title = "Australian Federal Election 2019 - Party Distribution",
      csv = "aus-election-2019-party-dist"
    ),
    elementId = "tab1B",
    colnames = c("Party", "# of electorates"),
    callback = toggle_select
  )


## -------------------------------------------------------------------------
#| label: aus-election-data2
#| echo: TRUE
#| eval: FALSE
## tdf2 <- df1 |>
##   mutate(
##     PartyNm = str_to_title(PartyNm),
##     PartyNm = recode(PartyNm, !!!recode_party_names)
##   ) |>
##   count(PartyNm, sort = TRUE)


## ----aus-election-table2, echo = TRUE, eval = FALSE-----------------------
## tdf2 |>
##   DT::datatable(
##     rownames = FALSE,
##     escape = FALSE,
##     width = "900px",
##     options = table_options(
##       scrollY = "400px",
##       title = "Australian Federal Election 2019 - Party Distribution",
##       csv = "aus-election-2019-party-dist"
##     ),
##     elementId = "tab1B",
##     colnames = c("Party", "# of electorates"),
##     callback = toggle_select
##   )


## -------------------------------------------------------------------------
#| label: aus-election-data3
#| echo: false
#| eval: true

tdf3 <- df1 |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)


## -------------------------------------------------------------------------
#| label: aus-election-plot1
#| echo: false
#| eval: true
#| fig-height: 10
#| fig-width: 10
#| out-width: 100%
tdf3 |>
  ggplot(aes(perc_GRN)) +
  geom_histogram(color = "white", fill = "#00843D") +
  labs(
    x = "First preference votes %",
    y = "Count",
    title = "Greens party"
  )


## ----aus-election-data3-skim, echo = TRUE, render = knitr::normal_print----
skimr::skim(tdf3)


## -------------------------------------------------------------------------
#| label: aus-election-data3
#| echo: true
#| eval: false
## tdf3 <- df1 |>
##   group_by(DivisionID) |>
##   summarise(
##     DivisionNm = unique(DivisionNm),
##     State = unique(StateAb),
##     votes_GRN = TotalVotes[which(PartyAb == "GRN")],
##     votes_total = sum(TotalVotes)
##   ) |>
##   mutate(perc_GRN = votes_GRN / votes_total * 100)


## -------------------------------------------------------------------------
#| label: aus-election-plot1
#| echo: true
#| eval: false
## tdf3 |>
##   ggplot(aes(perc_GRN)) +
##   geom_histogram(color = "white", fill = "#00843D") +
##   labs(
##     x = "First preference votes %",
##     y = "Count",
##     title = "Greens party"
##   )


## -------------------------------------------------------------------------
#| label: aus-election-plot1
#| echo: false
tdf3 |>
  ggplot(aes(perc_GRN)) +
  geom_histogram(color = "white", fill = "#00843D") +
  labs(
    x = "First preference votes %",
    y = "Count",
    title = "Greens party"
  )


## ----eval=FALSE, echo=TRUE------------------------------------------------
## ggplot(data, aes(x=var1)) +
##   geom_histogram()


## ----eval=FALSE, echo=TRUE------------------------------------------------
## # Symmetric, unimodal, bell-shaped
## null_dist("var1", "norm")
## null_dist("var1", "cauchy")
## null_dist("var1", "t")
## 
## # Skewed right
## null_dist("var1", "exp")
## null_dist("var1", "chisq")
## null_dist("var1", "gamma")
## 
## # Constant
## null_dist("var1", "uniform")
## 


## -------------------------------------------------------------------------
#| label: votes-lineup
#| eval: true
#| echo: false
#| fig-width: 10
#| fig-height: 4
#| out-width: 100%
library(nullabor)
set.seed(241)
ggplot(lineup(null_dist("perc_GRN", "exp"), tdf3, n=10),
       aes(x=perc_GRN)) +
  geom_histogram(color = "white", fill = "#00843D", bins = 30) +
  facet_wrap(~.sample, ncol=5, scales="free") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank())


## -------------------------------------------------------------------------
#| label: votes-lineup
#| eval: false
#| echo: true
## library(nullabor)
## set.seed(241)
## ggplot(lineup(null_dist("perc_GRN", "exp"), tdf3, n=10),
##        aes(x=perc_GRN)) +
##   geom_histogram(color = "white", fill = "#00843D", bins = 30) +
##   facet_wrap(~.sample, ncol=5, scales="free") +
##   theme(axis.text = element_blank(),
##         axis.title = element_blank(),
##         panel.grid.major = element_blank())


## -------------------------------------------------------------------------
#| label: aus-election-table3
#| echo: false
tdf3 |>
  group_by(State) |>
  summarise(
    mean = mean(perc_GRN),
    median = median(perc_GRN),
    sd = sd(perc_GRN),
    iqr = IQR(perc_GRN),
    skewness = moments::skewness(perc_GRN),
    kurtosis = moments::kurtosis(perc_GRN)
  ) |>
  arrange(desc(mean)) |>
  rbind(data.frame(
    State = "National",
    mean = mean(tdf3$perc_GRN),
    median = median(tdf3$perc_GRN),
    sd = sd(tdf3$perc_GRN),
    iqr = IQR(tdf3$perc_GRN),
    skewness = moments::skewness(tdf3$perc_GRN),
    kurtosis = moments::kurtosis(tdf3$perc_GRN)
  )) |>
  knitr::kable(col.names = c("State", "Mean", "Median", "SD", "IQR", "Skewness", "Kurtosis"), digits = 3) |>
  kableExtra::kable_classic() |>
  kableExtra::add_header_above(c(" ", "% of first preference for the Greens" = 4, " " = 2)) |>
  kableExtra::row_spec(9, extra_css = "border-top: 2px solid black;")


## -------------------------------------------------------------------------
#| label: aus-election-data3
#| echo: true
#| eval: false

## tdf3 <- df1 |>
##   group_by(DivisionID) |>
##   summarise(
##     DivisionNm = unique(DivisionNm),
##     State = unique(StateAb),
##     votes_GRN = TotalVotes[which(PartyAb == "GRN")],
##     votes_total = sum(TotalVotes)
##   ) |>
##   mutate(perc_GRN = votes_GRN / votes_total * 100)


## -------------------------------------------------------------------------
#| label: aus-election-table3
#| echo: true
#| eval: false
## tdf3 |>
##   group_by(State) |>
##   summarise(
##     mean = mean(perc_GRN),
##     median = median(perc_GRN),
##     sd = sd(perc_GRN),
##     iqr = IQR(perc_GRN),
##     skewness = moments::skewness(perc_GRN),
##     kurtosis = moments::kurtosis(perc_GRN)
##   ) |>
##   arrange(desc(mean)) |>
##   rbind(data.frame(
##     State = "National",
##     mean = mean(tdf3$perc_GRN),
##     median = median(tdf3$perc_GRN),
##     sd = sd(tdf3$perc_GRN),
##     iqr = IQR(tdf3$perc_GRN),
##     skewness = moments::skewness(tdf3$perc_GRN),
##     kurtosis = moments::kurtosis(tdf3$perc_GRN)
##   )) |>
##   knitr::kable(col.names = c("State", "Mean", "Median", "SD", "IQR", "Skewness", "Kurtosis"), digits = 3) |>
##   kableExtra::kable_classic() |>
##   kableExtra::add_header_above(c(" ", "% of first preference for the Greens" = 4, " " = 2)) |>
##   kableExtra::row_spec(9, extra_css = "border-top: 2px solid black;")


## -------------------------------------------------------------------------
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


## ----results = "asis", cache = FALSE--------------------------------------
#| label: robust-plots
#| results: asis
#| cache: false
#| echo: false
cat(sprintf("<img src='images/robust-mean-%d.png' class='ba pl2' height ='210px'/>", 1:length(df_list)))


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
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


## ----results = "asis", cache = FALSE--------------------------------------
#| label: robust-disp-plots
#| results: asis
#| cache: false
#| echo: false
cat(sprintf("<img src='images/robust-dispersion-%d.png' class='ba pl2' height ='210px'/>", 1:length(df_list)))


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| label: simulate
#| echo: false
#| fig-width: 6
#| fig-height: 3
#| out-width: 80%
est_r <- fitdistr(tdf3$perc_GRN[tdf3$perc_GRN < 40], "exponential")$estimate
# Check fit
# ggplot(tdf3, aes(sample=perc_GRN)) + stat_qq(distribution = stats::qexp, dparams = est_r) + stat_qq_line(distribution = stats::qexp, dparams = est_r)
samp <- matrix(rexp(n=151*100, rate=est_r), ncol=100, byrow=TRUE)
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


## -------------------------------------------------------------------------
#| echo: false
#| eval: true
set.seed(558)
x <- sort(round(runif(8, 1, 10), 0))
x


## -------------------------------------------------------------------------
#| eval: true
sort(sample(x, replace=TRUE))
sort(sample(x, replace=TRUE))


## -------------------------------------------------------------------------
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



## -------------------------------------------------------------------------
#| label: aus-election-plot2
#| fig-height: 9
#| fig-width: 9
#| echo: false
tdf3 |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(perc_GRN, State)) +
  geom_boxplot(varwidth = TRUE) +
  labs(
    x = "First preference votes %",
    y = "Count",
    title = "Greens party"
  )


## -------------------------------------------------------------------------
#| label: aus-election-data3
#| echo: true
#| eval: false
## tdf3 <- df1 |>
##   group_by(DivisionID) |>
##   summarise(
##     DivisionNm = unique(DivisionNm),
##     State = unique(StateAb),
##     votes_GRN = TotalVotes[which(PartyAb == "GRN")],
##     votes_total = sum(TotalVotes)
##   ) |>
##   mutate(perc_GRN = votes_GRN / votes_total * 100)


## -------------------------------------------------------------------------
#| label: aus-election-plot2
#| echo: true
#| eval: false
## tdf3 |>
##   mutate(State = fct_reorder(State, perc_GRN)) |>
##   ggplot(aes(perc_GRN, State)) +
##   geom_boxplot(varwidth = TRUE) +
##   labs(
##     x = "First preference votes %",
##     y = "Count",
##     title = "Greens party"
##   )


## -------------------------------------------------------------------------
#| label: aus-election-plot2
#| echo: false
tdf3 |>
  mutate(State = fct_reorder(State, perc_GRN)) |>
  ggplot(aes(perc_GRN, State)) +
  geom_boxplot(varwidth = TRUE) +
  labs(
    x = "First preference votes %",
    y = "Count",
    title = "Greens party"
  )


## -------------------------------------------------------------------------
#| label: annotated-boxplot
#| echo: false
#| fig-height: 3
fn <- quantile(mtcars$wt, c(0, 0.25, 0.5, 0.75, 1), type = 7)
iqr <- fn[4] - fn[2]
ggplot(mtcars, aes(wt, "")) +
  geom_boxplot(
    size = 1.4, width = 0.2, outlier.color = "#006DAE",
    outlier.size = 3, outlier.fill = "#006DAE"
  ) +
  geom_dotplot(binwidth = 0.1, fill = "gray") +
  annotate("line", x = fn[c(2, 4)], y = 1.3, size = 1.3, color = "#C8008F") +
  annotate("segment",
    x = fn[c(2, 4, 3)], xend = fn[c(2, 4, 3)],
    y = c(1.25, 1.25, 0.75), yend = c(1.35, 1.35, 0.9), size = 1.3, color = "#C8008F"
  ) +
  annotate("text", x = c(mean(fn[c(2, 4)]), fn[3], fn[4] + 1.5 * iqr), y = c(1.6, 0.7, 1.9), label = c("IQR", "median", "fence"), size = 10, color = "#C8008F") +
  # some reason vline didn't work
  annotate("segment", x = c(fn[4] + 1.5 * iqr, fn[2] - 1.5 * iqr), xend = c(fn[4] + 1.5 * iqr, fn[2] - 1.5 * iqr), y = -Inf, yend = Inf, linetype = "dashed") +
  theme_void()


## -------------------------------------------------------------------------
#| label: aus-election-2019-plot3
#| echo: false
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


## -------------------------------------------------------------------------
#| label: aus-election-data3
#| echo: true
#| eval: false
## tdf3 <- df1 |>
##   group_by(DivisionID) |>
##   summarise(
##     DivisionNm = unique(DivisionNm),
##     State = unique(StateAb),
##     votes_GRN = TotalVotes[which(PartyAb == "GRN")],
##     votes_total = sum(TotalVotes)
##   ) |>
##   mutate(perc_GRN = votes_GRN / votes_total * 100)


## ----aus-election-2019-plot3, echo = TRUE, eval = FALSE-------------------
## tdf3 |>
##   mutate(State = fct_reorder(State, perc_GRN)) |>
##   ggplot(aes(perc_GRN, State)) +
##   ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
##   labs(
##     x = "First preference votes %",
##     y = "State",
##     title = "Greens party"
##   )


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| label: melb-house-data-display
#| echo: false
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


## -------------------------------------------------------------------------
#| label: melb-house-plot-miss
#| echo: false
#| eval: true
#| fig-height: 6
#| out-width: 80%

df2 |>
  select(Suburb, Rooms, Type, Price, Date) |>
  arrange(Suburb, Date) |>
  visdat::vis_miss()


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| label: melb-house-data
#| echo: true
#| eval: false
## df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
##   col_types = cols(
##     .default = col_character(),
##     Rooms = col_double(),
##     Price = col_double(),
##     Date = col_date(format = "%d/%m/%Y"),
##     Propertycount = col_double(),
##     Distance = col_double()
##   )
## )


## ----melb-house-data-skim, echo = TRUE, render = knitr::normal_print------
skimr::skim(df2)


## -------------------------------------------------------------------------
#| label: melb-house-plot-miss
#| echo: true
#| eval: false
## df2 |>
##   select(Suburb, Rooms, Type, Price, Date) |>
##   arrange(Suburb, Date) |>
##   visdat::vis_miss()


## -------------------------------------------------------------------------
#| label: melb-house-plot-room-miss
#| echo: true
#| eval: false
## df2 |>
##   mutate(miss = ifelse(is.na(Price),
##     "Missing", "Recorded")) |>
##   count(Rooms, miss) |>
##   filter(Rooms < 8) |>
##   group_by(miss) |>
##   mutate(perc = n / sum(n) * 100) |>
##   ggplot(aes(as.factor(Rooms), perc, fill = miss)) +
##     geom_col(position = "dodge") +
##     scale_fill_viridis_d(begin=0.3, end=0.7) +
##     labs(x = "Rooms", y = "Percentage", fill = "Price") +
##     theme(aspect.ratio = 0.8)


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| label: melb-house-lineup
#| echo: true
#| eval: false

## library(nullabor)
## df2_d <- df2 |>
##   mutate(miss = ifelse(is.na(Price), "Missing", "Recorded")) |>
##   select(Rooms, miss) |>
##   filter(Rooms < 8)
## df2_l <- lineup(null_permute("miss"), df2_d, n=10, pos=7)
## df2_l_agg <- df2_l |>
##   group_by(.sample) |>
##   count(Rooms, miss) |>
##   ungroup() |>
##   group_by(miss) |>
##   mutate(perc = n / sum(n) * 100) |>
##   mutate(Rooms = as.factor(Rooms))
## ggplot(df2_l_agg, aes(x=Rooms, y=perc, fill = miss)) +
##   geom_col(position = "dodge") +
##   scale_fill_viridis_d(begin=0.3, end=0.7) +
##   facet_wrap(~.sample, ncol=5) +
##   theme(legend.position = "none",
##         axis.text = element_blank(),
##         axis.title = element_blank(),
##         panel.grid.major.x = element_blank())


## ----melb-house-price-plot1-----------------------------------------------
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


## -------------------------------------------------------------------------
#| label: melb-house-data
#| echo: true
#| eval: false

## df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
##   col_types = cols(
##     .default = col_character(),
##     Rooms = col_double(),
##     Price = col_double(),
##     Date = col_date(format = "%d/%m/%Y"),
##     Propertycount = col_double(),
##     Distance = col_double()
##   )
## )


## -------------------------------------------------------------------------
#| label: melb-house-price-plot1
#| echo: true
#| eval: false

## df2 |>
##   ggplot(aes(Price / 1e6)) +
##   geom_histogram(color = "white") +
##   labs(
##     x = "Price (mil)",
##     y = "Count"
##   )


## ----melb-house-price-plot2-----------------------------------------------
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


## -------------------------------------------------------------------------
#| label: melb-house-data
#| echo: true
#| eval: false

## df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
##   col_types = cols(
##     .default = col_character(),
##     Rooms = col_double(),
##     Price = col_double(),
##     Date = col_date(format = "%d/%m/%Y"),
##     Propertycount = col_double(),
##     Distance = col_double()
##   )
## )


## -------------------------------------------------------------------------
#| label: melb-house-price-plot2
#| echo: true
#| eval: false
## df2 |>
##   ggplot(aes(Price / 1e6)) +
##   geom_histogram(color = "white") +
##   labs(
##     x = "Price (mil)",
##     y = "Count"
##   ) +
##   scale_x_log10()


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| label: melb-house-stats
#| echo: true
#| eval: false
## df2 |>
##   filter(!is.na(Price)) |>
##   summarise(
##     Mean = scales::dollar(mean(Price)),
##     Median = scales::dollar(median(Price)),
##     `Trimmed Mean` = scales::dollar(mean(Price, trim = 0.2)),
##     `Winsorised Mean` = scales::dollar(psych::winsor.mean(Price))
##   ) |>
##   knitr::kable(align = "r") |>
##   kableExtra::kable_classic() |>
##   kableExtra::kable_styling(full_width=FALSE)


## -------------------------------------------------------------------------
#| label: melb-house-stats-tranformed
#| echo: true
#| eval: false
## df2 |>
##   filter(!is.na(Price)) |>
##   mutate(lPrice = log10(Price)) |>
##   summarise(
##     Mean = scales::dollar(10^mean(lPrice)),
##     Median = scales::dollar(10^median(lPrice)),
##     `Trimmed Mean` = scales::dollar(10^mean(lPrice, trim = 0.2)),
##     `Winsorised Mean` = scales::dollar(10^psych::winsor.mean(lPrice))
##   ) |>
##   knitr::kable(align = "r") |>
##   kableExtra::kable_classic() |>
##   kableExtra::kable_styling(full_width=FALSE)


## -------------------------------------------------------------------------
#| label: transform
#| echo: false
#| out-width: 80%
set.seed(419)
d <- tibble(raw = sqrt(runif(365))) |>
  mutate(transf = raw^2)
scale2 <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, TRUE)
d <- d |>
  mutate_at(c("raw","transf"), scale2)
d_long <- d |>
  pivot_longer(raw:`transf`, names_to = "gp", values_to = "value")
transf_p <- ggplot(d_long, aes(x=value)) +
  geom_density(bw=0.25, fill="#FFC107") +
  theme(aspect.ratio=0.5, 
        plot.title.position = "panel",
        axis.text = element_blank(),
        axis.title = element_blank()) +
  transition_states(
    gp,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  ggtitle("{closest_state}")
transf_p


## -------------------------------------------------------------------------
#| label: quantile
#| echo: false
#| fig-width: 9
#| fig-height: 3
#| out-width: 100%
library(bestNormalize)
set.seed(448)
d2 <- tibble(x1 = sort(c(rexp(65)+3, -rexp(40)-3, rnorm(73)))) |>
  mutate(x1 = (x1 - mean(x1))/sd(x1)) |>
  mutate(x2 = orderNorm(x1)$x.t)
d2 <- d2 |>
  rename(raw = x1, transf = x2)
d2_p1 <- ggplot(d2, aes(sample=raw)) +
  stat_qq_line() + stat_qq()
d2_p2 <- ggplot(d2, aes(x=raw)) +
  geom_density()
d2_p3 <- ggplot(d2, aes(x=transf)) +
  geom_density()
d2_p1 + d2_p2 + d2_p3 + plot_layout(ncol=3)


## -------------------------------------------------------------------------
#| label: melb-house-by-room
#| echo: false
#| eval: true
#| fig-height: 6
#| out-width: 80%

df2 |>
  filter(Rooms < 8) |>
  ggplot(aes(x=as.factor(Rooms), y=Price / 1e6, )) +
  ggbeeswarm::geom_quasirandom(varwidth=TRUE, alpha=0.3) +
  scale_y_log10() +
  labs(y = "Price (mil)", x = "# of Rooms")


## -------------------------------------------------------------------------
#| label: melb-house-data
#| echo: true
#| eval: false

## df2 <- read_csv(here::here("data/MELBOURNE_HOUSE_PRICES_LESS.csv"),
##   col_types = cols(
##     .default = col_character(),
##     Rooms = col_double(),
##     Price = col_double(),
##     Date = col_date(format = "%d/%m/%Y"),
##     Propertycount = col_double(),
##     Distance = col_double()
##   )
## )


## -------------------------------------------------------------------------
#| label: melb-house-by-room
#| echo: true
#| eval: false

## df2 |>
##   filter(Rooms < 8) |>
##   ggplot(aes(x=as.factor(Rooms), y=Price / 1e6, )) +
##   ggbeeswarm::geom_quasirandom(varwidth=TRUE, alpha=0.3) +
##   scale_y_log10() +
##   labs(y = "Price (mil)", x = "# of Rooms")


## -------------------------------------------------------------------------
#| label: hidalgo-data
#| include: false
load(here::here("data/Hidalgo1872.rda"))
skimr::skim(Hidalgo1872)


## -------------------------------------------------------------------------
#| label: hidalgo-plot
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
hid <- ggplot(Hidalgo1872, aes(x=thickness, y=25)) +
  geom_quasirandom(width=15, alpha=0.5, size=1) +
  labs(x = "Thickness (0.001 mm)", y = "Density") + 
  ylim(c(0, 70)) +
  theme(aspect.ratio = 0.6) 
hid_p1 <- hid +
  geom_density(aes(x=thickness), alpha=0.5,
    color = "#E16A86", bw = 0.01, linewidth=2,
    inherit.aes = FALSE)
hid_p2 <- hid +
  geom_density(aes(x=thickness), alpha=0.5,
    color = "#E16A86", bw = 0.0075, linewidth=2,
    inherit.aes = FALSE) 
hid_p3 <- hid +
  geom_density(aes(x=thickness), alpha=0.5,
    color = "#E16A86", bw = 0.004, linewidth=2,
    inherit.aes = FALSE) 
hid_p4 <- hid +
  geom_density(aes(x=thickness), alpha=0.5,
    color = "#E16A86", bw = 0.001, linewidth=2,
    inherit.aes = FALSE) 
hid_p1 + hid_p2 + hid_p3 + hid_p4 + plot_layout(ncol=2)


## ----hidalgo-data, echo = TRUE, render = knitr::normal_print--------------
load(here::here("data/Hidalgo1872.rda"))
skimr::skim(Hidalgo1872)


## -------------------------------------------------------------------------
#| label: hidalgo-plot
#| echo: true
#| eval: false
## hid <- ggplot(Hidalgo1872, aes(x=thickness, y=25)) +
##   geom_quasirandom(width=15, alpha=0.5, size=1) +
##   labs(x = "Thickness (0.001 mm)", y = "Density") +
##   ylim(c(0, 70)) +
##   theme(aspect.ratio = 0.6)
## hid_p1 <- hid +
##   geom_density(aes(x=thickness), alpha=0.5,
##     color = "#E16A86", bw = 0.01, linewidth=2,
##     inherit.aes = FALSE)
## hid_p2 <- hid +
##   geom_density(aes(x=thickness), alpha=0.5,
##     color = "#E16A86", bw = 0.0075, linewidth=2,
##     inherit.aes = FALSE)
## hid_p3 <- hid +
##   geom_density(aes(x=thickness), alpha=0.5,
##     color = "#E16A86", bw = 0.004, linewidth=2,
##     inherit.aes = FALSE)
## hid_p4 <- hid +
##   geom_density(aes(x=thickness), alpha=0.5,
##     color = "#E16A86", bw = 0.001, linewidth=2,
##     inherit.aes = FALSE)
## hid_p1 + hid_p2 + hid_p3 + hid_p4 + plot_layout(ncol=2)


## -------------------------------------------------------------------------
#| label: olive-mixture
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 100%
olive <- read_csv("http://ggobi.org/book/data/olive.csv") |>
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


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| label: movies-data
#| include: false
data(movies, package = "ggplot2movies")
skimr::skim(movies)


## -------------------------------------------------------------------------
#| label: movies-plot1
#| echo: false
#| eval: true
#| out-width: 70%
#| fig-width: 8
#| fig-height: 4

ggplot(movies, aes(length)) +
  geom_histogram(color = "white") +
  labs(x = "Length of movie (minutes)", y = "Frequency") +
  theme(aspect.ratio = 0.6)

ggplot(movies, aes(length)) +
  geom_histogram(color = "white") +
  labs(x = "Length of movie (minutes)", y = "Frequency") +
  scale_x_log10() +
  theme(aspect.ratio = 0.6)



## -------------------------------------------------------------------------
#| label: movies-plot3
#| echo: false
#| eval: true
#| fig-width: 9

movies |>
  filter(length < 180) |>
  ggplot(aes(length)) +
  geom_histogram(binwidth = 1, fill = "#795549", color = "black") +
  labs(x = "Length of movie (minutes)", y = "Frequency")



## ----movies-data, echo = TRUE, render = knitr::normal_print---------------
data(movies, package = "ggplot2movies")
skimr::skim(movies)


## -------------------------------------------------------------------------
#| label: movies-plot1
#| echo: true
#| eval: false
## ggplot(movies, aes(length)) +
##   geom_histogram(color = "white") +
##   labs(x = "Length of movie (minutes)", y = "Frequency") +
##   theme(aspect.ratio = 0.6)
## 
## ggplot(movies, aes(length)) +
##   geom_histogram(color = "white") +
##   labs(x = "Length of movie (minutes)", y = "Frequency") +
##   scale_x_log10() +
##   theme(aspect.ratio = 0.6)
## 


## -------------------------------------------------------------------------
#| label: movies-plot3
#| echo: true
#| eval: false
## movies |>
##   filter(length < 180) |>
##   ggplot(aes(length)) +
##   geom_histogram(binwidth = 1, fill = "#795549", color = "black") +
##   labs(x = "Length of movie (minutes)", y = "Frequency")
## 


## ----factors, echo = TRUE-------------------------------------------------
data <- c(2, 2, 1, 1, 3, 3, 3, 1)
factor(data)


## ----factor-labels, echo = TRUE-------------------------------------------
factor(data, labels = c("I", "II", "III"))


## ----factor-input, echo = TRUE--------------------------------------------
# numerical input are ordered in increasing order 
factor(c(1, 3, 10))
# character input are ordered by first char, alphabetically 
factor(c("1", "3", "10"))
# you can specify order of levels explicitly 
factor(c("1", "3", "10"),
  levels = c("1", "3", "10")
)


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
#| echo: false
#| eval: true
options(digits=2)
tb_oz |>
  filter(year >= 2000) |>
  mutate(p = count/sum(count),
         pct = p*100, 
         odds = count/count[year==2000]) |>
  print(n=100)


## -------------------------------------------------------------------------
#| label: aus-election-data4
#| echo: false
#| eval: true

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

## -------------------------------------------------------------------------
#| label: aus-election-data5
#| echo: false
#| eval: true
tdf3 <- df1 |>
  group_by(DivisionID) |>
  summarise(
    DivisionNm = unique(DivisionNm),
    State = unique(StateAb),
    votes_GRN = TotalVotes[which(PartyAb == "GRN")],
    votes_total = sum(TotalVotes)
  ) |>
  mutate(perc_GRN = votes_GRN / votes_total * 100)


## -------------------------------------------------------------------------
#| label: aus-election-plot3
#| fig-height: 5
#| echo: false
#| eval: true
#| out-width: 80%
tdf3 |>
  ggplot(aes(perc_GRN, State)) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
  labs(
    x = "First preference votes %",
    y = "State",
    title = "Greens party"
  )


## -------------------------------------------------------------------------
#| label: aus-election-plot4
#| fig-height: 5
#| echo: false
#| eval: true
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


## -------------------------------------------------------------------------
#| label: aus-election-data4
#| echo: true
#| eval: false
## df1 <- read_csv(here::here("data/HouseFirstPrefsByCandidateByVoteTypeDownload-24310.csv"),
##   skip = 1,
##   col_types = cols(
##     .default = col_character(),
##     OrdinaryVotes = col_double(),
##     AbsentVotes = col_double(),
##     ProvisionalVotes = col_double(),
##     PrePollVotes = col_double(),
##     PostalVotes = col_double(),
##     TotalVotes = col_double(),
##     Swing = col_double()
##   )
## )


## -------------------------------------------------------------------------
#| label: aus-election-data5
#| echo: true
#| eval: false
## tdf3 <- df1 |>
##   group_by(DivisionID) |>
##   summarise(
##     DivisionNm = unique(DivisionNm),
##     State = unique(StateAb),
##     votes_GRN = TotalVotes[which(PartyAb == "GRN")],
##     votes_total = sum(TotalVotes)
##   ) |>
##   mutate(perc_GRN = votes_GRN / votes_total * 100)


## -------------------------------------------------------------------------
#| label: aus-election-plot3
#| echo: true
#| eval: false
## tdf3 |>
##   ggplot(aes(perc_GRN, State)) +
##   ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
##   labs(
##     x = "First preference votes %",
##     y = "State",
##     title = "Greens party"
##   )


## -------------------------------------------------------------------------
#| label: aus-election-plot4
#| echo: true
#| eval: false
## tdf3 |>
##   mutate(State = fct_reorder(State, perc_GRN)) |>
##   ggplot(aes(perc_GRN, State)) +
##   ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE) +
##   labs(
##     x = "First preference votes %",
##     y = "State",
##     title = "Greens party"
##   )


## -------------------------------------------------------------------------
#| echo: true
#| eval: false
## stats::reorder(factor, value, mean)
## forcats::fct_reorder(factor, value, median)
## forcats::fct_reorder2(factor, value1, value2, func)


## ----eval=FALSE, echo=TRUE------------------------------------------------
## ggplot(data, aes(x=var1)) +
##   geom_col()
## 
## ggplot(data, aes(x=var1)) +
##   geom_bar()


## ----eval=FALSE, echo=TRUE------------------------------------------------
## # Only one option
## null_dist("var1", "binom",
##   list(size=n, p=phat))


## -------------------------------------------------------------------------
#| label: tb-lineup
#| echo: FALSE
#| eval: TRUE
#| out-width: 80%
#| fig-width: 10
#| fig-height: 4
library(nullabor)
set.seed(252)
tb_oz_2012 <- tb |>
  filter(iso3 == "AUS",
         year == 2012) |>
  select(iso3, year, new_sp_m04:new_ep_m65) |>
  pivot_longer(cols=new_sp_m04:new_ep_m65, 
               names_to = "var", 
               values_to = "count") |>
  separate(var, into=c("new", "type", "sexage")) |>
  select(-new) |>
  filter(!(sexage %in% c("sexunk014", "sexunk04", 
                         "sexunk15plus", "sexunk514",
                         "f15plus", "m15plus"))) |>
  group_by(sexage) |>
  summarise(count = sum(count, na.rm=TRUE)) |>
  mutate(sex=str_sub(sexage, 1, 1),
         age=str_sub(sexage, 2, str_length(sexage))) |>
  group_by(sex) |>
  summarise(count=sum(count)) |>
  ungroup() |>
  mutate(sex01 = ifelse(sex=="m", 0, 1)) |>
  select(-sex)

ggplot(lineup(null_dist("count", "binom", 
                        list(size=sum(tb_oz_2012$count),
                               p=0.5)), 
              tb_oz_2012, n=10),
       aes(x=sex01, y=count)) +
  geom_col() +
  facet_wrap(~.sample, ncol=5) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank())


## -------------------------------------------------------------------------
binom.test(tb_oz_2012$count, n, p = 0.5, alternative = "two.sided")


## -------------------------------------------------------------------------
#| label: tb-lineup
#| echo: TRUE
#| eval: FALSE
## library(nullabor)
## set.seed(252)
## tb_oz_2012 <- tb |>
##   filter(iso3 == "AUS",
##          year == 2012) |>
##   select(iso3, year, new_sp_m04:new_ep_m65) |>
##   pivot_longer(cols=new_sp_m04:new_ep_m65,
##                names_to = "var",
##                values_to = "count") |>
##   separate(var, into=c("new", "type", "sexage")) |>
##   select(-new) |>
##   filter(!(sexage %in% c("sexunk014", "sexunk04",
##                          "sexunk15plus", "sexunk514",
##                          "f15plus", "m15plus"))) |>
##   group_by(sexage) |>
##   summarise(count = sum(count, na.rm=TRUE)) |>
##   mutate(sex=str_sub(sexage, 1, 1),
##          age=str_sub(sexage, 2, str_length(sexage))) |>
##   group_by(sex) |>
##   summarise(count=sum(count)) |>
##   ungroup() |>
##   mutate(sex01 = ifelse(sex=="m", 0, 1)) |>
##   select(-sex)
## 
## ggplot(lineup(null_dist("count", "binom",
##                         list(size=sum(tb_oz_2012$count),
##                                p=0.5)),
##               tb_oz_2012, n=10),
##        aes(x=sex01, y=count)) +
##   geom_col() +
##   facet_wrap(~.sample, ncol=5) +
##   theme(axis.text = element_blank(),
##         axis.title = element_blank(),
##         panel.grid.major = element_blank())


## -------------------------------------------------------------------------
#| label: price-impute
df2 <- df2 |>
  mutate(lPrice = log10(Price),
         price_miss = ifelse(is.na(Price), "yes", "no"))

df2_smry <- df2 |>
  summarise(m = mean(lPrice, na.rm=TRUE),
            s = sd(lPrice, na.rm=TRUE))
set.seed(1003)  
df2 <- df2 |>
  rowwise() |>
  mutate(lPrice = ifelse(price_miss == "yes", 
    rnorm(1, df2_smry$m, df2_smry$s), lPrice)) |>
  mutate(Price = ifelse(price_miss == "yes", 10^lPrice, Price))


## -------------------------------------------------------------------------
#| label: price-impute-plot
#| echo: false
#| out-width: 80%

df2 |>
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


## -------------------------------------------------------------------------
#| label: cat-impute
#| echo: false
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

