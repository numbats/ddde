## ----include = FALSE, echo=FALSE-----------------------------------
source("../setup.R")


## ----coin----------------------------------------------------------
#| echo: false
head <- '<img src="images/Australian_20c_H.png" height = "50px" style="vertical-align:middle;">'
tail <- '<img src="images/Australian_20c_T.png" height = "50px" style="vertical-align:middle;">'


## ----coin-bias, results='asis'-------------------------------------
set.seed(924)
samp10 <- sample(rep(c(head, tail), c(7, 3)))
cat(paste0(samp10, collapse = ""))


## ----coin-bias100, results='asis'----------------------------------
samp100 <- sample(rep(c(head, tail), c(70, 30)))
cat(paste0(samp100, collapse = ""))


## ----echo=TRUE-----------------------------------------------------
sum(dbinom(7:10, 10, 0.5))


## ----echo=TRUE-----------------------------------------------------
sum(dbinom(70:100, 100, 0.5))


## ------------------------------------------------------------------
#| label: coin-fair
#| code-fold: false
#| eval: false
# coinflips <- sample(c("H", "T"), 10, replace=TRUE)
# sum(coinflips == "H")


## ------------------------------------------------------------------
#| echo: false
countdown::countdown(8, 34)


## ------------------------------------------------------------------
#| label: analyse-results1
#| echo: false
#| eval: false
# results1 <- read_csv("data/coinflips1-week4.csv")
# ggplot(results1, aes(x=coinflips)) +
#   geom_histogram(breaks = seq(-0.5, 10.5, 1),
#     fill = "grey70") +
#   geom_vline(x = 7) # observed value


## ------------------------------------------------------------------
#| label: coin-100
#| code-fold: false
#| eval: false
# coinflips <- sample(c("H", "T"), 100, replace=TRUE)
# sum(coinflips == "H")


## ------------------------------------------------------------------
#| echo: false
countdown::countdown(4, 46)


## ------------------------------------------------------------------
#| label: analyse-results2
#| echo: false
#| eval: false
# results2 <- read_csv("data/coinflips2-week4.csv")
# ggplot(results2, aes(x=coinflips)) +
#   geom_histogram(breaks = seq(-5, 105, 10),
#     fill = "grey70") +
#   geom_vline(x = 70) # observed value


## ------------------------------------------------------------------
#| label: cars-residual
cars_fit <- lm(dist ~ speed, data = cars)
cars_lm <- augment(cars_fit)
set.seed(1051)
ggplot(cars_lm, aes(x=speed, y=.resid)) +
  geom_point() 


## ------------------------------------------------------------------
#| label: cars-lineup
#| echo: false
#| out-width: 90%
#| fig-width: 9
#| fig-height: 7
library(nullabor)
cars_fit <- lm(dist ~ speed, data = cars)
cars_lm <- augment(cars_fit)
set.seed(1051)
ggplot(lineup(null_lm(dist ~ speed, method="rotate"), cars_lm), aes(x=speed, y=.resid)) +
  geom_point() +
  facet_wrap(~.sample, ncol=5) +
  theme(axis.text=element_blank(),
        axis.title=element_blank())


## ------------------------------------------------------------------
#| eval: false
#| echo: true
# lm(dist ~ speed, data = cars)


## ------------------------------------------------------------------
nullabor::pvisual(2, 16, 20)


## ------------------------------------------------------------------
#| label: diamonds-lineup
#| out-width: 80%
#| fig-width: 9
#| fig-height: 7
library(broom)
diamonds <- diamonds %>%
  mutate(lprice = log10(price),
         lcarat = log10(carat))
d_fit <- lm(lprice ~ lcarat, data=diamonds)
d_res <- augment(d_fit, diamonds)

set.seed(923)
l <- lineup(null_lm(lprice ~ lcarat,
                      method="rotate"), d_res)
ggplot(l, aes(lcarat, .resid)) + 
  geom_hline(yintercept=0, colour="grey70") +
  geom_point(alpha = 0.01) +
  geom_smooth(data=l, method = "lm", colour="orange", se=F) +
  facet_wrap(~.sample, ncol=5) +
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.title=element_blank())



## ------------------------------------------------------------------
#| eval: false
# d_fit <- lm(lprice ~ lcarat, data=diamonds)


## ------------------------------------------------------------------
#| echo: true
nullabor::pvisual(8, 12, 20)


## ------------------------------------------------------------------
#| echo: false
#| out-width: 60%
#| fig-width: 3
#| fig-height: 3
set.seed(332)
d <- tibble(.fitted = -rexp(n=84*12),
            .resid = rnorm(n=84*12),
            .sample = rep(1:12, 84))

d |>
  dplyr::filter(.sample == 1) |>
  ggplot(aes(x=.fitted, y=.resid)) +
    geom_hline(yintercept = 0, colour = "red") +
    geom_point(alpha = 0.8) +
    theme_bw() +
    theme(axis.text = element_blank())


## ------------------------------------------------------------------
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
threept <- subset(lal, type == "3pt" & !is.na(x) & !is.na(y))
threept <- threept[c(".id", "period", "time", "team", "etype", "player", "points", "result", "x", "y")]
threept <- transform(threept, 
  x = x + runif(length(x), -0.5, 0.5),
  y = y + runif(length(y), -0.5, 0.5))
threept <- transform(threept, 
  r = sqrt((x - 25) ^ 2 + y ^ 2),
  angle = atan2(y, x - 25))

# Focus in on shots in the typical range
threept_sub <- threept %>% 
  filter(between(r, 20, 39)) %>%
  mutate(angle = angle * 180 / pi) %>%
  select(angle, r)

ggplot(lineup(null_lm(r ~ poly(angle, 2)), 
              true=threept_sub, n = 20, pos = 2), 
       aes(x=angle, y=r)) + 
  geom_point(alpha=0.3) + 
  scale_x_continuous("Angle (degrees)", 
  breaks = c(0, 45, 90, 135, 180), limits = c(0, 180)) +
  facet_wrap(~ .sample, ncol = 5) +
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.title=element_blank())



## ------------------------------------------------------------------
#| label: lineup-aud
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
library(forecast)

l <- lineup(null_ts("rate", auto.arima), aud, pos=10)
ggplot(l, aes(x=date, y=rate)) + geom_line() +
  facet_wrap(~.sample, scales="free_y") +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")



## ------------------------------------------------------------------
#| label: lineup-cars
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
ggplot(lineup(null_permute('mpg'), mtcars), aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ .sample, ncol=5) +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")


## ------------------------------------------------------------------
#| label: lineup-cars
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
ggplot(lineup(null_permute('mpg'), mtcars), aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ .sample, ncol=5) +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")


## ----star-null, message = TRUE-------------------------------------
line_df <- lineup(null_dist("temp", "exp", 
    list(rate = 1 / mean(dslabs::stars$temp))),
  true = dslabs::stars,
  n = 10
)


## ----stars-lineup, echo = FALSE, fig.width = 14--------------------
ggplot(line_df, aes(temp)) +
  geom_histogram(color = "white") +
  facet_wrap(~.sample, nrow = 2) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )


## ----stars-lineup, eval = FALSE------------------------------------
# ggplot(line_df, aes(temp)) +
#   geom_histogram(color = "white") +
#   facet_wrap(~.sample, nrow = 2) +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank()
#   )


## ------------------------------------------------------------------
#| label: lineup-aud
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
library(forecast)

l <- lineup(null_ts("rate", auto.arima), aud, pos=10)
ggplot(l, aes(x=date, y=rate)) + geom_line() +
  facet_wrap(~.sample, scales="free_y") +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")



## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, colour=species)) +
  geom_point(alpha=0) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
set.seed(400)
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, colour=species)) +
  geom_point(alpha=0.4) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, colour=species)) +
  geom_point(alpha=0) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, fill=species), colour="white") +
  geom_boxplot() +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_fill_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, colour=species)) +
  geom_point(alpha=0) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, fill=species), colour="white") +
  geom_violin() +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_fill_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, colour=species)) +
  geom_point(alpha=0) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, colour=species)) +
  ggbeeswarm::geom_quasirandom(alpha=0.8) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 60%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_len, colour=species)) +
  geom_point(alpha=0) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ------------------------------------------------------------------
#| echo: false
countdown::countdown(0, 3)


## ------------------------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 6
#| out-width: 90%
library(DiagrammeR)
grViz("
digraph process {
  graph [rankdir = TB, nodesep = 0.4, ranksep = 0.4]
  node [fontname = Helvetica, shape = box, style = filled, fillcolor = white, color = '#006dae', fontcolor = '#006dae']
  edge [color = '#006dae']

  A [label = 'Generate a lineup:\ntrue data plot hidden among null plots']
  B [label = 'Show the lineup to blind evaluators\nfor a fixed viewing time']
  C [label = 'Each evaluator picks the panel\nthat looks most different']
  D [label = 'Record x = number of evaluators who picked\nthe true data plot, out of n evaluators']
  E [label = 'Compute power = x / n\nfor this plot design']
  F [label = 'Repeat for every candidate\nplot design', shape = diamond]
  G [label = 'Compare power across designs']
  H [label = 'Choose the design\nwith the highest power']

  A -> B -> C -> D -> E -> F
  F -> A [label = '  geom_point, geom_boxplot,\n  geom_violin, geom_quasirandom  ']
  F -> G -> H
}
")

