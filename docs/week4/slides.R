## ----include = FALSE, echo=FALSE-------------------------
source("../setup.R")


## ----coin------------------------------------------------
#| echo: false
head <- '<img src="images/Australian_20c_H.png" height = "50px" style="vertical-align:middle;">'
tail <- '<img src="images/Australian_20c_T.png" height = "50px" style="vertical-align:middle;">'


## ----coin-bias, results='asis'---------------------------
set.seed(924)
samp10 <- sample(rep(c(head, tail), c(7, 3)))
cat(paste0(samp10, collapse = ""))


## ----coin-bias100, results='asis'------------------------
samp100 <- sample(rep(c(head, tail), c(70, 30)))
cat(paste0(samp100, collapse = ""))


## ----echo=TRUE-------------------------------------------
sum(dbinom(7:10, 10, 0.5))


## ----echo=TRUE-------------------------------------------
sum(dbinom(70:100, 100, 0.5))


## --------------------------------------------------------
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


## --------------------------------------------------------
#| eval: false
#| echo: true
# lm(dist ~ speed, data = cars)


## --------------------------------------------------------
#| echo: true
1 - pbinom(2 - 1, 16, 1/20)
nullabor::pvisual(2, 16, 20)


## --------------------------------------------------------
#| label: diamonds-lineup
#| echo: false
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



## --------------------------------------------------------
#| eval: false
#| echo: true
# d_fit <- lm(lprice ~ lcarat, data=diamonds)


## --------------------------------------------------------
#| echo: true
1 - pbinom(8 - 1, 12, 1/20)
nullabor::pvisual(8, 12, 20)


## --------------------------------------------------------
#| echo: false
#| out-width: 60%
#| fig-width: 3
#| fig-height: 3
library(visage)
vi_lineup <- readRDS("/Users/cookd/students_PhD/Patrick/lineup_residual_diagnostics/data/vi_lineup.rds")
i <- 915
l <- vi_lineup[[i]]
VI_MODEL$plot(filter(l$data, k==1), 
                     remove_grid_line = TRUE, 
                     theme = theme_light())


## --------------------------------------------------------
#| echo: false
#| out-width: 60%
#| fig-width: 3
#| fig-height: 3
VI_MODEL$plot(filter(l$data, k==5), 
                     remove_grid_line = TRUE, 
                     theme = theme_light())



## --------------------------------------------------------
#| echo: false
#| out-width: 60%
#| fig-width: 3
#| fig-height: 3
VI_MODEL$plot(filter(l$data, k==14), 
                     remove_grid_line = TRUE, 
                     theme = theme_light())


## --------------------------------------------------------
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
VI_MODEL$plot_lineup(l$data, 
                     remove_grid_line = TRUE, 
                     theme = theme_light(),
                     remove_axis = TRUE)


## --------------------------------------------------------
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
VI_MODEL$plot_lineup(l$data, 
                     remove_grid_line = TRUE, 
                     theme = theme_light(),
                     remove_axis = TRUE) +
  geom_rect(data=filter(l$data, k %in% c(1, 5, 14)), 
            aes(xmin=min(l$data$.fitted), 
                xmax=max(l$data$.fitted), 
                ymin=min(l$data$.resid), 
                ymax=max(l$data$.resid)), 
            colour="black", alpha=0.5, fill=NA, linewidth=0.2) 


## --------------------------------------------------------
#| echo: false
#| out-width: 80%
#| fig-width: 7
#| fig-height: 6
VI_MODEL$plot_lineup(l$data, 
                     remove_grid_line = TRUE, 
                     theme = theme_light(),
                     remove_axis = TRUE) +
  geom_rect(data=filter(l$data, k==2), 
            aes(xmin=min(l$data$.fitted), 
                xmax=max(l$data$.fitted), 
                ymin=min(l$data$.resid), 
                ymax=max(l$data$.resid)), 
            colour="yellow", alpha=0.5, fill=NA, linewidth=1)


## --------------------------------------------------------
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



## --------------------------------------------------------
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



## --------------------------------------------------------
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


## --------------------------------------------------------
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


## ----star-null, message = TRUE---------------------------
line_df <- lineup(null_dist("temp", "exp", 
    list(rate = 1 / mean(dslabs::stars$temp))),
  true = dslabs::stars,
  n = 10
)


## ----stars-lineup, echo = FALSE, fig.width = 14----------
ggplot(line_df, aes(temp)) +
  geom_histogram(color = "white") +
  facet_wrap(~.sample, nrow = 2) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank()
  )


## ----stars-lineup, eval = FALSE--------------------------
# ggplot(line_df, aes(temp)) +
#   geom_histogram(color = "white") +
#   facet_wrap(~.sample, nrow = 2) +
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank()
#   )


## --------------------------------------------------------
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



## --------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 80%
library(palmerpenguins)
set.seed(400)
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_length_mm, colour=species)) +
  geom_point(alpha=0.4) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## --------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 80%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_length_mm, fill=species), colour="white") +
  geom_boxplot() +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_fill_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## --------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 80%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_length_mm, fill=species), colour="white") +
  geom_violin() +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_fill_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## --------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 80%
ggplot(lineup(null_permute("species"), penguins, n=10),
       aes(x=species, y=bill_length_mm, colour=species)) +
  ggbeeswarm::geom_quasirandom(alpha=0.8) +
  facet_wrap(~.sample, ncol=5) +
  colorspace::scale_color_discrete_divergingx(palette="Temps") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title = element_blank(),
    axis.text = element_blank()
  )


## ----lineup 1, fig.height=8, fig.width=8, out.width="70%"----
set.seed(20190709)
ggplot(lineup(null_permute('mpg'), mtcars), 
  aes(x=mpg, y=wt)) +
  geom_point() +
  facet_wrap(~ .sample) +
  theme(axis.text=element_blank(),
        axis.title=element_blank())

