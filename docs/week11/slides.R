## ----include = FALSE, echo=FALSE-------------------------------------------------
source("../setup.R")


## --------------------------------------------------------------------------------
#| eval: false
#| echo: false
## # Code to generate data in idea image
## set.seed(1135)
## df2 <- tibble(id = 1:224) |>
##   mutate(x = runif(n(), -10, 10),
##          y = 0.5 * x + 3 * sin(x) + rnorm(n(), 0, 2))
## df2 <- bind_rows(df2, tibble(id=225, x=1.5, y=-7))
## ggplot(df2, aes(x, y)) +
##   geom_point(colour="#D93F00", alpha=0.8, size=3) +
##   xlim(c(-10.1, 10.1)) + ylim(c(-13, 13)) +
##   theme(aspect.ratio = 0.8,
##         axis.title = element_blank(),
##         axis.text = element_blank())
## 
## ggplot(df2, aes(x, y)) +
##   geom_smooth(se = FALSE, color = "#D93F00",
##      method = stats::loess, size = 3,
##      method.args = list(span = 0.4)) +
##   xlim(c(-10.1, 10.1)) + ylim(c(-13, 13)) +
##   theme(aspect.ratio = 0.8,
##         axis.title = element_blank(),
##         axis.text = element_blank())
## 
## ggplot(df2, aes(x, y)) +
##   geom_smooth(se = FALSE, color = "#D93F00",
##      method = stats::loess, size = 3,
##      method.args = list(span = 0.4)) +
##   xlim(c(-10.1, 10.1)) + ylim(c(-13, 13)) +
##   theme(aspect.ratio = 0.8,
##         axis.title = element_blank(),
##         axis.text = element_blank())
## 
## df2_lo <- loess(y~x, data=df2, span = 0.4)
## df2 <- df2 |>
##   mutate(.fitted = df2_lo$fitted,
##          .resid = df2_lo$residuals)
## 
## ggplot(df2, aes(x, .fitted)) +
##   geom_line(color = "#D93F00",
##      linewidth = 3) +
##   xlim(c(-10.1, 10.1)) + ylim(c(-13, 13)) +
##   theme(aspect.ratio = 0.8,
##         axis.title = element_blank(),
##         axis.text = element_blank())
## 
## ggplot(df2, aes(x, y)) +
##   geom_point(colour="#D93F00", alpha=0.3, size=3) +
##   geom_point(data=filter(df2, abs(.resid) > 5), colour="#D93F00", size=6) +
##   xlim(c(-10.1, 10.1)) + ylim(c(-13, 13)) +
##   theme(aspect.ratio = 0.8,
##         axis.title = element_blank(),
##         axis.text = element_blank())


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 3
#| out-width: 80%
mtcars_fit <- lm(mpg~poly(hp, 2), data=mtcars)
mtcars_all <- augment(mtcars_fit, mtcars)
p1 <- ggplot(mtcars_all, aes(x=hp, y=mpg)) +
  geom_point() +
  ylim(c(10, 35)) +
  ggtitle("data")
p2 <- ggplot(mtcars_all, aes(x=hp, y=.fitted)) +
  geom_point() +
  ylab("f") +
  ylim(c(10, 35)) +
  ggtitle("model")
p1 + p2 + plot_layout(ncol=2)

mtcars_coefs <- tidy(mtcars_fit)
mtcars_stats <- glance(mtcars_fit)
mtcars_coefs
mtcars_stats


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 6
#| out-width: 70%
set.seed(1026)
ggplot(lineup(null_dist(".resid", "norm"), n=12, true=mtcars_all), 
       aes(x=.resid)) +
  geom_histogram(binwidth=1, colour="white") +
  facet_wrap(~.sample, ncol=4) +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 6
#| out-width: 70%
p3 <- ggplot(mtcars_all, aes(x=.resid)) +
  geom_histogram(binwidth=1, colour="white") 
p4 <- ggplot(mtcars_all, aes(sample=.resid)) +
  geom_qq_line() +
  geom_qq() 
p5 <- ggplot(mtcars_all, aes(x=1, y=.resid)) +
  geom_quasirandom() 
p6 <- ggplot(mtcars_all, aes(x=1, y=.resid)) +
  geom_violin(fill="#D93F00", colour="white", 
              draw_quantiles=c(0.25, 0.5, 0.75)) 

p3 + p4 + p5 + p6 + plot_layout(ncol=2)



## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 6
#| out-width: 70%
set.seed(1058)
ggplot(lineup(null_dist(".resid", "norm"), n=12, true=mtcars_all), 
       aes(x=hp, y=.resid)) +
  geom_point() +
  facet_wrap(~.sample, ncol=4) +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 8
#| fig-height: 5
#| out-width: 70%
p7 <- ggplot(mtcars_all, aes(x=hp, y=.resid)) +
  geom_point() +
  ylab("e") +
  ylim(c(-3, 3)) +
  ggtitle("residuals")
mt_full_fit <- tibble(hp = seq(50, 340, 10))
mt_full_fit <- mt_full_fit |>
                mutate(mpg = predict(mtcars_fit, mt_full_fit))
p8 <- ggplot(mtcars_all, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_line(data=mt_full_fit, colour="#D93F00", linewidth=2) +
  ylim(c(10, 35)) +
  ggtitle("data+f")

p7 + p8 + plot_layout(ncol=2)



## --------------------------------------------------------------------------------
#| code-fold: true
mtcars_all <- mtcars_all |>
  mutate(id = 1:n())
p10 <- ggplot(mtcars_all, aes(x=id, y=.hat, label=id)) +
  geom_point() +
  labs(title="leverage")
p11 <- ggplot(mtcars_all, aes(x=id, y=.cooksd, label=id)) +
  geom_point() +
  labs(title="cooks d") 
p12 <- ggplot(mtcars_all, aes(x=.resid, y=.std.resid, label=id)) +
  geom_point() +
  labs(title="studentised residuals")


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-show: hold
ggplotly(p10, width=400, height=400) |> config(displayModeBar = FALSE)


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-show: hold
ggplotly(p11, width=400, height=400) |> config(displayModeBar = FALSE)


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| fig-show: hold
ggplotly(p12, width=400, height=400) |> config(displayModeBar = FALSE)


## --------------------------------------------------------------------------------
#| eval: false
## mt_full_fit <- tibble(hp = seq(50, 340, 10))
## mt_full_fit <- mt_full_fit |>
##                 mutate(mpg = predict(mtcars_fit, mt_full_fit))


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 50%
ggplot(mt_full_fit, aes(x=hp, y=mpg)) +
  geom_point(colour="#D93F00") +
  ylim(c(10, 35)) 


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 4
#| out-width: 70%
set.seed(1249)
mt_sample1 <- tibble(hp = runif(nrow(mtcars), 50, 340),
                     e = rnorm(nrow(mtcars), sd = mtcars_stats$sigma))
mt_sample1 <- mt_sample1 |>
                mutate(mpg = predict(mtcars_fit, mt_sample1) + e,
                       type = "sim") |>
  select(hp, mpg, type)
mtcars_sub <- mtcars |>
  select(hp, mpg) |>
  mutate(type = "data")
mt_sample1 <- bind_rows(mt_sample1, mtcars_sub)

ggplot(mt_sample1, aes(x=hp, y=mpg, colour=type)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  ylim(c(10, 35)) 



## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 4
#| out-width: 70%
set.seed(1250)
mt_sample2 <- tibble(hp = runif(nrow(mtcars), 50, 340),
                     e = rnorm(nrow(mtcars), sd = mtcars_stats$sigma))
mt_sample2 <- mt_sample2 |>
                mutate(mpg = predict(mtcars_fit, mt_sample2) + e,
                       type = "sim") |>
  select(hp, mpg, type)
mtcars_sub <- mtcars |>
  select(hp, mpg) |>
  mutate(type = "data")
mt_sample2 <- bind_rows(mt_sample2, mtcars_sub)

ggplot(mt_sample2, aes(x=hp, y=mpg, colour=type)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  ylim(c(10, 35)) 


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 4
#| out-width: 70%
set.seed(1350)
mt_sample3 <- tibble(hp = runif(nrow(mtcars), 50, 340),
                     e = rnorm(nrow(mtcars), sd = mtcars_stats$sigma))
mt_sample3 <- mt_sample3 |>
                mutate(mpg = predict(mtcars_fit, mt_sample3) + e,
                       type = "sim") |>
  select(hp, mpg, type)
mtcars_sub <- mtcars |>
  select(hp, mpg) |>
  mutate(type = "data")
mt_sample3 <- bind_rows(mt_sample3, mtcars_sub)

ggplot(mt_sample3, aes(x=hp, y=mpg, colour=type)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  ylim(c(10, 35)) 


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 4
#| out-width: 80%
mtcars_fit2 <- lm(mpg~hp, data=mtcars)
mtcars_all2 <- augment(mtcars_fit2, mtcars)
sp1 <- ggplot(mtcars_all2, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_point(aes(x=hp, y=.fitted), colour="#D93F00") + 
  ylim(c(10, 35)) +
  ggtitle("data+model")
sp2 <- ggplot(mtcars_all2, aes(x=hp, y=.resid)) +
  geom_hline(yintercept=0, colour="grey60") +
  geom_point() +
  ylab("e") +
  ggtitle("residuals")
sp1 + sp2 + plot_layout(ncol=2)


## --------------------------------------------------------------------------------
#| echo: false
tidy(mtcars_fit)
glance(mtcars_fit)


## --------------------------------------------------------------------------------
#| echo: false
tidy(mtcars_fit2)
glance(mtcars_fit2)


## --------------------------------------------------------------------------------
#| code-fold: true
mtcars_fit3 <- lm(mpg~poly(hp, 5), data=mtcars)
mtcars_all3 <- augment(mtcars_fit3, mtcars)
mtcars_stats3 <- glance(mtcars_fit3)

set.seed(129)
mt_extrap <- tibble(hp = seq(250, 450, 10),
                    e = rnorm(length(hp), sd=mtcars_stats3$sigma))
mt_extrap <- mt_extrap |>
                mutate(mpg = predict(mtcars_fit3, mt_extrap) + e) |>
  mutate(type = "new") |>
  select(-e)
mt_extrap <- bind_rows(mt_extrap, mtcars_sub)


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
ggplot(mt_extrap, aes(x=hp, y=mpg, colour=type)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") 


## --------------------------------------------------------------------------------
#| echo: false
mtcars <- mtcars |>
  mutate(am = factor(am, labels = c("auto", "manual")))
ggplot(mtcars, aes(x=hp, y=mpg, colour=am)) +
  geom_point() + 
  scale_color_discrete_divergingx(palette="Zissou 1") 


## --------------------------------------------------------------------------------
#| code-fold: true
mtcars_fit4 <- lm(mpg~poly(hp, 2)+am, data=mtcars)
mtcars_all4 <- augment(mtcars_fit4, mtcars)
mtcars_coefs4 <- tidy(mtcars_fit4)
mtcars_stats4 <- glance(mtcars_fit4)
mtcars_fit5 <- lm(mpg~poly(hp, 2)*am, data=mtcars)
mtcars_all5 <- augment(mtcars_fit5, mtcars)
mtcars_coefs5 <- tidy(mtcars_fit5)
mtcars_stats5 <- glance(mtcars_fit5)
ip1 <- ggplot(mtcars_all4, aes(x=hp, y=.fitted, colour=am)) +
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  ggtitle("No interaction") +
  theme(legend.position = "None")
ip2 <- ggplot(mtcars_all5, aes(x=hp, y=.fitted, colour=am)) +
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  ggtitle("With interaction") +
  theme(legend.position = "None")


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 90%
ip1 + ip2 + plot_layout(ncol=2)


## --------------------------------------------------------------------------------
#| echo: false
mtcars_coefs4
mtcars_coefs5


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 4
#| fig-height: 4
#| out-width: 50%
mtcars_fit6 <- loess(mpg~hp, data=mtcars, degree=1, span=0.2)
mtcars_fit7 <- loess(mpg~hp, data=mtcars, degree=1, span=0.5)
mtcars_lo <- mtcars |>
  mutate(fit6 = mtcars_fit6$fitted,
         fit7 = mtcars_fit7$fitted)
mtcars_lop <- tibble(hp = seq(50, 340, 5))
mtcars_lop <- mtcars_lop |>
                mutate(mpg6 = predict(mtcars_fit6, mtcars_lop),
                       mpg7 = predict(mtcars_fit7, mtcars_lop))

ggplot(data=mtcars_lo, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_line(data=mtcars_lop, aes(x=hp, y=mpg6), colour="#D93F00") +
  geom_line(data=mtcars_lop, aes(x=hp, y=mpg7), colour="#027EB6") 


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 4
#| fig-height: 4
#| out-width: 50%
mtcars_fit8 <- gam(mpg~s(hp, bs="cr", k=3, m=1), data=mtcars, 
                   knots=list(seq(90, 300, length=3)))
mtcars_fit9 <- gam(mpg~s(hp, bs="cr", k=10, m=1), data=mtcars, 
                   knots=list(seq(90, 300, length=3)))
mtcars_gam <- mtcars |>
  mutate(fit8 = mtcars_fit8$fitted.values,
         fit9 = mtcars_fit9$fitted.values)
mtcars_gamp <- tibble(hp = seq(50, 450, 5))
mtcars_gamp <- mtcars_gamp |>
                mutate(mpg8 = predict(mtcars_fit8, mtcars_gamp),
                       mpg9 = predict(mtcars_fit9, mtcars_gamp))

ggplot(data=mtcars_gam, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_line(data=mtcars_gamp, aes(x=hp, y=mpg8), colour="#D93F00") +
  geom_line(data=mtcars_gamp, aes(x=hp, y=mpg9), colour="#027EB6") 


## --------------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 6
#| fig-height: 3
#| out-width: 100%
mtcars_ordered <- mtcars |>
  mutate(ami = as.numeric(am)-1) |>
  arrange(mpg)
mtcars_am_fit1 <- loess(ami~mpg, data=mtcars_ordered, degree=0, span=0.7)
mtcars_am_lo <- mtcars_ordered |>
  mutate(fit1 = mtcars_am_fit1$fitted)
lg1 <- ggplot(mtcars_ordered, aes(x=mpg, y=ami)) +
  geom_point() +
  geom_point(data=mtcars_am_lo, aes(x=mpg, y=fit1), colour="#D93F00") +
  geom_line(data=mtcars_am_lo, aes(x=mpg, y=fit1), colour="#D93F00") +
  ylab("automatic") +
  ggtitle("Loess")

mtcars_am_fit2 <- glm(ami~mpg, data=mtcars_ordered, family = binomial(link = "logit"))
mtcars_am_lo <- mtcars_am_lo |>
  mutate(fit2 = mtcars_am_fit2$fitted.values)
lg2 <- ggplot(mtcars_ordered, aes(x=mpg, y=ami)) +
  geom_point() +
  geom_point(data=mtcars_am_lo, aes(x=mpg, y=fit2), colour="#D93F00") +
  geom_line(data=mtcars_am_lo, aes(x=mpg, y=fit2), colour="#D93F00") +
  ylab("automatic") +
  ggtitle("Logistic")

lg1 + lg2 + plot_layout(ncol=2)


## --------------------------------------------------------------------------------
#| code-fold: true
co2 <- read_csv("../data/CO2-monthly.csv") |>
  mutate(date = dmy(date))
co2_long <- co2 |>
  pivot_longer(ljo:spo, names_to = "stn", values_to = "co2") |>
  filter(stn %in% c("ptb", "ljo", "spo", "mlf")) |>
  mutate(stn = factor(stn, levels = c("ptb", "ljo", "mlf", "spo"))) |>
  filter(!is.na(co2)) |>
  filter(co2 < 430)
co2_p1 <- ggplot(co2_long, aes(x=date, y=co2, colour=stn)) +
  geom_point() +
  facet_wrap(~stn, ncol=2) +
  theme(legend.position="none") +
  ylim(c(330, 430)) +
  theme(aspect.ratio = 0.5)

co2_fit <- co2_long |>
  mutate(time = as.numeric(date)-7304) |>
  nest(dat = -stn) |>
  mutate(mod_trend = map(dat, ~lm(co2~time+I(time^2), data=.x)),
         mod_season = map(dat, ~lm(co2~time+I(time^2)+month, data=.x)),
        coef_trend = map(mod_trend, tidy),
        coef_seasonal = map(mod_season, tidy),
        fit_trend = map(mod_trend, augment),
        fit_season = map(mod_season, augment)) |>
    select(stn, starts_with('fit')) |> 
    pivot_longer(
        cols = starts_with('fit')
    ) |>
    unnest(value)
co2_p2 <- co2_fit |>
  filter(name == "fit_trend") |>
  ggplot(aes(x=time, y=.fitted, colour=stn)) +
  geom_point() +
  ylim(c(330, 430))

co2_p3 <- co2_fit |>
  filter(name == "fit_season") |>
  mutate(year = time %/% 356 + 1990) |>
  group_by(stn, year) |>
  mutate(.fitted_seas = .fitted - mean(.fitted, na.rm=TRUE)) |>
  ggplot(aes(x=time, y=.fitted_seas, colour=stn)) +
  geom_line() + 
  facet_wrap(~stn, ncol=2) +
  theme(aspect.ratio=0.5)


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 70%
co2_p1


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 60%
co2_p2 + theme(legend.position = "none")


## --------------------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
co2_p3 + ylab("De-trended fit")


## --------------------------------------------------------------------------------
#| eval: false
#| echo: false
## track <- read_csv(here::here("data/womens_track.csv"))
## animate_xy(track[,1:7], col="#EC5C00",
##              cex=2,
##              rescale=TRUE,
##              half_range = 1.4)
## render_gif(track[,1:7],
##            grand_tour(),
##            display_xy(col="#EC5C00",
##              cex=2,
##              half_range = 1.4),
##            rescale=TRUE,
##            gif_file = "images/track.gif",
##            apf = 1/30,
##            frames = 1500,
##            width = 400,
##            height = 400)


## --------------------------------------------------------------------------------
#| eval: false
#| echo: false
## track_std <- track |>
##   mutate_if(is.numeric, function(x) (x-
##       mean(x, na.rm=TRUE))/
##       sd(x, na.rm=TRUE))
## track_std_pca <- prcomp(track_std[,1:7],
##                scale = FALSE,
##                retx=TRUE)
## track_model <- pca_model(track_std_pca, d=2, s=2)
## track_all <- rbind(track_model$points, track_std[,1:7])
## animate_xy(track_all, edges=track_model$edges,
##            edges.col="#EC5C00",
##            edges.width=3,
##            axes="bottomleft",
##            half_range = 6)
## render_gif(track_all,
##            grand_tour(),
##            display_xy(
##                       edges=track_model$edges,
##                       edges.col="#EC5C00",
##                       edges.width=3,
##                       axes="bottomleft",
##                       half_range = 6),
##            gif_file="images/track_model.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)

