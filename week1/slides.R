## ----include = FALSE-------------------------------
source("../setup.R")


## --------------------------------------------------
library(tidyverse)
tips <- read_csv("http://ggobi.org/book/data/tips.csv")


## ----echo = TRUE-----------------------------------
tips <- tips %>%
  mutate(tip_pct = tip/totbill * 100) 


## ----echo = TRUE-----------------------------------
tips_lm <- tips %>%
  select(tip_pct, sex, smoker, day, time, size) %>%
  lm(tip_pct ~ ., data=.) 


## ----modela, echo=TRUE, results="hide"-------------
library(broom)
library(kableExtra)
tidy(tips_lm) %>% 
  kable(digits=2) %>% 
  kable_styling() 


## ----modelb, echo=TRUE, results="hide"-------------
glance(tips_lm) %>% 
  select(r.squared, statistic, 
         p.value) %>% 
  kable(digits=3)


## ----ref.label="modela", echo=FALSE----------------


## ----ref.label="modelb", echo=FALSE----------------


## ----model_smalla, echo=TRUE, results='hide'-------
tips_lm <- tips %>%
  select(tip_pct, size) %>% 
  lm(tip_pct ~ ., data=.) 
tidy(tips_lm) %>% 
  kable(digits=2) %>% 
  kable_styling() 


## ----model_smallb, echo=TRUE, results='hide'-------
glance(tips_lm) %>% 
  select(r.squared, statistic, p.value) %>% 
  kable(digits=3)


## ----ref.label="model_smalla", echo=FALSE----------


## ----ref.label="model_smallb", echo=FALSE----------


## ----res_hist, echo=TRUE, fig.show='hide'----------
tips_aug <- augment(tips_lm)
ggplot(tips_aug, 
    aes(x=.resid)) + 
  geom_histogram() +
  xlab("residuals") 


## ----res_hist2, ref.label="res_hist", echo=FALSE----


## ----res_qq, echo=TRUE, fig.show='hide'------------
ggplot(tips_aug, 
    aes(sample=.resid)) + 
  stat_qq() +
  stat_qq_line() +
  xlab("residuals") +
  theme(aspect.ratio=1)


## ----res_qq2, ref.label="res_qq", echo=FALSE-------


## ----obs_fitted, echo=TRUE, fig.show='hide'--------
ggplot(tips_aug, 
    aes(x=.fitted, y=tip_pct)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  xlab("observed") +
  ylab("fitted")


## ----obs_fitted2, ref.label="obs_fitted", echo=FALSE----


## ----fitted_model, echo=TRUE, fig.show='hide'------
ggplot(tips_aug, 
    aes(x=size, y=tip_pct)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("tip %")


## ----fitted_model2, ref.label="fitted_model", echo=FALSE----


## --------------------------------------------------
#| echo: false
options(width = 50)


## --------------------------------------------------
glimpse(tips)


## ----tips_hist, echo=TRUE, fig.show='hide'---------
ggplot(tips, 
    aes(x=tip)) + 
  geom_histogram(
    colour="white")  


## ----tips_hist2, ref.label="tips_hist", echo=FALSE----


## ----tips_hist_fat, echo=TRUE, fig.show='hide'-----
ggplot(tips, 
    aes(x=tip)) +
  geom_histogram(
    breaks=seq(0.5,10.5,1), #<< 
    colour="white") + 
  scale_x_continuous(
    breaks=seq(0,11,1))


## ----tips_hist_fat2, ref.label="tips_hist_fat", echo=FALSE----


## ----tips_hist_skinny, echo=TRUE, fig.show='hide'----
ggplot(tips, 
    aes(x=tip)) + 
  geom_histogram(
    breaks=seq(0.5,10.5,0.1), #<<
    colour="white") +
  scale_x_continuous(
    breaks=seq(0,11,1))


## ----tips_hist_skinny2, ref.label="tips_hist_skinny", echo=FALSE----


## ----tips_tot, echo=TRUE, fig.show='hide'----------
p <- ggplot(tips, 
    aes(x= totbill, y=tip)) + 
  geom_point() + #<<
  scale_y_continuous(
    breaks=seq(0,11,1))
p


## ----tips_tot_b, ref.label="tips_tot", echo=FALSE----


## ----tips_tot2, echo=TRUE, fig.show='hide'---------
p <- p + geom_abline(intercept=0, #<<
              slope=0.2) + #<<
  annotate("text", x=45, y=10, 
           label="20% tip") 
p


## ----tips_tot2b, ref.label="tips_tot2", echo=FALSE----


## ----tips_sexsmoke, echo=TRUE, fig.show='hide'-----
p + facet_grid(smoker~sex) #<<


## ----tips_sexsmokeb, ref.label="tips_sexsmoke", echo=FALSE, fig.width=9, fig.height=6, out.width="100%"----

