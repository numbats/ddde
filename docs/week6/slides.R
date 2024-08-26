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
#| label: scatterplots
#| echo: false
#| eval: false
#| fig-width: 2
#| fig-height: 2
## set.seed(55555)
## d_trend <- tibble(x = runif(100) - 0.5) |>
##   mutate(
##     positive = 4 * x + rnorm(100) * 0.5,
##     none = rnorm(100) * 0.5,
##     negative = -4 * x + rnorm(100) * 0.5
##   ) |>
##   pivot_longer(cols = positive:negative, names_to = "trend", values_to = "y") |>
##   mutate(trend = factor(trend,
##     levels = c("positive", "none", "negative")
##   )) |>
##   select(trend, x, y)
## 
## d_trend |>
##   filter(trend == "positive") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_trend |>
##   filter(trend == "negative") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_trend |>
##   filter(trend == "none") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_strength <- tibble(x = runif(100) - 0.5) |>
##   mutate(
##     strong = 4 * x + rnorm(100) * 0.5,
##     moderate = 4 * x + rnorm(100),
##     weak = -4 * x + rnorm(100) * 3
##   ) |>
##   pivot_longer(
##     cols = strong:weak,
##     names_to = "strength", values_to = "y"
##   ) |>
##   mutate(strength = factor(strength,
##     levels = c("strong", "moderate", "weak")
##   )) |>
##   select(strength, x, y)
## 
## d_strength |>
##   filter(strength == "strong") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_strength |>
##   filter(strength == "moderate") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_strength |>
##   filter(strength == "weak") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )


## -------------------------------------------------------------------------
#| label: feature-table
#| echo: false
tribble(
  ~Feature, ~Example, ~Description,
  "positive trend", "", "Low value corresponds to low value, and high to high.",
  "negative trend", "", "Low value corresponds to high value, and high to low.",
  "no trend", "", "No relationship",
  "strong", "", "Very little variation around the trend",
  "moderate", "", "Variation around the trend is almost as much as the trend",
  "weak", "", "A lot of variation making it hard to see any trend"
) |>
  knitr::kable(escape = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=30, 
                            full_width=FALSE) |>
  kableExtra::column_spec(2, 
    image=spec_image(
      c("images/scatterplots-1.png",
        "images/scatterplots-2.png",
        "images/scatterplots-3.png",
        "images/scatterplots-4.png",
        "images/scatterplots-5.png",
        "images/scatterplots-6.png"), width=250, height=200))



## -------------------------------------------------------------------------
#| label: scatterplots2
#| echo: false
#| eval: false
#| include: false
#| fig-width: 2
#| fig-height: 2

## d_form <- tibble(x = runif(100) - 0.5) |>
##   mutate(
##     linear = 4 * x + rnorm(100) * 0.5,
##     nonlinear1 = 12 * x^2 + rnorm(100) * 0.5,
##     nonlinear2 = 2 * x - 5 * x^2 + rnorm(100) * 0.1
##   ) |>
##   pivot_longer(
##     cols = linear:nonlinear2, names_to = "form",
##     values_to = "y"
##   ) |>
##   select(form, x, y)
## 
## d_form |>
##   filter(form == "linear") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_form |>
##   filter(form == "nonlinear1") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_form |>
##   filter(form == "nonlinear2") |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_outliers <- tibble(x = runif(100) - 0.5) |>
##   mutate(y = 4 * x + rnorm(100) * 0.5)
## d_outliers <- d_outliers |>
##   bind_rows(tibble(x = runif(5) / 10 - 0.45, y = 2 + rnorm(5) * 0.5))
## 
## d_outliers |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_clusters <- tibble(x = c(
##   rnorm(50) / 6 - 0.5,
##   rnorm(50) / 6,
##   rnorm(50) / 6 + 0.5
## )) |>
##   mutate(y = c(
##     rnorm(50) / 6,
##     rnorm(50) / 6 + 1, rnorm(50) / 6
##   ))
## 
## d_clusters |>
##   ggplot(aes(x = x, y = y)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## d_gaps <- tibble(x = runif(150)) |>
##   mutate(y = runif(150))
## d_gaps <- d_gaps |>
##   filter(!(between(x + 2 * y, 1.2, 1.6)))
## 
## d_gaps |>
##   ggplot(aes(x = x, y = y)) +
##   geom_polygon(
##     data = tibble(x = c(0, 1, 1, 0), y = c(1.2 / 2, 0.2 / 2, 0.6 / 2, 1.6 / 2)),
##     fill = "red", alpha = 0.3
##   ) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )


## -------------------------------------------------------------------------
#| label: feature-table2
#| echo: false
tribble(
  ~Feature, ~Example, ~Description,
  "linear form", "", "The shape is linear",
  "nonlinear form", "", "The shape is more of a curve",
  "nonlinear form", "", "The shape is more of a curve",
  "outliers", "", "There are one or more points that do not fit the pattern on the others",
  "clusters", "", "The observations group into multiple clumps",
  "gaps", "", "There is a gap, or gaps, but its not clumped"
) |>
  knitr::kable(escape = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=30, 
                            full_width=FALSE) |>
  kableExtra::column_spec(2, 
    image=spec_image(
      c("images/scatterplots2-1.png",
        "images/scatterplots2-2.png",
        "images/scatterplots2-3.png",
        "images/scatterplots2-4.png",
        "images/scatterplots2-5.png",
        "images/scatterplots2-6.png"), width=250, height=200))


## -------------------------------------------------------------------------
#| label: scatterplots3
#| echo: false
#| include: false
#| fig-width: 2
#| fig-height: 2
d_barrier <- tibble(x = runif(200)) |>
  mutate(y = runif(200))
d_barrier <- d_barrier |>
  filter(-x + 3 * y < 1.2)

d_barrier |>
  ggplot(aes(x = x, y = y)) +
  geom_polygon(
    data = tibble(x = c(0, 1, 1, 0), y = c(1.2 / 3, 2.2 / 3, 1, 1)),
    fill = "red", alpha = 0.3
  ) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    axis.line.x = element_line(color = "black", size = 2),
    axis.line.y = element_line(color = "black", size = 2)
  )

l_shape <- tibble(
  x = c(rexp(50, 0.01), runif(50) * 20),
  y = c(runif(50) * 20, rexp(50, 0.01))
)

l_shape |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    axis.line.x = element_line(color = "black", size = 2),
    axis.line.y = element_line(color = "black", size = 2)
  )

discrete <- tibble(x = rnorm(200)) |>
  mutate(y = -x + rnorm(25) * 0.1 + rep(0:7, 25)) |>
  filter((scale(x)^2 + scale(y)^2) < 2)

discrete |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    axis.line.x = element_line(color = "black", size = 2),
    axis.line.y = element_line(color = "black", size = 2)
  )

hetero <- tibble(x = runif(200) - 0.5) |>
  mutate(y = -2 * x + rnorm(200) * (x + 0.5))

hetero |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    axis.line.x = element_line(color = "black", size = 2),
    axis.line.y = element_line(color = "black", size = 2)
  )

weighted <- tibble(x = runif(50) - 0.5) |>
  mutate(
    y = -2 * x + rnorm(50) * 0.8,
    w = runif(50) * (x + 0.5)
  )

weighted |>
  ggplot(aes(x = x, y = y, size = w + 0.1)) +
  geom_point(alpha = 0.7) +
  scale_size_area(max_size = 6) +
  theme_void() +
  theme(
    aspect.ratio = 1, legend.position = "none",
    axis.line.x = element_line(color = "black", size = 2),
    axis.line.y = element_line(color = "black", size = 2)
  )


## -------------------------------------------------------------------------
#| label: feature-table3
#| echo: false
tribble(
  ~Feature, ~Example, ~Description,
  "barrier", "", "There is combination of the variables which appears impossible",
  "l-shape", "", "When one variable changes the other is approximately constant",
  "discreteness", "", "Relationship between two variables is different from the overall, and observations are in a striped pattern",
  "heteroskedastic", "", "Variation is different in different areas, maybe depends on value of x variable",
  "weighted", "", "If observations have an associated weight,  reflect in scatterplot, e.g. bubble chart"
) |>
  knitr::kable(escape = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=30, 
                            full_width=FALSE) |>
  kableExtra::column_spec(2, 
    image=spec_image(
      c("images/scatterplots3-1.png",
        "images/scatterplots3-2.png",
        "images/scatterplots3-3.png",
        "images/scatterplots3-4.png",
        "images/scatterplots3-5.png"), width=250, height=200))


## -------------------------------------------------------------------------
#| label: anscombe
#| echo: false
#| fig-width: 6
#| fig-height: 2
#| out-width: 100%
anscombe_tidy <- anscombe |>
  pivot_longer(cols = x1:y4, names_to = "var", values_to = "value") |>
  mutate(
    group = substr(var, 2, 2),
    var = substr(var, 1, 1),
    id = rep(1:11, rep(8, 11))
  ) |>
  pivot_wider(
    id_cols = c(id, group), names_from = var,
    values_from = value
  )
anscombe_tidy |>
  ggplot(aes(x = x, y = y)) +
  geom_point(colour = "orange", size = 3) +
  facet_wrap(~group, ncol = 4, scales = "free") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )


## -------------------------------------------------------------------------
#| label: dinosaur
#| echo: false
#| out-width: 30%
#| fig-width: 3
#| fig-height: 2.8
datasaurus_dozen |>
  filter(dataset == "dino") |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )


## -------------------------------------------------------------------------
#| label: datasaurus
#| echo: false
#| fig-height: 7
#| fig-width: 7
#| out-width: 60%

datasaurus_dozen |>
  filter(dataset != "dino") |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~dataset, ncol = 4) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )


## -------------------------------------------------------------------------
#| label: 2012-olympics
#| echo: false
data(oly12, package = "VGAMdata")


## -------------------------------------------------------------------------
#| label: 2012-olympics-plot1
#| echo: false
#| fig-width: 6.4
ggplot(oly12, aes(x = Height, y = Weight, label = Sport)) +
  geom_point()


## -------------------------------------------------------------------------
skimr::skim(oly12)


## -------------------------------------------------------------------------
#| label: 2012-olympics-plot1
#| echo: true
#| eval: false
## ggplot(oly12, aes(x = Height, y = Weight, label = Sport)) +
##   geom_point()


## -------------------------------------------------------------------------
#| eval: false
#| echo: true
## library(tidyverse)
## library(plotly)
## data(oly12, package = "VGAMdata")
## p <- ggplot(oly12, aes(x = Height, y = Weight, label = Sport)) +
##   geom_point()
## ggplotly(p)


## ----oly_smry-------------------------------------------------------------
#| echo: false
oly12 |>
  count(Sport, sort = TRUE) |>
  DT::datatable(
    rownames = FALSE,
    escape = FALSE,
    width = "900px",
    options = table_options(
      scrollY = "500px",
      title = "",
      csv = "oly12"
    ),
    callback = toggle_select
  )


## ----oly_cat, echo=TRUE---------------------------------------------------
oly12 <- oly12 |>
  mutate(Sport = as.character(Sport)) |>
  mutate(Sport = ifelse(grepl("Cycling", Sport), 
    "Cycling", Sport
  )) |> 
  mutate(Sport = ifelse(grepl("Gymnastics", Sport),
    "Gymnastics", Sport
  )) |>
  mutate(Sport = ifelse(grepl("Athletics", Sport),
    "Athletics", Sport
  )) |>
  mutate(Sport = as.factor(Sport))


## -------------------------------------------------------------------------
#| label: oly_facet
#| out-width: 70%
#| fig-width: 12
#| fig-height: 7
#| echo: false
ggplot(oly12, aes(x = Height, y = Weight)) +
  geom_point(alpha = 0.5) + 
  facet_wrap(~Sport, ncol = 8) +
  theme(aspect.ratio = 1) 


## -------------------------------------------------------------------------
#| label: oly_facet
#| echo: true
#| eval: false
## ggplot(oly12, aes(x = Height, y = Weight)) +
##   geom_point(alpha = 0.5) +
##   facet_wrap(~Sport, ncol = 8) +
##   theme(aspect.ratio = 1)


## -------------------------------------------------------------------------
#| label: oly_women
#| out-width: 70%
#| fig-width: 12
#| fig-height: 7
#| echo: false
oly12 |>
  filter(!(Sport %in% c("Boxing", "Gymnastics", "Synchronised Swimming", "Taekwondo", "Trampoline"))) |>
  mutate(Sport = fct_drop(Sport)) |>
  ggplot(aes(x = Height, y = Weight, colour = Sex)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Sport, ncol = 7, scales = "free") +
  scale_colour_brewer("", palette = "Dark2") +
  theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## -------------------------------------------------------------------------
#| label: oly_women
#| echo: true
#| eval: false
## oly12 |>
##   filter(!(Sport %in% c("Boxing", "Gymnastics", "Synchronised Swimming", "Taekwondo", "Trampoline"))) |>
##   mutate(Sport = fct_drop(Sport)) |>
##   ggplot(aes(x = Height, y = Weight, colour = Sex)) +
##   geom_point(alpha = 0.5) +
##   facet_wrap(~Sport, ncol = 7, scales = "free") +
##   scale_colour_brewer("", palette = "Dark2") +
##   theme(aspect.ratio = 1, axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## -------------------------------------------------------------------------
#| label: generate_data
#| echo: false
set.seed(2222)
df <- tibble(x = c(rnorm(500) * 0.2, runif(300) + 1)) |>
  mutate(
    x2 = c(rnorm(500), runif(300) - 0.5),
    y1 = c(
      -2 * x[1:500] + rnorm(500),
      3 * x[501:800] + rexp(300)
    ),
    y2 = c(rep("A", 500), rep("B", 300)),
    y3 = c(
      -2 * (x2[1:500]) + rnorm(500) * 2,
      2 * (x2[501:800]) + rnorm(300) * 0.5
    )
  )


## -------------------------------------------------------------------------
#| label: scatmodify
#| echo: false
#| eval: false
#| fig-width: 2
#| fig-height: 2
#| out-width: 100%

## ggplot(df, aes(x = x2, y = y3)) +
##   geom_point() +
##   xlab("") +
##   ylab("") +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## ggplot(df, aes(x = x2, y = y3)) +
##   geom_point(alpha = 0.1) +
##   xlab("") +
##   ylab("") +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## ggplot(df, aes(x = x2, y = y3)) +
##   geom_smooth(colour = "purple", se = F, size = 2, span = 0.2) +
##   xlab("") +
##   ylab("") +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## ggplot(df, aes(x = x2, y = y3)) +
##   geom_point(alpha = 0.2) +
##   geom_smooth(colour = "purple", se = F, size = 2, span = 0.2) +
##   xlab("") +
##   ylab("") +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## ggplot(df, aes(x = x, y = y1)) +
##   geom_density_2d(colour = "black") +
##   xlab("") +
##   ylab("") +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## ggplot(df, aes(x = x, y = y1)) +
##   geom_density_2d_filled() +
##   xlab("") +
##   ylab("") +
##   theme_void() +
##   theme(
##     legend.position = "none", aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## ggplot(df, aes(x = x, y = y1, colour = y2)) +
##   geom_point(alpha = 0.2) +
##   xlab("") +
##   ylab("") +
##   scale_colour_brewer("", palette = "Dark2") +
##   theme_void() +
##   theme(
##     legend.position = "none", aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## 
## ggplot(df, aes(x = x, y = y1, colour = y2)) +
##   geom_density2d() +
##   xlab("") +
##   ylab("") +
##   scale_colour_brewer("", palette = "Dark2") +
##   theme_void() +
##   theme(
##     legend.position = "none", aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )


## -------------------------------------------------------------------------
#| label: feature-table4
#| echo: false
tribble(
  ~Modification, ~Example, ~Purpose,
  "alpha-blend", "", "alleviate overplotting to examine density at centre",
  "model overlay", "", "focus on the trend",
  "model + data", "", "trend plus variation",
  "density", "", "overall distribution, variation and clustering",
  "filled density", "", "high density locations in distribution (modes), variation and clustering",
  "colour", "", "relationship with conditioning and lurking variables",
) |>
  knitr::kable(escape = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=30, 
                            full_width=FALSE) |>
  kableExtra::column_spec(2, 
    image=spec_image(
      c("images/scatmodify-2.png",
        "images/scatmodify-3.png",
        "images/scatmodify-4.png",
        "images/scatmodify-5.png",
        "images/scatmodify-6.png",
        "images/scatmodify-7.png"), width=250, height=200))


## -------------------------------------------------------------------------
#| label: oly_model
#| echo: false
#| out-width: 100%
#| fig-width: 10
#| fig-height: 8
oly12 |>
  filter(Sport %in% c(
    "Swimming", "Archery", "Basketball",
    "Handball", "Hockey", "Tennis",
    "Weightlifting", "Wrestling"
  )) |>
  filter(Sex == "F") |>
  mutate(Sport = fct_drop(Sport), Sex = fct_drop(Sex)) |>
  ggplot(aes(x = Height, y = Weight, colour = Sport)) +
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


## -------------------------------------------------------------------------
#| label: oly_model
#| echo: true
#| eval: false
## oly12 |>
##   filter(Sport %in% c(
##     "Swimming", "Archery", "Basketball",
##     "Handball", "Hockey", "Tennis",
##     "Weightlifting", "Wrestling"
##   )) |>
##   filter(Sex == "F") |>
##   mutate(Sport = fct_drop(Sport), Sex = fct_drop(Sex)) |>
##   ggplot(aes(x = Height, y = Weight, colour = Sport)) +
##   geom_smooth(method = "lm", se = FALSE) +
##   scale_color_discrete_divergingx(palette = "Zissou 1") +
##   theme(
##     legend.title = element_blank(),
##     legend.position = "bottom",
##     legend.direction = "horizontal"
##   )


## -------------------------------------------------------------------------
#| label: oly_density
#| echo: false
#| out-width: 100%
#| fig.width: 10
#| fig-height: 8

oly12 |>
  filter(Sport %in% c("Shooting", "Modern Pentathlon", "Basketball")) |> 
  filter(Sex == "F") |>
  mutate(Sport = fct_drop(Sport), Sex = fct_drop(Sex)) |>
  ggplot(aes(x = Height, y = Weight, colour = Sport)) +
  geom_density2d() + 
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


## -------------------------------------------------------------------------
#| label: oly_density
#| echo: true
#| eval: false
## oly12 |>
##   filter(Sport %in% c("Shooting", "Modern Pentathlon", "Basketball")) |>
##   filter(Sex == "F") |>
##   mutate(Sport = fct_drop(Sport), Sex = fct_drop(Sex)) |>
##   ggplot(aes(x = Height, y = Weight, colour = Sport)) +
##   geom_density2d() +
##   scale_color_discrete_divergingx(palette = "Zissou 1") +
##   theme(
##     legend.title = element_blank(),
##     legend.position = "bottom",
##     legend.direction = "horizontal"
##   )


## -------------------------------------------------------------------------
#| label: oly_canyousee
#| echo: false
#| fig-width: 8
#| fig-height: 8
#| out-width: 70%
p1 <- oly12 |>
  filter(Sport == "Athletics") |>
  ggplot(aes(x = Height, y = Weight)) +
  geom_point(alpha = 0.2, size = 4) 
p2 <- oly12 |>
  filter(Sport == "Athletics") |>
  ggplot(aes(x = Height, y = Weight)) +
  geom_density2d_filled() +
  theme(legend.position = "none")
p3 <- oly12 |>
  filter(Sport == "Athletics") |>
  ggplot(aes(x = Height, y = Weight)) +
  geom_density2d(binwidth = 0.01) 
p4 <- oly12 |>
  filter(Sport == "Athletics") |>
  ggplot(aes(x = Height, y = Weight)) +
  geom_density2d(binwidth = 0.001, color = "white", size = 0.2) +
  geom_density2d_filled(binwidth = 0.001) +
  theme(legend.position = "none")
grid.arrange(p1, p3, p2, p4, ncol = 2)


## -------------------------------------------------------------------------
#| label: oly_canyousee
#| echo: true
#| eval: false
## p1 <- oly12 |>
##   filter(Sport == "Athletics") |>
##   ggplot(aes(x = Height, y = Weight)) +
##   geom_point(alpha = 0.2, size = 4)
## p2 <- oly12 |>
##   filter(Sport == "Athletics") |>
##   ggplot(aes(x = Height, y = Weight)) +
##   geom_density2d_filled() +
##   theme(legend.position = "none")
## p3 <- oly12 |>
##   filter(Sport == "Athletics") |>
##   ggplot(aes(x = Height, y = Weight)) +
##   geom_density2d(binwidth = 0.01)
## p4 <- oly12 |>
##   filter(Sport == "Athletics") |>
##   ggplot(aes(x = Height, y = Weight)) +
##   geom_density2d(binwidth = 0.001, color = "white", size = 0.2) +
##   geom_density2d_filled(binwidth = 0.001) +
##   theme(legend.position = "none")
## grid.arrange(p1, p3, p2, p4, ncol = 2)


## -------------------------------------------------------------------------
#| label: oly_tennis
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
oly12 |>
  filter(Sport == "Tennis", Sex == "F") |>
  ggplot(aes(x = Height, y = Weight)) +
  geom_point(alpha = 0.9, size = 3)


## -------------------------------------------------------------------------
#| label: oly_tennis
#| echo: true
#| eval: false
## oly12 |>
##   filter(Sport == "Tennis", Sex == "F") |>
##   ggplot(aes(x = Height, y = Weight)) +
##   geom_point(alpha = 0.9, size = 3)


## -------------------------------------------------------------------------
#| label: oly_wrestling
#| fig-width: 6
#| fig-height: 6
#| out.width: 80%
oly12 |>
  filter(Sport == "Wrestling", Sex == "F") |>
  ggplot(aes(x = Height, y = Weight)) +
  geom_point(alpha = 0.9, size = 3)


## -------------------------------------------------------------------------
#| label: oly_wrestling
#| echo: true
#| eval: false
## oly12 |>
##   filter(Sport == "Wrestling", Sex == "F") |>
##   ggplot(aes(x = Height, y = Weight)) +
##   geom_point(alpha = 0.9, size = 3)


## -------------------------------------------------------------------------
#| echo: false
#| out-width: 80%
set.seed(45)
d1 <- tibble(x = runif(200) - 0.5) |>
  mutate(y = 2 * x + rnorm(200))
d1_p <- ggplot(d1, aes(x = x, y = y)) +
  geom_point() +
  theme(aspect.ratio = 1)
d1_p


## -------------------------------------------------------------------------
cor(d1$x, d1$y)


## ----echo=TRUE, highlight.output = c(5)-----------------------------------
cor.test(d1$x, d1$y)


## -------------------------------------------------------------------------
#| echo: false
d2 <- tibble(x = runif(200) - 0.5) |>
  mutate(y = x^2 + rnorm(200) * 0.01)
d2_p <- ggplot(d2, aes(x = x, y = y)) +
  geom_point() +
  theme(aspect.ratio = 1)
d2_p


## -------------------------------------------------------------------------
cor(d2$x, d2$y)


## ----highlight.output = c(5)----------------------------------------------
cor.test(d2$x, d2$y)


## -------------------------------------------------------------------------
#| echo: false
d3 <- tibble(x = c(rnorm(200), 10)) |>
  mutate(y = c(rnorm(200), 10))
d3_p <- ggplot(d3, aes(x = x, y = y)) +
  geom_point() +
  theme(aspect.ratio = 1)
d3_p


## ----echo=FALSE, highlight.output = c(2,3,9,10)---------------------------
cor.test(d3$x, d3$y)[c(4, 1, 3)]


## ----echo=FALSE, highlight.output = c(2,3,9,10)---------------------------
cor.test(d3$x[1:200], d3$y[1:200])[c(4, 1, 3)]


## -------------------------------------------------------------------------
#| label: simcor
#| echo: false
#| fig-width: 10
#| fig-height: 5
#| out-width: 70%
set.seed(7777)
vc <- matrix(c(1, 0, 0, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p1 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
vc <- matrix(c(1, 0.4, 0.4, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p2 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
vc <- matrix(c(1, 0.6, 0.6, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p3 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
vc <- matrix(c(1, 0.8, 0.8, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p4 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
vc <- matrix(c(1, -0.2, -0.2, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p5 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
vc <- matrix(c(1, -0.5, -0.5, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p6 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
vc <- matrix(c(1, -0.7, -0.7, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p7 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
vc <- matrix(c(1, -0.9, -0.9, 1), ncol = 2, byrow = T)
d <- as_tibble(rmvnorm(500, sigma = vc))
p8 <- ggplot(d, aes(x = V1, y = V2)) +
  geom_point() +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)


## -------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 2
#| out-width: 40%
a1 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = 0.0")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
a2 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = 0.4")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
a3 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = 0.6")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
a4 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = 0.8")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
a5 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = -0.2")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
a6 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = -0.5")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
a7 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = -0.7")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
a8 <- ggplot() +
  geom_text(aes(x = 0, y = 0, label = "r = -0.9")) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray90")
  )
grid.arrange(a1, a2, a3, a4, a5, a6, a7, a8, ncol = 4)


## -------------------------------------------------------------------------
#| label: simcor
#| echo: true
#| eval: false
## set.seed(7777)
## vc <- matrix(c(1, 0, 0, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p1 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## vc <- matrix(c(1, 0.4, 0.4, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p2 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## vc <- matrix(c(1, 0.6, 0.6, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p3 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## vc <- matrix(c(1, 0.8, 0.8, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p4 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## vc <- matrix(c(1, -0.2, -0.2, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p5 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## vc <- matrix(c(1, -0.5, -0.5, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p6 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## vc <- matrix(c(1, -0.7, -0.7, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p7 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## vc <- matrix(c(1, -0.9, -0.9, 1), ncol = 2, byrow = T)
## d <- as_tibble(rmvnorm(500, sigma = vc))
## p8 <- ggplot(d, aes(x = V1, y = V2)) +
##   geom_point() +
##   theme_void() +
##   theme(
##     aspect.ratio = 1,
##     plot.background = element_rect(fill = "gray90")
##   )
## grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)


## -------------------------------------------------------------------------
set.seed(60)
df <- tibble(
  x = c(round(rnorm(5), 1), 10),
  y = c(round(rnorm(5), 1), 10)
) |>
  mutate(xr = rank(x), yr = rank(y))
df


## ----echo=TRUE------------------------------------------------------------
cor(df$x, df$y)
cor(df$xr, df$yr)
cor(df$x, df$y, method = "spearman")


## -------------------------------------------------------------------------
#| echo: false
#| out-width: 70%
ggplot(df, aes(x = x, y = y)) +
  annotate("rect",
    xmin = -2, xmax = df$x[2],
    ymin = -2, ymax = df$y[2],
    fill = "red", alpha = 0.5, colour = NA
  ) +
  annotate("rect",
    xmin = df$x[2], xmax = 11,
    ymin = df$y[2], ymax = 11,
    fill = "red", alpha = 0.5, colour = NA
  ) +
  annotate("text", x = 3, y = 10, label = "concordant", colour = "red") +
  annotate("text", x = 5, y = -1.5, label = "disconcordant") +
  geom_point() +
  annotate("point", x = df$x[2], y = df$y[2], color = "red") +
  theme(aspect.ratio = 1)


## ----echo=TRUE------------------------------------------------------------
cor(df$x, df$y)
cor(df$x, df$y, method = "kendall")


## -------------------------------------------------------------------------
#| label: diffscatter
#| echo: false
#| eval: false
#| fig-height: 2
#| fig-width: 2
## d1_p +
##   theme_void() +
##   theme(
##     legend.position = "none", aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## d2_p +
##   theme_void() +
##   theme(
##     legend.position = "none", aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )
## d3_p +
##   theme_void() +
##   theme(
##     legend.position = "none", aspect.ratio = 1,
##     axis.line.x = element_line(color = "black", size = 2),
##     axis.line.y = element_line(color = "black", size = 2)
##   )


## -------------------------------------------------------------------------
#| label: corr-table
#| echo: false
tribble(
  ~sample, ~corr, ~spearman, ~kendall,
  "",
  cor(d1$x, d1$y),
  cor(d1$x, d1$y, method = "spearman"),
  cor(d1$x, d1$y, method = "kendall"),
  "",
  cor(d2$x, d2$y),
  cor(d2$x, d2$y, method = "spearman"),
  cor(d2$x, d2$y, method = "kendall"),
  "",
  cor(d3$x, d3$y),
  cor(d3$x, d3$y, method = "spearman"),
  cor(d3$x, d3$y, method = "kendall")
) |>
  knitr::kable(escape = FALSE, digits = 3) |>
  kableExtra::kable_styling(full_width = FALSE) |>
  kableExtra::kable_classic() |>
  kableExtra::kable_styling(font_size=30, 
                            full_width=FALSE) |>
  kableExtra::column_spec(1, 
    image=spec_image(
      c("images/diffscatter-1.png",
        "images/diffscatter-2.png",
        "images/diffscatter-3.png"), width=250, height=200))


## -------------------------------------------------------------------------
#| label: movies
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
#| echo: false
ggplot(movies, aes(x = votes, y = rating)) +
  geom_point() +
  scale_y_continuous("rating", breaks = seq(0, 10, 2))


## -------------------------------------------------------------------------
#| label: movies
#| echo: true
#| eval: false
## ggplot(movies, aes(x = votes, y = rating)) +
##   geom_point() +
##   scale_y_continuous("rating", breaks = seq(0, 10, 2))


## -------------------------------------------------------------------------
#| label: logmovies
#| echo: false
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
ggplot(movies, aes(x = votes, y = rating)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = F, colour = "orange", size = 2) +
  scale_x_log10() +
  scale_y_continuous("rating", breaks = seq(0, 10, 2))


## -------------------------------------------------------------------------
#| label: logmovies
#| echo: true
#| eval: false
## ggplot(movies, aes(x = votes, y = rating)) +
##   geom_point(alpha = 0.1) +
##   geom_smooth(se = F, colour = "orange", size = 2) +
##   scale_x_log10() +
##   scale_y_continuous("rating", breaks = seq(0, 10, 2))


## -------------------------------------------------------------------------
#| label: cars
#| echo: false
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
data(mtcars)
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(colour = "forestgreen", se = F)


## -------------------------------------------------------------------------
#| label: cars
#| echo: true
#| eval: false
## data(mtcars)
## ggplot(mtcars, aes(x = hp, y = mpg)) +
##   geom_point() +
##   geom_smooth(colour = "forestgreen", se = F)


## -------------------------------------------------------------------------
#| label: logcars
#| echo: false
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  scale_y_log10("log mpg") + 
  geom_smooth(method = "lm", colour = "forestgreen", se = F) +
  geom_smooth(data = filter(mtcars, hp < 300), method = "lm", colour = "orangered", se = F, lty = 2)


## -------------------------------------------------------------------------
#| label: logcars
#| echo: true
#| eval: false
## ggplot(mtcars, aes(x = hp, y = mpg)) +
##   geom_point() +
##   scale_y_log10("log mpg") +
##   geom_smooth(method = "lm", colour = "forestgreen", se = F) +
##   geom_smooth(data = filter(mtcars, hp < 300), method = "lm", colour = "orangered", se = F, lty = 2)


## -------------------------------------------------------------------------
#| label: circleoftrans
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 80%
x <- c(seq(-0.95, -0.1, 0.025), seq(0.1, 0.95, 0.025))
x <- c(x, x)
d <- tibble(x,
  y = sqrt(1 - x^2),
  q = c(
    rep("4", 35),
    rep("1", 35),
    rep("3", 35),
    rep("2", 35)
  )
)
d$y[71:140] <- -d$y[71:140]
ggplot(d, aes(x = x, y = y, colour = q, group = q)) +
  geom_line(size = 2) +
  annotate("text",
    x = c(0.5, 0.5, -0.5, -0.5),
    y = c(0.5, -0.5, -0.5, 0.5),
    label = c(
      "x up, y up", "x up, y down",
      "x down, y down", "x down, y up"
    ),
    size = 5
  ) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  annotate("text", x = 0, y = 0, label = "(0,0)", size = 10) +
  theme_void() +
  theme(aspect.ratio = 1, legend.position = "none")


## -------------------------------------------------------------------------
#| label: baker
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 80%
baker <- read_csv(here("data/baker.csv"))
p <- ggplot(baker, aes(x = B, y = Corn97BU)) +
  geom_point() +
  xlab("Boron (ppm)") +
  ylab("Corn Yield (bushells)")
ggMarginal(p, type = "density")


## -------------------------------------------------------------------------
#| label: transfbaker
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 80%
p <- ggplot(
  baker,
  aes(x = B, y = Corn97BU^2)
) + 
  geom_point() +
  xlab("log Boron (ppm)") +
  ylab("Corn Yield^2 (bushells)") +
  scale_x_log10() 
ggMarginal(p, type = "density") 


## -------------------------------------------------------------------------
#| label: transfbaker
#| echo: true
#| eval: false
## p <- ggplot(
##   baker,
##   aes(x = B, y = Corn97BU^2)
## ) +
##   geom_point() +
##   xlab("log Boron (ppm)") +
##   ylab("Corn Yield^2 (bushells)") +
##   scale_x_log10()
## ggMarginal(p, type = "density")


## -------------------------------------------------------------------------
#| label: bakeriron
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 80%
p <- ggplot(
  baker,
  aes(x = Fe, y = Corn97BU^2)
) +
  geom_density2d(colour = "orange") +
  geom_point() +
  xlab("Iron (ppm)") + 
  ylab("Corn Yield^2 (bushells)")
ggMarginal(p, type = "density")


## -------------------------------------------------------------------------
#| label: bakeriron
#| echo: true
#| eval: false
## p <- ggplot(
##   baker,
##   aes(x = Fe, y = Corn97BU^2)
## ) +
##   geom_density2d(colour = "orange") +
##   geom_point() +
##   xlab("Iron (ppm)") +
##   ylab("Corn Yield^2 (bushells)")
## ggMarginal(p, type = "density")


## -------------------------------------------------------------------------
#| label: bakerironca
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 100%
ggplot(baker, aes(
  x = Fe, y = Corn97BU^2,
  colour = ifelse(Ca > 5200, 
    "high", "low"
  )
)) + 
  geom_point() +
  xlab("Iron (ppm)") +
  ylab("Corn Yield^2 (bushells)") +
  scale_colour_brewer("", palette = "Dark2") +
  theme(
    aspect.ratio = 1,
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


## -------------------------------------------------------------------------
#| label: bakerironca
#| echo: true
#| eval: false
## ggplot(baker, aes(
##   x = Fe, y = Corn97BU^2,
##   colour = ifelse(Ca > 5200,
##     "high", "low"
##   )
## )) +
##   geom_point() +
##   xlab("Iron (ppm)") +
##   ylab("Corn Yield^2 (bushells)") +
##   scale_colour_brewer("", palette = "Dark2") +
##   theme(
##     aspect.ratio = 1,
##     legend.position = "bottom",
##     legend.direction = "horizontal"
##   )


## -------------------------------------------------------------------------
#| label: prepdata
#| echo: false
#| eval: false
## # Read data
## nyt_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
## 
## # Add centroid lat/long
## geoCounty_new <- geoCounty |>
##   as_tibble() |>
##   mutate(
##     fips = as.character(fips),
##     county = as.character(county),
##     state = as.character(state)
##   ) |>
##   select(fips, lon, lat)
## nyt_county_total <- nyt_county |>
##   group_by(fips) |>
##   filter(date == max(date)) |>
##   left_join(geoCounty_new, by = c("fips" = "fips"))
## 
## save(nyt_county_total, file = "../data/nyt_covid.rda")


## -------------------------------------------------------------------------
#| label: usacovid
#| echo: false
#| fig-width: 10
#| fig-height: 6.7
#| out-width: 60%
load(here("data/nyt_covid.rda"))
usa <- map_data("state")
ggplot() +
  geom_polygon(
    data = usa,
    aes(x = long, y = lat, group = group),
    fill = "grey90", colour = "white"
  ) +
  geom_point(
    data = nyt_county_total,
    aes(x = lon, y = lat, size = cases),
    colour = "red", shape = 1
  ) +
  geom_point(
    data = nyt_county_total,
    aes(x = lon, y = lat, size = cases),
    colour = "red", fill = "red", alpha = 0.1, shape = 16
  ) +
  scale_size("", range = c(1, 30)) +
  theme_map() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------
#| label: usacovid
#| echo: true
#| eval: false
## load(here("data/nyt_covid.rda"))
## usa <- map_data("state")
## ggplot() +
##   geom_polygon(
##     data = usa,
##     aes(x = long, y = lat, group = group),
##     fill = "grey90", colour = "white"
##   ) +
##   geom_point(
##     data = nyt_county_total,
##     aes(x = lon, y = lat, size = cases),
##     colour = "red", shape = 1
##   ) +
##   geom_point(
##     data = nyt_county_total,
##     aes(x = lon, y = lat, size = cases),
##     colour = "red", fill = "red", alpha = 0.1, shape = 16
##   ) +
##   scale_size("", range = c(1, 30)) +
##   theme_map() +
##   theme(legend.position = "none")


## -------------------------------------------------------------------------
#| label: usacovid
#| echo: false
#| eval: true
#| fig-width: 10
#| fig-height: 6.7
#| out-width: 70%
load(here("data/nyt_covid.rda"))
usa <- map_data("state")
ggplot() +
  geom_polygon(
    data = usa,
    aes(x = long, y = lat, group = group),
    fill = "grey90", colour = "white"
  ) +
  geom_point(
    data = nyt_county_total,
    aes(x = lon, y = lat, size = cases),
    colour = "red", shape = 1
  ) +
  geom_point(
    data = nyt_county_total,
    aes(x = lon, y = lat, size = cases),
    colour = "red", fill = "red", alpha = 0.1, shape = 16
  ) +
  scale_size("", range = c(1, 30)) +
  theme_map() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------
#| label: generate_more_data
#| echo: false
set.seed(2222)
df <- tibble(x = c(rnorm(500) * 0.2, runif(300) + 1)) |>
  mutate(
    y1 = c(
      -2 * x[1:500] + rnorm(500),
      3 * x[501:800] + rexp(300)
    ),
    y2 = c(rep("A", 500), rep("B", 300))
  )


## -------------------------------------------------------------------------
#| label: scat
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| out-width: 70%
ggplot(df, aes(x = x, y = y1)) +
  geom_point(alpha = 0.5) +
  xlab("") +
  ylab("") +
  annotate("text", x = 0, y = 8, label = paste0("r = ", round(cor(df$x, df$y1), 2))) +
  theme_minimal()


## -------------------------------------------------------------------------
#| label: scatcol
#| echo: false
#| fig-width: 3
#| fig-height: 3
#| out-width: 70%
ggplot(df, aes(x = x, y = y1, colour = y2)) +
  geom_point(alpha = 0.5) +
  xlab("") +
  ylab("") +
  scale_colour_brewer("", palette = "Dark2") +
  annotate("text", x = 0, y = 8, label = paste0("r = ", round(cor(df$x[df$y2 == "A"], df$y1[df$y2 == "A"]), 2)), colour = "forestgreen") +
  annotate("text", x = 1.5, y = 0, label = paste0("r = ", round(cor(df$x[df$y2 == "B"], df$y1[df$y2 == "B"]), 2)), colour = "orangered") +
  theme_minimal() +
  theme(legend.position = "none")


## -------------------------------------------------------------------------
#| label: berkeley
#| echo: false
#| fig-width: 10
#| fig-height: 4
#| out-width: 90%
ucba <- as_tibble(UCBAdmissions)
a <- ggplot(ucba, aes(Dept)) +
  geom_bar(aes(weight = n))
b <- ggplot(ucba, aes(Admit)) +
  geom_bar(aes(weight = n)) +
  facet_wrap(~Gender)
grid.arrange(a, b, ncol = 2)


## -------------------------------------------------------------------------
#| label: berkeleydd
#| echo: false
#| fig-width: 10
#| fig-height: 4
#| out-width: 100%

library(vcd)
ucba <- ucba |>
  mutate(
    Admit = factor(Admit,
      levels = c("Rejected", "Admitted")
    ),
    Gender = factor(Gender,
      levels = c("Male", "Female"),
      labels = c("M", "F")
    )
  )
doubledecker(xtabs(n ~ Dept + Gender + Admit, data = ucba),
  gp = gpar(fill = c("grey90", "orangered"))
)


## -------------------------------------------------------------------------
#| label: soils-lineup
#| echo: false
#| fig-width: 10
#| fig-height: 7
#| out-width: 60%
ggplot(
  lineup(null_permute("Corn97BU"), baker, n = 12),
  aes(x = B, y = Corn97BU)
) +
  geom_point() +
  facet_wrap(~.sample, ncol = 4)


## -------------------------------------------------------------------------
#| label: soils-lineup
#| echo: true
#| eval: false
## ggplot(
##   lineup(null_permute("Corn97BU"), baker, n = 12),
##   aes(x = B, y = Corn97BU)
## ) +
##   geom_point() +
##   facet_wrap(~.sample, ncol = 4)


## -------------------------------------------------------------------------
#| label: oly-lineup
#| echo: false
#| fig-width: 10
#| fig-height: 7
#| out-width: 60%
data(oly12, package = "VGAMdata")
oly12_sub <- oly12 |>
  filter(Sport %in% c(
    "Swimming", "Archery",
    "Hockey", "Tennis"
  )) |>
  filter(Sex == "F") |>
  mutate(Sport = fct_drop(Sport), Sex = fct_drop(Sex))

ggplot(
  lineup(null_permute("Sport"), oly12_sub, n = 12),
  aes(x = Height, y = Weight, colour = Sport)
) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_brewer("", palette = "Dark2") +
  facet_wrap(~.sample, ncol = 4) +
  theme(legend.position = "none")


## -------------------------------------------------------------------------
#| label: oly-lineup
#| echo: true
#| eval: false
## data(oly12, package = "VGAMdata")
## oly12_sub <- oly12 |>
##   filter(Sport %in% c(
##     "Swimming", "Archery",
##     "Hockey", "Tennis"
##   )) |>
##   filter(Sex == "F") |>
##   mutate(Sport = fct_drop(Sport), Sex = fct_drop(Sex))
## 
## ggplot(
##   lineup(null_permute("Sport"), oly12_sub, n = 12),
##   aes(x = Height, y = Weight, colour = Sport)
## ) +
##   geom_smooth(method = "lm", se = FALSE) +
##   scale_colour_brewer("", palette = "Dark2") +
##   facet_wrap(~.sample, ncol = 4) +
##   theme(legend.position = "none")


## -------------------------------------------------------------------------
#| label: check-missings
#| fig-width: 5
#| fig-height: 4
#| out-width: 100%
#| code-fold: true
ggplot(oceanbuoys,
       aes(x = air_temp_c,
           y = humidity)) +
     geom_miss_point()


## -------------------------------------------------------------------------
#| label: check-missings-year
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
#| code-fold: true
ggplot(oceanbuoys,
       aes(x = air_temp_c,
           y = humidity)) +
     geom_miss_point() +
     facet_wrap(~year, ncol=2) +
     theme(legend.position = "none")


## -------------------------------------------------------------------------
#| label: impute-missings
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
#| code-fold: true

ocean_imp_yr_mean <- oceanbuoys |>
  select(air_temp_c, humidity, year) |>
  bind_shadow() |>
  group_by(year) |>
  impute_mean_at(vars(air_temp_c, humidity)) |>
  ungroup() |>
  add_label_shadow()
  
ggplot(ocean_imp_yr_mean,
       aes(x = air_temp_c,
           y = humidity,
           colour = any_missing)) + 
  geom_miss_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(legend.title = element_blank(),
        legend.position = "none")


## -------------------------------------------------------------------------
#| label: impute-missings-random
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
#| code-fold: true
ocean_imp_yr_sim <- nabular(oceanbuoys) |>
  select(air_temp_c, humidity, year) |>
  bind_shadow() |>
  add_label_shadow()

# Need to operate on each subset
ocean_imp_yr_sim_93 <- ocean_imp_yr_sim |>
  filter(year == 1993)
ocean_imp_yr_sim_97 <- ocean_imp_yr_sim |>
  filter(year == 1997)

ocean_imp_yr_sim_93 <- VIM::hotdeck(ocean_imp_yr_sim_93) 
ocean_imp_yr_sim_97 <- VIM::hotdeck(ocean_imp_yr_sim_97) 
  
ocean_imp_yr_sim <- bind_rows(ocean_imp_yr_sim_93, ocean_imp_yr_sim_97)  

ggplot(ocean_imp_yr_sim,
       aes(x = air_temp_c,
           y = humidity,
           colour = any_missing)) + 
  geom_miss_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(legend.title = element_blank(),
        legend.position = "none")

