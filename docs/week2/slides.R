## ----include = FALSE-----------------------------------
source("../setup.R")


## ------------------------------------------------------
options(width=20)
chevrolets <- tibble(
  prices = c(250, 150, 795, 895, 695, 
               1699, 1499, 1099, 1693, 1166,
               688, 1333, 895, 1775, 895,
               1895, 795))
#chevrolets$prices


## ------------------------------------------------------
#| echo: false
fabric_drawing(cid = "canvas1", 
               cwidth = 1050, 
               cheight = 450, 
               cfill = "whitesmoke", 
               drawingWidth = 3, 
               gumSize = 10)
fabric_text_add(cid = "canvas1", textId = "txt1",
                text = "250, 150, 795, 895, 695,  ",
                left = 10, top = 10, 
                fontFamily = "Courier", fontSize = 28)
fabric_text_add(cid = "canvas1", textId = "txt2",
                text = "1699, 1499, 1099, 1693,",
                left = 10, top = 60, 
                fontFamily = "Courier", fontSize = 28)
fabric_text_add(cid = "canvas1", textId = "txt3",
                text = "1166, 688, 1333, 895,",
                left = 10, top = 110, 
                fontFamily = "Courier", fontSize = 28)
fabric_text_add(cid = "canvas1", textId = "txt3",
                text = "1775, 895, 1895, 795",
                left = 10, top = 160, 
                fontFamily = "Courier", fontSize = 28)
#fabric_curtail(cid = "canvas1", imgsrc = #"images/lecture-02A/chevy.png", 
#                    type = "background")


## ----stem, echo=TRUE-----------------------------------
chevrolets$prices
stem(chevrolets$prices)


## ----echo=FALSE----------------------------------------
options(width=80, digits=3)
tips <- read_csv("http://ggobi.org/book/data/tips.csv")
head(tips$tip, 50)


## ----stem_tips, echo=TRUE------------------------------
stem(tips$tip, scale=0.5, width=120)


## ----echo=FALSE----------------------------------------
options(width=100)


## ----stem_tips2, echo=TRUE-----------------------------
stem(tips$tip, scale=2)


## ------------------------------------------------------
median(tips$tip)


## ----echo=FALSE----------------------------------------
options(width=30)
tips$sex[1:72]


## ------------------------------------------------------
#| echo: false
fabric_drawing(cid = "canvas2", 
               cwidth = 800, 
               cheight = 750, 
               cfill = "whitesmoke", 
               drawingWidth = 3, 
               gumSize = 10)


## ------------------------------------------------------
#| echo: false
options(width=25)
x <- c(-3.2, -1.7, -0.4, 0.1, 0.3, 1.2, 1.5, 1.8, 2.4, 3.0, 4.3, 6.4, 9.8)
x


## ------------------------------------------------------
#| echo: false
fabric_drawing(cid = "canvas3", 
               cwidth = 750, 
               cheight = 600, 
               cfill = "whitesmoke", 
               drawingWidth = 3, 
               gumSize = 10)


## ------------------------------------------------------
#| echo: false
fabric_drawing(cid = "canvas4", 
               cwidth = 750, 
               cheight = 600, 
               cfill = "whitesmoke", 
               drawingWidth = 3, 
               gumSize = 10)


## ----lvplot, echo=TRUE---------------------------------
library(lvplot)
p <- ggplot(mpg, 
            aes(class, hwy))
p + geom_lv(aes(fill=..LV..)) + 
  scale_fill_brewer() + 
  coord_flip() + 
  xlab("")

