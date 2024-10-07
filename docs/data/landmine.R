# Generating the landmine data
# mystery data generated using locator function
plot(-1:1, -1:1, type = "n", xlab = "Re", ylab = "Im")
text(x=0, y=0, labels="you found me", cex=5)
d <- locator(512)
plot(d$x, d$y)
save(d, file = "data/mystery.rda")
d2 <- d
d2$y <- d2$y
d3$x <- d2$x*cos(pi/6) - d2$y*sin(5*pi/6)
d3 <- d2
d3$x <- d2$x*cos(pi/6) - d2$y*sin(pi/6)
d3$y <- d2$x*sin(pi/6) + d2$y*cos(pi/6)
d3 <- as_tibble(d3)
df <- tibble(x=runif(7500, -2, 2), y=runif(7500, -2, 2))
df <- bind_rows(df, d3)
df <- df[sample(1:nrow(df)),]
write_csv(df, file="data/landmine1.csv")

df4 <- read_csv("data/landmine1.csv")
df4 <- df4 |>
filter(!((x-1)^2+(y-1)^2)<0.01) |>
filter(!((x+1)^2+(y+1)^2)<0.01) |>
filter(!((x+1)^2+(y-1)^2)<0.01) |>
filter(!((x-1)^2+(y+1)^2)<0.01)
ggplot(df4, aes(x, y)) + geom_point(alpha=0.1)
ggplot(df4, aes(x, y)) + geom_point()
ggplot(df4, aes(x, y)) + geom_density_2d()
ggplot(df4, aes(x, y)) + geom_density_2d_filled()
ggplot(lineup(null_permute("x"), df4), aes(x, y)) + geom_point(alpha=0.5) + facet_wrap(~.sample, ncol=5)
ggplot(lineup(null_permute("x"), df4), aes(x, y)) + geom_point(alpha=0.1) + facet_wrap(~.sample, ncol=5)



