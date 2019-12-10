# load libraries
library(dplyr)
library(ggplot2)

# load data
download.file("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_data.csv", destfile = "/Users/damian_wollny/Desktop/gapminder.project/data/gapminder_data.csv")
gapminder <- read.csv("/Users/damian_wollny/Desktop/gapminder.project/data/gapminder_data.csv")
summary(gapminder)

# clean up data (make two subsets)
asian_counties = c("India", "Thailand", "China", "Japan", "Vietnam")
gapminder %>% filter(country %in% asian_counties)
write.csv(asia_gapminder, file = "/Users/damian_wollny/Desktop/gapminder.project/processed-data/gapminder_se_asia.csv", row.names = F)
gapminder_2007 <- gapminder %>% filter(year == 2007)
write.csv(gapminder_2007, file = "/Users/damian_wollny/Desktop/gapminder.project/processed-data/gapminder_2007.csv", row.names = F)

# sanity check
dim(asia_gapminder)
dim(gapminder_2007)

# simple analysis
gapminder$gdp <- gapminder$gdpPercap * gapminder$pop
mod <- lm(pop~lifeExp, data = gapminder)
class(mod)
str(mod)
summary(mod)
attributes(mod) # attributes are everything that you can access via the '$' sign

mod_residuals <- mod$residuals
mod_coefficients <- mod$coefficients

# calculating summary stats in base R
# learn tapply
tapply(gapminder$gdp, gapminder$continent, sum) # use sum without ()
tapply(gapminder$gdp, list(gapminder$continent,gapminder$year), sum) # cool! generates a matrix!

# base R graphics

# ggplot2
pdf("/Users/damian_wollny/Desktop/gapminder.project/data/figure1.pdf", height = 5, width = 7)
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(pch = 21, size = 2) +
  scale_x_continuous(trans = "log10") +
  geom_smooth(method = "lm")
dev.off()

ggplot(data = gapminder, mapping = aes(x = year, y = lifeExp, by = country, color = continent)) +
  geom_line() 
  
  

