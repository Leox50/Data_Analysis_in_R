library(stargazer)
library(corrr)
library(car)
library(readxl)
library(stargazer)
library(car)
library(tseries)
library(foreign)
library(tsibble)
library(tsibbledata)
library(feasts)
library(fable)
library(urca)
library(dplyr)
library(tidyverse)
library(astsa)
library(ggplot2)
library(corrplot)

#Slide 3. Histogram.
#Choosing the variables and the correlogram

data1 <- read_xlsx("data_prep.csv.xlsx", sheet = "clean")
view(data1)

#we are interested in the effect of the following variables:

data2 <- data1 %>% 
  select(.,c("Price","Traffic","tld","BL","DP","SG","CPCUS(USD)","Bids","Traffic","Valuation","Endtime"))

names_num <- c()

#lets pay attention to our dependent variable.
#for this purpose we will plot a histogram to see it's distribution.

ggplot(data2, aes(x=data2$Price)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)

#it doesn't look good. lets try to make it smoother by removing the outliers (95% quantile).

# data with removed outliers
quantile(data2$Price, na.rm=TRUE, 0.95)

data4 <- data2 %>% 
  filter(., Price <= 10050.8)

#lets see how many observations have we removed
nrow(data2) - nrow(data4)
#it's okay. we can proceed.

ggplot(data4, aes(x=data4$Price)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)

#now our distribution is still bad, but a bit smoother comparing to what we had.

#Slide 4. Correlation. Corrplot.
#we are interested in correlation between only quantative data
#so, for this purpose we will take only numeric type of data

for (i in (1:ncol(data4))) {
  if (sapply(data4[i], class)[1] == "numeric") {
    names_num <- c(names_num, names(data4[i]))
  }
} 

data5 <- data4 %>% 
  select(.,names_num)

corr_all <- cor(data5, use="pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(corr_all[,'Price'], decreasing = TRUE))
cor_sorted

#our correlations are pretty weak.
#lets plot a graph to confirm that.

corrplot(corr_all, method = 'shade', order = 'AOE', diag = FALSE, tl.col = 'black')

corrplot(corr_all, is.corr = FALSE, col.lim = c(min(cor_sorted), max(cor_sorted)+0.5), method = 'color', tl.pos = 'n',
         col = COL1('Purples'), cl.pos = 'b', addgrid.col = 'white', addCoef.col = 'black')

#descriptive statistics


