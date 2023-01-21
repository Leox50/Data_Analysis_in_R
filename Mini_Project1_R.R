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

getwd()

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

#Slide 5.
#tld variable. barplot.

data_to_use <- as.data.frame(data2) 

typeof(data_to_use$tld)

#as well as we have a character type of data, there is no sense in descriptive statists except separating 
#observations by the type of domain

#lets see what values are unique

uniq_groups <- unique(data_to_use$tld)

#lets group by all the values to see what number of observations there are in each group

agg_tld <- data_to_use %>% group_by(tld) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

agg_tld_srt <- agg_tld[order(agg_tld$total_count,decreasing = TRUE),]

#I will be interested only in 10 first TOP endings of the site

bar_data <- agg_tld_srt %>% 
  filter(.,total_count >= 21)

top_10_dom <- pull(bar_data, "tld")

bar_data_1 <- data_to_use %>% 
  filter(., tld %in% top_10_dom)

ggplot(bar_data_1, aes(x=as.factor(tld))) +
  geom_bar(colour = "black",fill="#69b3a2")

#as a conclusion of this graph we can say that there are
#not a lot of options to purchase a site with another domain

#does it affect the price?
#we can check it with adding another variable 
#and by using a scatter plot

ggplot(bar_data_1) +
  aes(x = bar_data_1$`CPCUS(USD)`, y = Price) +
  geom_point(aes(color = tld, shape = tld))

