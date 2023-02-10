#for reading files
library(readxl)

#for correlation:
library(corrr)
library(car)

#for tidy data:
library(dplyr)
library(tidyverse)

#for graphs:
library(ggplot2)
library(corrplot)
library(ggalt)
library(ggfortify)

#Slide 3. Histogram.
#Choosing the variables and the correlogram

data1 <- read_xlsx("data_prep.csv.xlsx", sheet = "clean")
view(data1)

#we are interested in the effect of the following variables:

data2 <- data1 %>% 
  select(.,c("Price","Traffic","tld","BL","DP","SG","CPCUS(USD)","Bids","Traffic","Valuation","Endtime","Reg32","Free32","Alexa"))

names_num <- c()

#lets pay attention to our dependent variable.
#for this purpose we will plot a histogram to see it's distribution.

ggplot(data2, aes(x=data2$Price)) +
  geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6) +   
  labs(title="Price Distribution Rough", 
       caption="Graph: Histogram",x = "Price", y = "Density")

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
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6) +   
  labs(title="Price Distribution Cleaned", 
       caption="Graph: Histogram",x = "Price", y = "Density")

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

corrplot(corr_all, method = 'shade', order = 'AOE', diag = FALSE, tl.col = 'black', title="Correlation", mar=c(10,0,5,0),sub="Graph: Correlation heatmap")   

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
  geom_bar(colour = "black",fill="#69b3a2") +   
  labs(x="Endings",title="Total count by ending", 
       caption="Graph: Barplot")

#as a conclusion of this graph we can say that there are
#not a lot of options to purchase a site with another domain

#does it affect the price?
#we can check it with a separate boxplots for all endings.

ggplot(data_a_rank_new, aes(x=as.factor(tld), y=Price))+
  geom_boxplot(col='blue') + labs(x='Ending') +   
  labs(x="Endings",title="Price distridution by endings", 
       caption="Graph: Boxplot")

#From the graph we can see that the price actually depends on some domains.
#For example, .com ending has much more outliers that are closer to higher values,
#.net, .org and .us have higher prices for sites in general.

#Slide 6. «reg32» & «free32». 

summary(data_to_use$Free32)
summary(data_to_use$Reg32)

#despite of a high correlation between these two variables,
#the descriptive statistics is a bit differs, so lets check
#the difference on a graph

#Scatterplot

ggplot(data_to_use, aes(Free32, Price)) +
       geom_point(col='blue') + geom_point(aes(Reg32, Price),color = "tomato") +   
  labs(x="Free32 & Reg32",title="Type of TLD on Price", 
       caption="Graph: Scatterplot")

#as the result, we can see that despite of presence of difference between
#Free32 and Reg32, there is no significant difference on the graph, so, it is
#not right to think that in general we prices on Reg32 a higher.

#Slide 7. Alexa rank and traffic 
#We have decided to combine these variables as well as these
#variables both stand for the reliability of the site and are great
#estimators of the site's wealth. Traffic - a pure variable
#that shows how many people visit websites.
#Alexa rank has it's own analytics and grading system that focuses mostly
#on traffics and the amount of backlinks.
#The goal of this slide is to define either there is any influence on the
#price because of the high rates of Alexa and if it always means that the higher
#traffic equals to a higher rate on Alexa.

#Firstly, lets check their descriptive statistics.

summary(data_to_use$Traffic)
summary(data_to_use$Alexa)

#For the Alexa's rank to be defined properly, lets divide our observations in three groups.
#a_top - good rate according to Alexa's specifications
#a_med - medium rate according to Alexa's specifications
#a_low - low rate according to Alexa's specifications

#lets say that 1 - 250 000 is a good one, 250 000 - 1 000 000, a medium one, 
#and 1 000 000+ - bad one

#lets separate data then

data_to_use$Al_rank <- NULL

data_a_rank <- data_to_use %>% 
  mutate(Al_rank = case_when(
  data_to_use$Alexa == 0 | is.na(data_to_use$Alexa) ~ "no_data",
  data_to_use$Alexa <= 250000 ~ 'a_top',
  data_to_use$Alexa >= 250001 & data_to_use$Alexa <= 1000000 ~ 'a_med',
  data_to_use$Alexa >= 1000001 ~ 'a_low'
))

#lets see how many observations there are in each group

agg_a_rank <- data_a_rank %>% group_by(Al_rank) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

agg_a_rank

data_a_rank <- data_a_rank %>% 
  filter(.,data_a_rank$Al_rank != "no_data")

#From what we can see, there are much more of data that does not have a rank
#it might be because of a low-quality website with a low traffic, so
#it didn't pass Alexa's standards, or simply because there is no data.

#lets build a clustering graph
ggplot(data_a_rank, aes(Traffic, Price, col=Al_rank)) + 
  geom_point(aes(shape=Al_rank), size=2) + 
  labs(title="Clustering by Al rank rough", 
       caption="Graph: Clustering graph") + 
  coord_cartesian(xlim = 1.2 * c(min(data_a_rank$Traffic), max(data_a_rank$Traffic)), 
                  ylim = 1.2 * c(min(data_a_rank$Price), max(data_a_rank$Price))) +
  geom_encircle(data = data_a_rank, aes(x=Traffic, y=Price)) +  
  geom_encircle(data = data_a_rank, aes(x=Traffic, y=Price)) + 
  geom_encircle(data = data_a_rank, aes(x=Traffic, y=Price))

#lets try to make it smoother by removing a 95%+ quantile.
quantile(data_a_rank$Price, na.rm=TRUE, 0.95)
quantile(data_a_rank$Traffic, na.rm=TRUE, 0.95)

data_a_rank_new <- data_a_rank %>% 
  filter(., Price < 50000 & Traffic < 9.3)

#lets plot a new one
ggplot(data_a_rank_new, aes(Traffic, Price, col=Al_rank)) + 
  geom_point(aes(shape=Al_rank), size=2) +
  labs(title="Clustering by Al rank cleaned", 
       caption="Graph: Clustering graph") + 
  coord_cartesian(xlim = 1.2 * c(min(data_a_rank_new$Traffic), max(data_a_rank_new$Traffic)), 
                  ylim = 1.2 * c(min(data_a_rank_new$Price), max(data_a_rank_new$Price))) +
  geom_encircle(data = data_a_rank_new, aes(x=Traffic, y=Price)) +
  geom_encircle(data = data_a_rank_new, aes(x=Traffic, y=Price)) + 
  geom_encircle(data = data_a_rank_new, aes(x=Traffic, y=Price))

nrow(data_a_rank) - nrow(data_a_rank_new) 
#We have removed only 35 observations. Ir is not too much.

#compare their distributions with violinplots.
ggplot(data_a_rank_new, aes(x=as.factor(Al_rank), y=Price, fill = Al_rank))+
  geom_violin() +
  labs(title="Distribution by Al rank", 
       caption="Graph: Violinplot")
#As the result, there is no clear difference in prices between Alexa's ratings,
#so we may disregard this rate in the price formation.



#check it

