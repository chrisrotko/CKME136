
# CKME 136 Market Basket Analysis: Chris Rotko
# Data Preparation

install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")
install.packages("ggplot2")
install.packages("arules")
install.packages("arulesViz")
install.packages("plyr")
install.packages("lubridate")


library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(arules)
library(arulesViz)
library(dplyr)
library(lubridate)

------------------------------------------------------------------------------------

retail <- read_excel('C:/Users/christopher.rotko/Downloads/online_retail.xlsx')
retail2 <- read_excel('C:/Users/christopher.rotko/Downloads/online_retail2.xlsx')

retail <- read_excel('C:/Users/Chris/Desktop/Semester 1 2018/CKME Data Analytics Capstone/online_retail.xlsx')

retail2 <- read_excel('C:/Users/Chris/Desktop/Semester 1 2018/CKME Data Analytics Capstone/online_retail2.xlsx')

fullretail <-bind_rows(retail, retail2)

str(fullretail)
head(fullretail, n=20)


fullretail <- fullretail[complete.cases(fullretail), ]
fullretail <- fullretail %>% mutate(Description = as.factor(Description))
fullretail <- fullretail %>% mutate(Country = as.factor(Country))
fullretail$Date <- as.Date(fullretail$InvoiceDate)
fullretail$Time <- format(fullretail$InvoiceDate,"%H:%M:%S")
fullretail$InvoiceNo <- as.numeric(as.character(fullretail$Invoice))

head(fullretail, n=10)

sum(is.na(fullretail))

fullretail <- na.omit(fullretail)
--------------------------------------------------------------------------------------------------------------------
# Data Exploration

" Number of Unique Items & Most Frequently Occuring Items"

nlevels(fullretail$Description)
fct_count(fullretail$Description, sort=TRUE)
itemcount <- count(fullretail, fullretail$Description)
head(itemcount, n=20)

"Distribution count of Purchase Times"

fullretail$Time <- as.factor(fullretail$Time)
x <- hms(as.character(fullretail$Time))
fullretail$Time = hour(x)

fullretail %>% 
  ggplot(aes(x=Time)) + geom_histogram(stat="count",fill="blue")


"Number of Items per Transaction"

detach("package:plyr", unload=TRUE)

fullretail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="blue", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))


---------------------------------------------------------------------
  
## Transactional sorting ##

library(plyr)

  
itemList <- ddply(fullretail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))
head(itemList, n=10)

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("Items")

-----------------------------------------------------------------------------------
# Importing new transactionally sorted dataset for Basket Analysis

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

transaction <- read.transactions('market_basket.csv', format = 'basket', sep=',')

summary(transaction)

# 33,108 transactions
# 44,774 items
# total items purchased = 33,108*44,774*0.0004737497 = 702,275

# Re-Creating Histogram Frequency Chart
itemFrequencyPlot(transaction, topN=10, type='absolute')


rules <- apriori(transaction, parameter = list(supp=0.01, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)

summary(rules)

inspect(rules)



