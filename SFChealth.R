is.na(BuybuySalesData)
summary(BuybuySalesData)
dim(BuybuySalesData)
class(BuybuySalesData)
typeof(BuybuySalesData)

library(ggplot2)
library(tidyverse)
library(patchwork)
#theme_set(theme_bw(base_size=16))

# Extract rows with NA in any column 
BuybuySalesData[rowSums(is.na(BuybuySalesData)) > 0, ] 

#To extract the name of column(s) with missing value 
names(which(colSums(is.na(BuybuySalesData)) > 0))
#Get the column names 
names(BuybuySalesData)

# Revenue made in each state
state <- aggregate(Revenue ~ State, BuybuySalesData, sum)
bp <- ggplot(state, aes(x=State, y=Revenue, fill=Revenue))+
  geom_bar(width = 1, stat = "identity")
bp

# Revenue made by product categories 
product_cat <- aggregate(Revenue ~ `Product.Category`, BuybuySalesData, sum)
product_cat
pp <- ggplot(product_cat, aes(x="", y=Revenue, fill=`Product.Category`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# Revenue made by country 
country_revenue <- aggregate(Revenue ~ Country, BuybuySalesData, sum)
country_revenue

#Sum of the quantities sold in each country 
qty <- aggregate(`Order.Quantity` ~ Country, BuybuySalesData, sum)
qty
qty_sold <- ggplot(qty, aes(x=Country, y=`Order.Quantity`, fill=Country))+
  geom_bar(stat = 'identity', width = 1)
qty_sold

#
# Revenue made by product
aggregate(Revenue ~ Product, BuybuySalesData, sum)
#Revenue made by product/ Product categories 
aggregate(Revenue ~ Product + `Product.Category`, BuybuySalesData, sum)

#Revenue made in each year 
year <- aggregate(Revenue ~ Year, BuybuySalesData, sum )
bp_year <- ggplot(year, aes(x=Year, y=Revenue, fill=Revenue))+
  geom_bar(width = 1, stat = "identity")
bp_year
#Calculate average revenue made by product category 
aggregate(Revenue ~ `Product.Category`, BuybuySalesData, sum)


#Scatter plot 

ggplot(BuybuySalesData, aes(x=Revenue, y=Cost))+
  geom_point(size=2, shape=20)+geom_smooth(method=lm)

# Profit 

BuybuySalesData$Profit =  BuybuySalesData$Revenue - BuybuySalesData$Cost
View(BuybuySalesData)

