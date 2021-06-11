#import libraries 
library(ggplot2)
library(tidyverse)
library(patchwork)
library(rmarkdown)
library(dplyr)


# Check the top five 
head(BuybuySalesData, n=5)

# Check the last 5 
tail(BuybuySalesData, n= 5)

# Dim names

dimnames(BuybuySalesData)

# Check Data Attributes
attributes(BuybuySalesData)

# Check for Missing values 
is.na(BuybuySalesData)

# Summary of the data values
summary(BuybuySalesData)

# Check the Dimension 
dim(BuybuySalesData)

# Check for data Structure of the dataset 
class(BuybuySalesData)

# Check for datatype of the object
typeof(BuybuySalesData)

# List out the variables 
names(BuybuySalesData)

# Profit 

BuybuySalesData$Profit =  BuybuySalesData$Revenue - BuybuySalesData$Cost
View(BuybuySalesData)

# Extract rows with NA in any column 
BuybuySalesData[rowSums(is.na(BuybuySalesData)) > 0, ] 

# To extract the name of column(s) with missing value 
names(which(colSums(is.na(BuybuySalesData)) > 0))

# Revenue made in each state
state <- aggregate(Revenue ~ State, BuybuySalesData, mean)
bp <- ggplot(state, aes(x=State, y=Revenue, fill=State))+
  geom_bar(width = 1, stat = "identity", color="white")+ 
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none")+
  ggtitle("Revenue made in each state")
bp

# Revenue made by product categories 
product_cat <- aggregate(Revenue ~ `Product.Category`, BuybuySalesData, sum)
product_cat

# Compute the position of labels
product_cat <- product_cat %>% 
  arrange(desc(`Product.Category`)) %>%
  mutate(prop = Revenue / sum(product_cat$Revenue) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

pp <- ggplot(product_cat, aes(x="", y=prop, fill=`Product.Category`)) +
  geom_bar(stat="identity", width=1, color="white")+
  coord_polar("y", start=0)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  geom_text(aes(y = ypos, label = `Product.Category`), color = "white", size=3) +
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="none") +
  ggtitle("Revenue by Product Categories")
pp




# Revenue made by country 
country_revenue <- aggregate(Revenue ~ Country, BuybuySalesData, sum)
country_revenue

#Sum of the quantities sold in each country 
qty <- aggregate(`Order.Quantity` ~ Country, BuybuySalesData, sum)
qty
qty_sold <- ggplot(qty, aes(x=Country, y=`Order.Quantity`, fill=Country))+
  geom_bar(stat = 'identity', width = 1, color="white")+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none") +
  ggtitle("Quantities sold in each country")

qty_sold

# Revenue made by product
aggregate(Revenue ~ Product, BuybuySalesData, sum)

#Revenue made by product/ Product categories 
aggregate(Revenue ~ Product + `Product.Category`, BuybuySalesData, sum)

#Revenue made in each year 
year <- aggregate(Revenue ~ Year, BuybuySalesData, sum )
bp_year <- ggplot(year, aes(x=Year, y=Revenue, fill=Year))+
  geom_bar(width = 1, stat = "identity", color="white")+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="none") +
  ggtitle("Revenue Made in each year")
bp_year

#Calculate average revenue made by product category 
aggregate(Revenue ~ `Product.Category`, BuybuySalesData, sum)


#Scatter plot 
cost <- as.factor(BuybuySalesData$Unit.Cost)
price <- as.factor(BuybuySalesData$Unit.Price)
sp = ggplot(BuybuySalesData, aes(x=price, y=cost))+
  geom_point(size=2, shape=20)+
  ggtitle("Scatter Plot")
sp


(qty_sold + bp_year + pp )/(bp) / sp
