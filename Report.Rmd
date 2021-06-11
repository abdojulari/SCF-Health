---
title: "Buy Buy Data Analysis"
author: "Abdulkabir Ojulari"
date: '2021-06-11'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


```{r }
summary(BuybuySalesData)
```

## Total Revenue made in each state

```{r }
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
```

## Total Revenue per product category
```{r }
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


```

## Total sum of quantity sold in each country 

```{r }
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

```

## Total Revenue per year 

```{r }
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

```

## Scatter plot 

```{r }

#Scatter plot 
cost <- as.factor(BuybuySalesData$Unit.Cost)
price <- as.factor(BuybuySalesData$Unit.Price)
sp = ggplot(BuybuySalesData, aes(x=price, y=cost))+
  geom_point(size=2, shape=20)+
  ggtitle("Scatter Plot")
sp

```
## Dashboard 

```{r }
(qty_sold + bp_year + pp )/(bp) / sp
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
