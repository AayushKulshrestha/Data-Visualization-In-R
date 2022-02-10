Sales_data <- read.csv("SalesData.csv")
str(Sales_data)


head(Sales_data)
tail(Sales_data)
summary(Sales_data)
sum(is.na(Sales_data))
class(Sales_data)
typeof(Sales_data)

#1. Compare Sales by region for 2016 with 2015 using bar chart

library(dplyr)
sales_by_region <- Sales_data%>%group_by(Region)%>%summarise(Sales2015=sum(Sales2015),Sales2016=sum(Sales2016))
table(sales_by_region)
is.data.frame(sales_by_region)
data.frame(sales_by_region)

as.data.frame(sales_by_region)
sales_by_region

#making key value pairs to make both years as one
#https://www.statology.org/gather-function-in-r/

library(tidyr)
sales_by_Region1 <-gather(sales_by_region,key = Year, value = Sales,-Region)

library(ggplot2)

ggplot(data = sales_by_Region1,aes(x = Region,y = Sales,fill=Year,label=Sales))+geom_bar(stat ="identity",position = "dodge")+
xlab('Region') +ylab('Sales') + ggtitle("Comparision of Sales by Region")

#2. Pie charts for sales for each region in 2016

library(dplyr)

pie_sales<- Sales_data%>% group_by(Region)%>% summarise(TotalSales2016 = sum(Sales2016))

pie_sales
#pie(pie_sales, labels, main = "City pie chart", col = rainbow(length(x)))

label1<-pie_sales$Region
label1
pie_percentage = (pie_sales$TotalSales2016/sum(pie_sales$TotalSales2016)*100)
pie_percentage
pie_percentage=round(pie_percentage,digits = 1)

label1 = paste(label1,":",pie_percentage)
label1
label1 = paste(label1,"%",sep = "")

label2= paste(label1,":",pie_percentage)
label2

pie(pie_sales$TotalSales2016,labels = label1 , col = c("red","green","blue"),
main = "Pie Chart of Sales 2016",radius = 1,border = "black")


pie(pie_sales$TotalSales2016, col = c("white","slategray1","pink1"),
    main = "Pie Chart of Sales 2016",radius = 1,border = "black")

library(plotrix)

pie3D(pie_sales$TotalSales2016,labels = label1,explode = 0.1, col = c("red","green","blue"))+ title("Pie Chart of Sales 2016")

#3. Compare sales of 2015 and 2016 with Region and Tiers

library(dplyr)
?summarise
sales_region_tier <- Sales_data%>% group_by(Region,Tier)%>%summarise(Sales2015=sum(Sales2015),Sales2016=sum(Sales2016),.groups = 'keep')

sales_region_tier

library(tidyr)
sales_region_tier1 <-gather(sales_region_tier,key = Year, value = Sales,-c(Region,Tier))
#grouping
sales_region_tier1


library(ggplot2)

ggplot(data = sales_region_tier1,aes(x=Tier,y = Sales,fill=Year))+geom_bar(stat ="identity",position = "dodge")+
  facet_wrap(~ Region) + ggtitle("Compare sales of 2015 and 2016 with Region and Tiers")

#4. In East region, which state registered a decline in 2016 as compared to 2015?

library(dplyr)
sales_east_region <- Sales_data%>%group_by(State)%>%filter(Region=='East')%>%summarise(Sales2015=sum(Sales2015),Sales2016=sum(Sales2016))

sales_east_region

library(tidyr)
sales_east_region1 <- gather(sales_east_region,key=Year,value=Sales,-State)
sales_east_region1

library(ggplot2)


ggplot(sales_east_region1,aes(x=State,y=Sales,fill=Year))+geom_bar(stat ="identity",position = "dodge")+
  ggtitle("In East region, which state registered a decline in 2016 as compared to 2015")

#5. In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015?


library(dplyr)
tier_sales <- Sales_data%>%filter(Tier=="High")%>%
  group_by(Division)%>%summarise(Sales2015=sum(Units2015),Sales2016=sum(Units2016))

library(tidyr)
tier_sales1 <- gather(tier_sales,key=Year,value = Sales,-Division)

tier_sales1

library(ggplot2)

ggplot(tier_sales1,aes(x=Division,y=Sales,fill=Year))+geom_bar(stat ="identity",position = "dodge")+
  ggtitle("In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015")

#6. Create a new column Qtr -
# Jan - Mar : Q1 
# Apr - Jun : Q2 
# Jul - Sep : Q3 
# Oct - Dec : Q4

Sales_data$Qtr <- if_else(Sales_data$Month=="Jan"|Sales_data$Month=="Feb"|Sales_data$Month=="Mar","Q1",
                          if_else(Sales_data$Month=="Apr"|Sales_data$Month=="May"|Sales_data$Month=="Jun","Q2",
                          if_else(Sales_data$Month=="Jul"|Sales_data$Month=="Aug"|Sales_data$Month=="Sep","Q3","Q4")))


#7. Compare Qtr wise sales in 2015 and 2016 in a bar plot

library(dplyr)
sales_qtr <- Sales_data%>%group_by(Qtr)%>%summarise(Sales2015=sum(Sales2015),Sales2016=sum(Sales2016))

library(tidyr)
sales_qtr1 <- gather(sales_qtr,key = Year,value = Sales,-Qtr)

sales_qtr1

library(ggplot2)

ggplot(sales_qtr1,aes(x=Qtr,y=Sales,fill=Year))+geom_bar(stat ="identity",position = "dodge")+
  ggtitle("Comparision of Qtr wise sales in 2015 and 2016 in a bar plot")

#8. Determine the composition of Qtr wise sales in and 2015 with regards to all the Tiers in a pie chart.  
#(Draw 4 pie charts representing a Quarter for each Tier)

sales_first_quater<- Sales_data%>%group_by(Qtr,Tier)%>%summarise(Sales2015=sum(Sales2015))%>%filter(Qtr=='Q1')
sales_first_quater

sales_second_quater<- Sales_data%>%group_by(Qtr,Tier)%>%summarise(Sales2015=sum(Sales2015))%>%filter(Qtr=='Q2')
sales_second_quater

sales_third_quater<- Sales_data%>%group_by(Qtr,Tier)%>%summarise(Sales2015=sum(Sales2015))%>%filter(Qtr=='Q3')
sales_third_quater

sales_fourth_quater<- Sales_data%>%group_by(Qtr,Tier)%>%summarise(Sales2015=sum(Sales2015))%>%filter(Qtr=='Q4')
sales_fourth_quater

pie_percentage1 = (sales_first_quater$Sales2015/sum(sales_first_quater$Sales2015)*100)
pie_percentage1
pie_percentage1=round(pie_percentage1,digits = 1)


pie_percentage2 = (sales_second_quater$Sales2015/sum(sales_second_quater$Sales2015)*100)
pie_percentage2
pie_percentage2=round(pie_percentage2,digits = 1)


pie_percentage3 = (sales_third_quater$Sales2015/sum(sales_third_quater$Sales2015)*100)
pie_percentage3
pie_percentage3=round(pie_percentage3,digits = 1)



pie_percentage4 = (sales_fourth_quater$Sales2015/sum(sales_fourth_quater$Sales2015)*100)
pie_percentage4
pie_percentage4=round(pie_percentage4,digits = 1)


Name1<-sales_first_quater$Tier

Name1 = paste(Name1,":",pie_percentage1)
Name1
Name1 = paste(Name1,"%",sep = "")
Name1

Name2<-sales_second_quater$Tier

Name2 = paste(Name2,":",pie_percentage2)
Name2
Name2 = paste(Name2,"%",sep = "")
Name2

Name3<-sales_third_quater$Tier

Name3 = paste(Name3,":",pie_percentage3)
Name3
Name3 = paste(Name3,"%",sep = "")
Name3

Name4<-sales_fourth_quater$Tier

Name4 = paste(Name4,":",pie_percentage4)
Name4
Name4 = paste(Name4,"%",sep = "")
Name4


#pie chart quater 1

pie(sales_first_quater$Sales2015,labels =Name1 ,col = c("white","slategray1","pink1"),radius = 1,main = "Qtr 1")

#pie chart quater 2


pie(sales_second_quater$Sales2015,labels = Name2,col = c("white","slategray1","pink1"),radius = 1,main = "Qtr 2")

#pie chart quater 3

pie(sales_third_quater$Sales2015,labels = Name3,col = c("white","slategray1","pink1"),radius = 1,main = "Qtr 3")

#pie chart quater 4

pie(sales_fourth_quater$Sales2015,labels = Name4 ,col = c("white","slategray1","pink1"),radius = 1,main = "Qtr 4")
















