#Import Data set:
Sales_data<-read.csv("SalesData.csv")
View(Sales_data)
getwd()
#Structure of the data set
str(Sales_data)

library(dplyr)
library(ggplot2)

# Q(1): Compare Sales by region for 2016 with 2015 using bar chart
regional_totalsales<-Sales_data%>%select(Region, Sales2015, Sales2016)%>%group_by(Region)%>%summarise(Total_sales2015=sum(Sales2015), Total_sales2016=sum(Sales2016))

#Create data Frame
df<-data.frame(Region=c("Central","East","West","Central","East","West"),
               Sales=c(7891729,9512916,5349745,9787809,12667230,7209689), 
               Year=c("Total_sales2015","Total_sales2015","Total_sales2015","Total_sales2016","Total_sales2016","Total_sales2016"))
df
ggplot(df,aes(x=Region,y=Sales, fill=Year))+geom_bar(position="dodge", stat="identity")+geom_text(aes(label=Sales),size=3.2, hjust=0.7)+theme_update(plot.title = element_text(hjust = ))+ggtitle("Compare Sales by Region for 2015 with 2016")

# Q(2): Pie Chart for Sales of each Region in 2016
totalsales2016<-Sales_data%>%select(Region, Sales2016)%>%group_by(Region)%>%summarise(Total_sales2016=sum(Sales2016))
totalsales2016

pie(totalsales2016$Total_sales2016,totalsales2016$Region, main="Region Pie Chart")

pct<-round(totalsales2016$Total_sales2016/sum(totalsales2016$Total_sales2016)*100,1)
pct                                        

lbs=paste(pct,"%",":",c("Central","East","West"))
lbs

pie(totalsales2016$Total_sales2016, labels =lbs, main = "Region Wise Sales2016",
    col = rainbow(length(totalsales2016$Total_sales2016)))

#3D Pie Chart
#install.packages("plotrix")
library(plotrix)

pie3D(totalsales2016$Total_sales2016, labels = lbs, main = "Region Wise Sales2016 3D Pie Chart",
      col = rainbow(length(totalsales2016$Total_sales2016)),explode = 0.05)

# Q(3): Compare sales of 2015 and 2016 with region and Tier        
totalsales_2015<-Sales_data%>%select(Region, Tier,Sales2015)%>%group_by(Region, Tier)%>%summarise(Total_sales2015=sum(Sales2015))
totalsales_2015

totalsales_2016<-Sales_data%>%select(Region, Tier,Sales2016)%>%group_by(Region, Tier)%>%summarise(Total_sales2016=sum(Sales2016))
totalsales_2016

library(dplyr)

total_sales<-full_join(totalsales_2015,totalsales_2016)
total_sales

df<-data.frame(Region=c("Central","Central","Central","Central","Central","Central","Central","Central","East","East","East","East","East","East","East","East","West","West","West","West","West","West","West","West"), Tier=c("High","High","Low","Low","Med","Med","out","out","High","High","Low","Low","Med","Med","out","out","High","High","Low","Low","Med","Med","out","out"),
               Year=c("Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016"),
               Sales=c(4798698, 6026043,943440,1132833,2068226,2632181,81365,-3249,6102946,7817151,901666,1144930,2470998,3705150,37307,0,2944789,3768038,671064,1099502,1718476,2342149,15415,0))
df
ggplot(df,aes(x=Tier,y=Sales, fill=Year))+geom_bar(position="dodge", stat="identity")+geom_text(aes(label=Sales),size=3.2, hjust=0.7)+theme_update(plot.title = element_text(hjust = ))+ggtitle("Compare sales of 2015 and 2016 with Region and Tier")+facet_grid(~Region)

# Q(4):In East region, Which state registered a decline in 2016 as compared to 2015 
totalsales_2015 = Sales_data%>%select(Region,State,Sales2015)%>%filter(Region=="East")%>%group_by(State)%>%summarise(totalsales_2015=sum(Sales2015))
totalsales_2016 = Sales_data%>%select(Region,State,Sales2016)%>%filter(Region=="East")%>%group_by(State)%>%summarise(totalsales_2016=sum(Sales2016))

total_sales<-full_join(totalsales_2015,totalsales_2016)
total_sales

df<-data.frame(State=c("CT","CT","DC","DC","FL","FL","GA","GA","MA","MA","MD","MD","ME","ME","NC","NC","NH","NH","NJ","NJ","NY","NY","PA","PA","RI","RI","SC","SC","TN","TN","VA","VA"), 
               Year=c("Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016","Total_sales2015","Total_sales2016"),
               Sales=c(197203,323502,216724,257422,1660162,2508232,681546,946920,419279,687096,527309,750009,77426,183673,1292802,1610521,136419,144718,520419,782289,1725415,1703136,785093,894319,156312,162455,247812,463414,49677,70109,819319,1179415))
df
ggplot(df,aes(x=State,y=Sales, fill=Year))+geom_bar(position="dodge", stat="identity")+geom_text(aes(label=Sales),size=3.2, hjust=0.7)+theme_update(plot.title = element_text(hjust = ))+ggtitle("In East Region, NY registered a decline in sales in 2016")

# Q(5):In all the high tier, which Division saw a decline in number of units sold in 2016 compared to 2015?
total_units=Sales_data %>%select(Division, Tier, Units2015,Units2016)%>%filter(Tier=="High")%>%group_by(Division)%>%summarise(TotalUnits_2015=sum(Units2015),TotalUnits_2016=sum(Units2016))
View(total_units)

#Save "total_units into csv
write.csv(total_units,"total_units.csv")

#Import modified "total_units.csv" file
total_units=read.csv("total_units.csv")
total_units

ggplot(total_units,aes(x=Division,y=Units, fill=Year))+geom_bar(position="dodge", stat="identity")+geom_text(aes(label=Units),size=3.2, hjust=0.7)+theme_update(plot.title = element_text(hjust = ))+ggtitle("Division wise Units Comparison for Each Year")+coord_flip()

##From the above graph, we can easily see that there is no decline number of units sold in 2016 as compared to 2015.

# Q(6): Create a new column Qtr-
#Jan-Mar: Q1
#Apr-Jan: Q2
#Jul-Sep: Q3
#Oct-Dec: Q4

#Calculating Quartiles
Q1<-Sales_data%>%select(Month, Sales2015,Sales2016)%>%filter(Month==c("Jan","Feb","Mar"))%>%summarise(sum(Sales2015),sum(Sales2016))
Q1

Q2<-Sales_data%>%select(Month, Sales2015,Sales2016)%>%filter(Month==c("Apr","May","Jun"))%>%summarise(sum(Sales2015),sum(Sales2016))
Q2

Q3<-Sales_data%>%select(Month, Sales2015,Sales2016)%>%filter(Month==c("Jul","Aug","Sep"))%>%summarise(sum(Sales2015),sum(Sales2016))
Q3

Q4<-Sales_data%>%select(Month, Sales2015,Sales2016)%>%filter(Month==c("Oct","Nov","Dec"))%>%summarise(sum(Sales2015),sum(Sales2016))
Q4

#Preparing data frame 
df<-data.frame(Year=c("TotalSales2015","TotalSales2016","TotalSales2015","TotalSales2016","TotalSales2015","TotalSales2016","TotalSales2015","TotalSales2016"),
               
               Qty=c("Q1","Q1","Q2","Q2","Q3","Q3","Q4","Q4"),Sales=c(2064545,2427928,1690440,2493748,2192199,2832778,1889045,2663758))
View(df)

# Q(7):Compare Qtr wise sales in 2015 and 2016 in a bar plot
#Dodge Graph
ggplot(df, aes(x=Qty,y=Sales, fill=Year))+geom_bar(position="dodge", stat="identity")+geom_text(aes(label=Sales),size=3.2, hjust=0.7)+theme_update(plot.title = element_text(hjust = ))+ggtitle("Division wise Units Comparison for Each Year")

# Q(8): Determine the composition of Qtr wise sales in and 2015 with regards to all the Tiers in Pie Chart.
#Calculating Quartiles
par(mfrow=c(2,2))
Q1<-Sales_data%>%select(Month,Tier, Sales2015)%>%filter(Month==c("Jan","Feb","Mar"))%>%group_by(Tier)%>%summarise(Total_Sales=sum(Sales2015))
Q1

pie(Q1$Total_Sales,Q1$Tier, main="Qtr1")

Q2<-Sales_data%>%select(Month,Tier, Sales2015)%>%filter(Month==c("Apr","May","Jun"))%>%group_by(Tier)%>%summarise(Total_Sales=sum(Sales2015))
Q2

pie(Q2$Total_Sales,Q2$Tier, main="Qtr2")

Q3<-Sales_data%>%select(Month, Tier, Sales2015)%>%filter(Month==c("Jul","Aug","Sep"))%>%group_by(Tier)%>%summarise(Total_Sales=sum(Sales2015))
Q3

pie(Q3$Total_Sales,Q3$Tier, main="Qtr3")

Q4<-Sales_data%>%select(Month,Tier, Sales2015)%>%filter(Month==c("Oct","Nov","Dec"))%>%group_by(Tier)%>%summarise(Total_Sales=sum(Sales2015))
Q4

pie(Q4$Total_Sales,Q4$Tier, main="Qtr4")

