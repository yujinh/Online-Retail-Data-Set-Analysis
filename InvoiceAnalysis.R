#install package for SQL queries
install.packages("sqldf")
install.packages("dplyr")
library(sqldf)
library(plyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemes)

#Data Preparation------------------------------------------

original <- data_science_analytics_2018_data
n_row(Original)

#Get rid of entries with blank column.
no_blank <- original[complete.cases(original),]
nrow(no_blank)

#Get rid of duplicates.
a<-nrow(no_blank)
no_blank<- distinct(no_blank)
b<-nrow(no_blank)
num_duplicate = a-b
num_duplicate
nrow(no_blank)

#Basic Data Summary------------------------------------------

summary(no_blank)

#How many unique customers, products, transactions, countries.
length(unique(no_blank$CustomerID))
length(unique(no_blank$StockCode))
length(unique(no_blank$InvoiceNo))
length(unique(no_blank$Country))

#Stock Code 5 Digit Only?--------------------------------------
code_not_five<- sqldf("SELECT * FROM original
                    WHERE StockCode NOT BETWEEN '00000' and '99999'")

strange_code <- sqldf("SELECT StockCode, Description
                      FROM code_not_five")

unique(strange_code)

install.packages("gridExtra")
library(gridExtra)

grid.new()
Strange_Code
typeof(Strange_Code)
plot.table(unique(Strange_Code))
tableGrob(Strange_Code)

#Canceled Orders---------------------------------------------
cancel <- sqldf("SELECT * FROM no_blank 
                WHERE InvoiceNo LIKE 'C%'")
nrow(cancel)
length(unique(cancel$InvoiceNo))

#% of transactions are Cancel transactions
percent(length(unique(cancel$InvoiceNo))/
         length(unique(no_blank$InvoiceNo)))
#% of entries are Cancel entries
percent(nrow(cancel)/nrow(no_blank))

#See if any Cancel Transaction has positive Quantity
pos_ctrans <- sqldf("SELECT * FROM cancel
                    WHERE Quantity>0")
pos_ctrans

#Non Cancel Transactions-------------------------------------
no_cancel <- no_blank
no_cancel<-no_cancel[grep('^[0-9]', no_cancel$InvoiceNo),]
nrow(no_cancel)

percent(nrow(cancel)/nrow(no_cancel)) #percent of canceled transactions)

#Invoices that have matching canceled transactions
canceled_matched<-sqldf("SELECT no_cancel.InvoiceNo, no_cancel.CustomerID, no_cancel.StockCode, no_cancel.Description, 
                        no_cancel.Quantity, no_cancel.UnitPrice, no_cancel.Country, no_cancel.InvoiceDate FROM no_cancel 
                        LEFT JOIN cancel ON (cancel.CustomerID = no_cancel.CustomerID)
                        WHERE cancel.StockCode = no_cancel.StockCode
                        AND cancel.Description = no_cancel.Description
                        AND ABS(cancel.Quantity)=no_cancel.Quantity
                        AND cancel.UnitPrice = no_cancel.UnitPrice
                        AND cancel.Country = no_cancel.Country
                        AND cancel.InvoiceDate > no_cancel.InvoiceDate")

length(unique(canceled_matched$InvoiceNo))

pos_filtered<-anti_join(no_cancel, canceled_matched)

#Make sure all matched with cancel are deleted no_cancel
canceled_matched = distinct(canceled_matched)
pos_flitered = distinct(pos_filtered)
no_cancel = distinct(no_cancel)
length(unique(pos_filtered$InvoiceNo))

length(unique(no_cancel$InvoiceNo))-
  length(unique(canceled_matched$InvoiceNo))
nrow(cancel)-nrow(canceled_matched)


test<-sqldf("SELECT pos_filtered.InvoiceNo, pos_filtered.CustomerID, pos_filtered.StockCode, pos_filtered.Description, 
            pos_filtered.Quantity,pos_filtered.UnitPrice, pos_filtered.Country, pos_filtered.InvoiceDate FROM pos_filtered
            LEFT JOIN canceled_matched ON (canceled_matched.CustomerID = pos_filtered.CustomerID)
            WHERE canceled_matched.StockCode = pos_filtered.StockCode
            AND canceled_matched.Description = pos_filtered.Description
            AND ABS(canceled_matched.Quantity)=pos_filtered.Quantity
            AND canceled_matched.UnitPrice = pos_filtered.UnitPrice
            AND canceled_matched.Country = pos_filtered.Country
            AND canceled_matched.InvoiceDate > pos_filtered.InvoiceDate")

test

#Revenue Barplots--------------------------------------------

pos_filtered$Revenue <- pos_filtered$UnitPrice * pos_filtered$Quantity
typeof(pos_filtered$InvoiceDate)

#barplot of monthly revenue - November has highest revenue
#See if I can make the graph split into first cust and reorder.
pos_filtered$InvoiceYearMonth <- format(pos_filtered$InvoiceDate, "%Y%m")
year_month_rev<-tapply(X = pos_filtered$Revenue, INDEX = pos_filtered$InvoiceYearMonth, FUN = sum)
barplot(year_month_rev)

#barplot of weekly revenue
# graph by week:
pos_filtered$InvoiceYearWeek <- format(pos_filtered$InvoiceDate, "%Y-%U")
year_week_rev<-tapply(X = pos_filtered$Revenue, INDEX = pos_filtered$InvoiceYearWeek, FUN = sum)
barplot(year_week_rev)

#barplot of daily revenue 
pos_filtered$InvoiceDay <- format(pos_filtered$InvoiceDate, "%Y%m%d")
day_rev<-tapply(X = pos_filtered$Revenue, INDEX = pos_filtered$InvoiceDay, FUN = sum)
barplot(day_rev)

#Frequency Barplot--------------------------------------------

#barplot of monthly frequency

pos_filtered_trans <- pos_filtered[!duplicated(pos_filtered$InvoiceNo),]
pos_filtered_trans<- mutate(pos_filtered_trans, count=1)
pos_filtered_trans
nrow(pos_filtered_trans)

pos_filtered_trans$InvoiceYearMonth <- format(pos_filtered_trans$InvoiceDate, "%Y%m")
monthly_freq<-tapply(X = pos_filtered_trans$count, INDEX = pos_filtered_trans$InvoiceYearMonth, FUN = sum)
barplot(monthly_freq)

sum(monthly_freq)

#barplot of weekly frequency

pos_filtered_trans$InvoiceYearWeek <- format(pos_filtered_trans$InvoiceDate, "%Y-%U")
weekly_freq<-tapply(X = pos_filtered_trans$count, INDEX = pos_filtered_trans$InvoiceYearWeek, FUN = sum)
barplot(weekly_freq)

sum(weekly_freq)

#barplot of weekday frequency

pos_filtered_trans$InvoiceWeekday <- weekdays(pos_filtered_trans$InvoiceDate)
weekday_freq<-tapply(X = pos_filtered_trans$count, INDEX = pos_filtered_trans$InvoiceWeekday, FUN = sum)
ordered_weekday_freq <- weekday_freq[c("Monday", "Tuesday", 
                                        "Wednesday", "Thursday",
                                        "Friday", "Sunday")]

barplot(ordered_weekday_freq, main = "Frequency of Orders by Weekday")

sum(weekly_freq)

weekly_freq<-as.data.frame(weekly_freq)
colnames(weekly_freq) = c('freq')

#barplot of hourly frequency
install.packages("lubridate")
library(lubridate)

pos_filtered_trans$InvoiceHour <- hour(pos_filtered_trans$InvoiceDate)
hourly_freq<-tapply(X = pos_filtered_trans$count, INDEX = pos_filtered_trans$InvoiceHour, FUN = sum)
barplot(hourly_freq)

sum(hourly_freq)

hourly_freq<-as.data.frame(hourly_freq)
hourly_freq<-mutate(hourly_freq, frequency = hourly_freq['freq'])
colnames(hourly_freq) = c('freq')


#Revenue, Frequency, Cancel by Country----------------------------
#RevbyCountry<-tapply(X = No_Cancel$Revenue, INDEX = No_Cancel$Country, FUN = sum)

#percentlabels<- round(100*RevbyCountry/sum(RevbyCountry), 1)
#pielabels<- paste(percentlabels, "%", sep="")
#pie(RevbyCountry, main = "Revenue by Country")
cancel

cancel_count <- count(cancel, Country, sort=TRUE)
cancel_count <- mutate(cancel_count,
                      percent = n / sum(n) * 100)
cancel_count <- arrange(cancel_count, desc(percent))
cancel_graph <- ggplot(cancel_count, aes(x = reorder(Country, n), y = n, fill = Country)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(y = n, label = comma(n), hjust = 0.04)) +
  theme_bw() +
  scale_color_tableau("tableau20") +
  theme(legend.position="none") +
  labs(title="Number of Cancels by Country", x = "Country", y = "Cancel Count")
cancel_graph

order_count <- count(pos_filtered, Country, sort=TRUE)
order_count <- mutate(order_count,
             percent = n / sum(n) * 100)
order_count <- arrange(order_count, desc(percent))
count_graph <- ggplot(order_count, aes(x = reorder(Country, n), y = n, fill = Country)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(y = n, label = comma(n), hjust = 0.04)) +
  theme_bw() +
  scale_color_tableau("tableau20") +
  theme(legend.position="none") +
  labs(title="Number of Orders by Country", x = "Country", y = "Order Count")
count_graph

country_rev <- count(pos_filtered, Country, wt=Revenue, sort=TRUE)
country_rev <- mutate(country_rev,
                      percent = n / sum(n) * 100)
country_rev <- arrange(country_rev, desc(percent))
rev_graph <- ggplot(country_rev, aes(x = reorder(Country, n), y = n, fill = Country)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(y = n, label=comma(n), hjust =-0.1)) +
  theme_bw() +
  scale_color_tableau("tableau20") +
  theme(legend.position="none") +
  labs(title="Revenue by Country", x = "Country", y = "Revenue in British Pound(GBP)")
rev_graph

#--------------------------------------------------------------------
#How much do people typically spend per order?
order_spent<-tapply(X = pos_filtered$Revenue, INDEX = pos_filtered$InvoiceNo, FUN = sum)
order_spent
bin_break <- c(0,50,100,200,300,400,500,1000,5000,10000,50000,100000)
bin_label <- c("<£50", "£50-£100", "£100-£200", "£200-£300",
            "£300-£400", "£400-£500", '£500-£1,000', "£1,000-£5,000",
            "£5,000-£10,000", "£10,000-£50,000", ">£50,000")
length(bin_break)
length(bin_label)
bins<-cut(order_spent, breaks=bin_break, include.lowest = T,
          right = FALSE, labels=bin_label)
summary(bins)
plot(bins, main="Frequency of Spent Per Order",
     ylab = "Count",
     col="bisque")

data.frame(order_spent)
y <- data.frame(order_spent, bins)
ggplot(data=y, aes(x= factor(y$bins),fill=..count..)) +
  geom_bar(color='black', alpha=0.9) +
  stat_count(geom="text", aes(label=..count..), vjust = -0.5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(title="Frequency of Spent Per Order", y="Count",x="Spent Per Order") +
  ylim(0,4500) +
  scale_x_discrete(drop=FALSE)# include the bins of length zero

#---------------------------------------------------------------
#Customer Segmentation based on Recency and Frequency
#LifeCycle Grids

#Frequencey by each client
library(plyr)
frequency<-ddply(pos_filtered, ~CustomerID, summarise, 
      Cust_Frequency = length(unique(InvoiceNo)))
frequency

freq_bin_break <- c(0,3,10,20,50,100,500,1000,50000)
freq_bin_label <- c("≤3", "4-10", "11-20", "21-50",
               "51-100", "101-500", "501-1000",
               ">1000")
freq_bins<-cut(frequency$Cust_Frequency, breaks=freq_bin_break, include.lowest = T,
          right = FALSE, labels=freq_bin_label)
summary(freq_bins)

y <- data.frame(frequency$Cust_Frequency, freq_bins)
ggplot(data=y, aes(x= factor(y$freq_bins),fill=..count..)) +
  geom_bar(color='black', alpha=0.9) +
  stat_count(geom="text", aes(label=..count..), vjust = -0.5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(title="Frequency of Order by Customer", y="Count",x="Frequency") +
  ylim(0,3000) +
  scale_x_discrete(drop=FALSE)# include the bins of length zero

#Recency of purchase by client
#exactly one day after last transaction
last_transaction<-max(pos_filtered$InvoiceDate)
end_date = last_transaction + 3600 * 24 

recency<-ddply(pos_filtered, ~CustomerID, summarise, 
                 Cust_Recency = as.numeric(end_date-max(InvoiceDate)))
recency$Cust_Recency[1]
as.numeric(recency$Cust_Recency[1])

rec_bin_break <- c(0,7,14,30,60,90,150,365,5000)
rec_bin_label <- c("≤7", "8-14", "15-30",
                    "31-60", '61-90', "91-150",
                    "151-365", ">365")
rec_bins<-cut(as.numeric(recency$Cust_Recency), breaks=rec_bin_break, include.lowest = T,
               right = FALSE, labels=rec_bin_label)
summary(rec_bins)

y <- data.frame(recency$Cust_Recency, rec_bins)
ggplot(data=y, aes(x= factor(y$rec_bins),fill=..count..)) +
  geom_bar(color='black', alpha=0.9) +
  stat_count(geom="text", aes(label=..count..), vjust = -0.5) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(title="Recency by Customer", y="Count",x="Recency in days") +
  ylim(0,1000) +
  scale_x_discrete(drop=FALSE)# include the bins of length zero


#Create Table with CustomerID, Cust_Frequency, and Cust_Recency
rf <-sqldf("SELECT frequency.CustomerID, Cust_Frequency,
           Cust_Recency
           FROM frequency JOIN recency
           ON frequency.CustomerID = recency.CustomerID")
rf

#Group Customers depending on frequency and recency
grid_info <- RF %>%
  mutate(Grid_Freq = ifelse(between(Cust_Frequency, 0, 3), '≤3', 
                     ifelse(between(Cust_Frequency, 4, 10), '4-10',
                     ifelse(between(Cust_Frequency, 11, 20), '11-20',
                     ifelse(between(Cust_Frequency, 21, 50), '21-50',
                     ifelse(between(Cust_Frequency, 51, 100), '51-100',
                     ifelse(between(Cust_Frequency, 101, 500), '101-500',
                     ifelse(between(Cust_Frequency, 501, 1000), '501-1000', '>1000'
                     )))))))) %>%
  mutate(Grid_Rec = ifelse(between(Cust_Recency, 0, 7), '≤7 days', 
                    ifelse(between(Cust_Recency, 8, 14), '8-14 days',
                    ifelse(between(Cust_Recency, 15, 30), '15-30 days',
                    ifelse(between(Cust_Recency, 31, 60), '31-60 days',
                    ifelse(between(Cust_Recency, 61, 90), '61-90 days',
                    ifelse(between(Cust_Recency, 91, 150), '91-150 days',
                    ifelse(between(Cust_Recency, 151, 365), '151-365 days', '>365 days'
                    ))))))))

grid_info$Grid_Freq <- factor(grid_info$Grid_Freq , 
                      levels=c("≤3", "4-10", "11-20", "21-50",
                              "51-100", "101-500", "501-1000",
                              ">1000"))
grid_info$Grid_Rec <- factor(grid_info$Grid_Rec, 
                             levels=c("≤7 days", "8-14 days", "15-30 days",
                                      "31-60 days", '61-90 days', "91-150 days",
                                      "151-365 days", ">365 days"))

grid_summarise <- grid_info %>%
  group_by(Grid_Freq, Grid_Rec) %>%
  dplyr::summarise (quantity=n()) %>%
  mutate(Customer='Customer') %>%
  ungroup()

library(reshape2)
grid_table <- dcast(Grid_Summarise , 
              Grid_Freq ~ Grid_Rec, 
              value.var='quantity',
              fun.aggregate=sum)

grid_table

#Plot LifeCycle Grids
ggplot(grid_summarise, aes(x=Customer, y=quantity, fill=quantity)) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=4) +
  facet_grid(Grid_Freq ~ Grid_Rec) +
  ggtitle("LifeCycle Grids")

#---------------------------------------------------------------
#Interesting to see which items are most frequently bought
#which items bring in most revenue
#Identify high-and low-value customers for marketing purposes.













