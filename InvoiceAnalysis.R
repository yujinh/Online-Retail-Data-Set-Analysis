#Style
#http://adv-r.had.co.nz/Style.html
#install package for SQL queries
install.packages("sqldf")
options(gsubfn.engine = "R")
library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(plyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemes)

#Data Preparation------------------------------------------

original <- data_science_analytics_2018_data
nrow(original)

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
options(scipen=5)
pos_filtered$Revenue <- pos_filtered$UnitPrice * pos_filtered$Quantity
typeof(pos_filtered$InvoiceDate)

pos_filtered$count <- 1


install.packages("data.table")
library(data.table)
fwrite(pos_filtered, "output.csv")

#barplot of monthly revenue - November has highest revenue
#forecast: http://blog.trap.it/blog/stats-for-marketers-a-simple-way-to-calculate-revenue-growth-in-excel
#See if I can make the graph split into first cust and reorder.
#https://www.kaggle.com/hendraherviawan/customer-purchasing-patterns
pos_filtered$InvoiceYearMonth <- format(pos_filtered$InvoiceDate, "%Y-%m")
year_month_rev <- aggregate(pos_filtered$Revenue, list(pos_filtered$InvoiceYearMonth), sum)
year_month_rev <- rename(year_month_rev, Date = Group.1, Revenue = x)

ggplot(data=year_month_rev, aes(Date, Revenue)) +
  geom_bar(stat="identity", fill ="#56B4E9") +
  labs(title="Revenue by Year-Month", x = "Year-Month", y = "Revenue in Sterling") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels=comma)

#barplot of weekly revenue
# graph by week:
pos_filtered$InvoiceYearWeek <- format(pos_filtered$InvoiceDate, "%Y-%U")
year_week_rev<- aggregate(pos_filtered$Revenue, list(pos_filtered$InvoiceYearWeek), sum)
year_week_rev <- rename(year_week_rev, Date = Group.1, Revenue = x)

ggplot(data=year_week_rev, aes(Date, Revenue)) +
  geom_bar(stat="identity", fill ="#56B4E9") +
  labs(title="Revenue by Year-Week", x = "Year-Week", y = "Revenue in Sterling") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels=comma)

#scatter of daily revenue regression line
pos_filtered$InvoiceDay <-  yday(pos_filtered$InvoiceDate)
day_rev<- aggregate(pos_filtered$Revenue, list(pos_filtered$InvoiceDay), sum)
day_rev <- rename(day_rev, Date = Group.1, Revenue = x)

ggplot(data=day_rev, aes(Date, Revenue)) +
  geom_point(color='steelblue') +
  labs(title="Revenue by Year-Day", x = "Year-Day", y = "Revenue in Sterling") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels=comma) +
  geom_smooth(method = 'lm', color='red')

revenue.lm <- lm(Revenue ~ Date, day_rev)
summary(revenue.lm)
# p-value is much less than 0.05 so reject
# null hypothesis that there is beta = 0.
# Hence, there is a significant relationship between
# the variables in the linear regression model


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
ggplot(weekly_freq)

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

install.packages("ggthemes")
library(ggthemes)

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
#https://analyzecore.com/2015/02/16/customer-segmentation-lifecycle-grids-with-r/
#http://blog.jimnovo.com/2007/04/25/engagement-customers/

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
grid_info <- rf %>%
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
grid_table <- dcast(grid_summarise , 
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

#Clustering Customers-------------------------------------------
#Create RFM table
customer_m <- aggregate(pos_filtered$Revenue , list(pos_filtered$CustomerID),sum) 
customer_m <- rename(customer_m, CustomerID = Group.1, Cust_Monetary =x)

rfm <-sqldf("SELECT rf.CustomerID, rf.Cust_Frequency,
            rf.Cust_Recency, Cust_Monetary
            FROM rf JOIN customer_m
            ON rf.CustomerID = customer_m.CustomerID")
rfm

#k-means clustering
rfm$Cust_Frequency.log <- log(rfm$Cust_Frequency)
rfm$Cust_Recency.log <- log(rfm$Cust_Recency)
rfm$Cust_Monetary.log <- log(rfm$Cust_Monetary)

rfm$Cust_Frequency.z <- scale (Cust_Frequency.log, 
                               center = TRUE,
                               scale = TRUE)
rfm$Cust_Recency.z <- scale (Cust_Recency.log, 
                             center = TRUE,
                             scale = TRUE)
rfm$Cust_Monetary.z <- scale (Cust_Monetary.log, 
                              center = TRUE,
                              scale = TRUE)


for (k in 1:10){
  print(k)
  output <- kmeans(rfm[8:10], centers = k, nstart = 20)
  cluster <- paste ("cluster", k, sep=" ")
  rfm[, cluster] <- output$cluster
  rfm[, cluster] <- factor(rfm[, cluster], c(1:k))
  
  colors <- c('red', 'green4', 'dodgerblue2', 'hotpink1', 'black',
              'gold1', 'violet', 'deepskyblue', 'chocolate3',
              'cadetblue3') 
  title <- paste("k-means with cluster:", k, sep = " ")
  graph <- ggplot(rfm, aes(x = Cust_Frequency.log, y = Cust_Monetary.log)) +
    geom_point(aes(colour = rfm[,cluster])) +
    scale_color_manual(name = "Cluster", values = colors) +
    xlab("Log-Transformed Frequency") +
    ylab("Log-Transformed Monetary") 
  title <- paste("k-means with cluster:", k, sep = " ") 
  graph <- graph + ggtitle(title) 
  
  print(graph)
}

for (k in 1:10){
  print(k)
  output <- kmeans(rfm[8:10], centers = k, nstart = 20)
  cluster <- paste ("cluster", k, sep=" ")
  rfm[, cluster] <- output$cluster
  rfm[, cluster] <- factor(rfm[, cluster], c(1:k))
  
  colors <- c('red', 'green4', 'dodgerblue2', 'hotpink1', 'black',
              'gold1', 'violet', 'deepskyblue', 'chocolate3',
              'cadetblue3') 
  title <- paste("k-means with cluster:", k, sep = " ")
  graph <- ggplot(rfm, aes(x = Cust_Frequency.log, y = Cust_Recency.log)) +
    geom_point(aes(colour = rfm[,cluster])) +
    scale_color_manual(name = "Cluster", values = colors) +
    xlab("Log-Transformed Frequency") +
    ylab("Log-Transformed Monetary") 
  title <- paste("k-means with cluster:", k, sep = " ") 
  graph <- graph + ggtitle(title) 
  
  print(graph)
}



kmeans_subset <- subset (rfm.log, select = -Cust_Recency.log)
km.out <- kmeans(kmeans_subset, 2, nstart=20)
plot(kmeans_subset, col = km.out$cluster + 1, pch = 20, cex = 2)



ggplot(rfm, aes(log(rfm$Cust_Frequency), log(rfm$Cust_Monetary))) +
  geom_point(color='steelblue') +
  labs(title="Revenue by Year-Day", x = "Year-Day", y = "Revenue in Sterling") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels=comma) 

scatter


rfm$Cust_Frequency.log <- log(rfm$Cust_Frequency)
rfm$Cust_Recency.log <- log(rfm$Cust_Recency)
rfm$Cust_Monetary.log <- log(rfm$Cust_Monetary)


#---------------------------------------------------------------
#Interesting to see which items are most frequently bought
#which items bring in most revenue

#https://www.r-bloggers.com/customer-segmentation-part-1-k-means-clustering/
#http://www.kimberlycoffey.com/blog/2016/8/k-means-clustering-for-customer-segmentation
#Identify high-and low-value customers for marketing purposes.

install.packages("tidytext")
library(tidytext)

#most common word in description
table <- unnest_tokens(pos_filtered, word, Description)
a <- anti_join(table, stop_words)
dplyr::count(table, word, sort = TRUE)

install.packages("tm")
install.packages("ddalpha")
install.packages("caret")
install.packages("lme4", dependencies = TRUE)
library(ddalpha)
library(lattice)
library(caret)
library(tm)
library(Matrix)
library(lme4)


#Cretae a Document Term Matrix and assign a classification 
descrip_corpus <- Corpus(VectorSource(pos_filtered$Description))

tdm <- DocumentTermMatrix(descrip_corpus, 
                          list(removePunctuation = TRUE, 
                               stopwords = TRUE, 
                               stemming = TRUE, 
                               removeNumbers = TRUE))
tdm <- removeSparseTerms(tdm, 0.999)

#Elbow Method for finding the optimal number of clusters
set.seed(5)
library(cluster)

library(data.table)
fwrite(cluster_table, "cluster.csv")

wss <- sapply(3:13, 
              function(k){kmeans(tdm, k)$tot.withinss})
plot(3:13, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "Within-Cluster Sum of Squares by Cluster Number")

cluster_9 <- kmeans(tdm, 9)
k_num <- 9


unique(pos_filtered$Description)


cluster_table <- pos_filtered


for (i in 1:k_num){
  assign(paste0("items_in_cluster",i), unique(cluster_table[cluster_9$cluster == i, ]$Description))
}


for (i in 1:k_num){
  fwrite(list(get(paste0("items_in_cluster", i))), paste0(i, "cluster_items.csv"))
}



install.packages("wordcloud2")
library(RColorBrewer)
library(wordcloud)


for (c in 1:k_num){
  l <- list()
  index <- 1
  for (i in get(paste0("items_in_cluster", c))){
    i <- tolower(i)
    word<-removeWords(i,c("colour", "small", "set", "green", "red", "blue", "colour", "white", "pink", "black"))
    l[index] <- word
    index <- index + 1
  }
  
  descrip_corpus <- Corpus(VectorSource(l))
  
  tdm <- DocumentTermMatrix(descrip_corpus)
  
  m <- as.matrix(tdm)
  m <- t(m)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word = names(v), freq = v)
  
  set.seed(1234)
  plot(0,0,main=c)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"), gridsize=15, scale=c(4,.5))
  
}
#Customer Categorization based on purchase history----------
cluster_table$Spent <- cluster_table$Quantity*cluster_table$UnitPrice
cluster_table$prod_clust <- cluster_9$cluster

cust_category <- cluster_table[, c("CustomerID", "prod_clust", "Spent")]

#total spent on each cluster
for(i in c(1:k_num)){
  assign(paste0("spent_total_",i), sum(cust_category$Spent*(cust_category$prod_clust==i)))
}

#spent on each cluster per invoice
cust_category <- as.data.frame(cust_category)
for(i in 1:k_num){
  cust_category[, ncol(cust_category) + 1] <- cust_category$Spent*ifelse(cust_category$prod_clust==i,1,0)
  names(cust_category)[ncol(cust_category)] <- paste0("spent_", i)
  
  pos_filtered[, ncol(pos_filtered) + 1] <- cust_category$Spent*ifelse(cust_category$prod_clust==i,1,0)
  names(pos_filtered)[ncol(pos_filtered)] <- paste0("spent_", i)
}


#prop spent on each cluster per invoice
for(i in 1:k_num){
  pos_filtered[, ncol(pos_filtered) + 1] <- cust_category[,paste0("spent_",i)]/cust_category$Spent
  names(pos_filtered)[ncol(pos_filtered)] <- paste0("prop_in_cluster", i)
}


#sum in each category per customerid
for(i in c(1:k_num)){
  assign(paste0("sum_in_",i), sqldf(sprintf("SELECT CustomerID, 
                                            SUM(spent_%s) 
                                            FROM cust_category 
                                            GROUP BY CustomerID", i)))
}

#add columns for total spent in each category per customer 
#calculated earlier
for (i in 1:k_num){
  cust_category <- merge(cust_category, get(paste0("sum_in_", i)), by="CustomerID")
}

#rename columns
index <- 13
for (i in 1:k_num){
  colnames(cust_category)[index] <- paste0("sum_in_", i)
  index <- index + 1
}

cust_category <- sqldf ("SELECT CustomerID, 
                        sum_in_1, sum_in_2, sum_in_3,
                        sum_in_4, sum_in_5, sum_in_6,
                        sum_in_7, sum_in_8, sum_in_9
                        FROM cust_category")
cust_category <- cust_category[!duplicated(cust_category),]

#spent overall per customer
cust_category$sum_overall <- cust_category$sum_in_1 + 
  cust_category$sum_in_2 + cust_category$sum_in_3 + 
  cust_category$sum_in_4 + cust_category$sum_in_5 +
  cust_category$sum_in_6 + cust_category$sum_in_7 +
  cust_category$sum_in_8 + cust_category$sum_in_9 

#proportion spent in each category per customer
for(i in 1:k_num){
  cust_category[, ncol(cust_category) + 1] <- cust_category[,paste0("sum_in_",i)]/cust_category$sum_overall
  names(cust_category)[ncol(cust_category)] <- paste0("prop_", i)
}

#categorize customers based on max proportion spent among clusters
cust_category$Category <- max.col(cust_category[,c("prop_1","prop_2",
                                                   "prop_3", "prop_4",
                                                   "prop_5", "prop_6", 
                                                   "prop_7","prop_8", "prop_9")]
                                  ,ties.method = "first")

#see how many customers in each category
no_na <- cust_category[!is.na(cust_category$Category),]
no_na$Category <- as.vector(no_na$Category)
one <- sum(no_na$Category == 1)
two <- sum(no_na$Category == 2)
three <- sum(no_na$Category == 3)
four <- sum(no_na$Category == 4)
five <- sum(no_na$Category == 5)
six <- sum(no_na$Category == 6)
seven <- sum(no_na$Category == 7)
eight <- sum(no_na$Category == 8)
nine <- sum(no_na$Category == 9)

pos_filtered$Cust_Category <- cust_category$Category[match(pos_filtered$CustomerID, cust_category$CustomerID)]


#fit 
pos_filtered$Revenue <- pos_filtered$UnitPrice * pos_filtered$Quantity
prop_model_data <- sqldf("SELECT Revenue, prop_in_cluster1, 
                         prop_in_cluster2, prop_in_cluster3,
                         prop_in_cluster4, prop_in_cluster5,
                         prop_in_cluster6, prop_in_cluster7,
                         prop_in_cluster8, prop_in_cluster9,
                         Cust_Category 
                         FROM pos_filtered")

spent_model_data <- sqldf("SELECT Revenue, spent_1, 
                          spent_2, spent_3, spent_4, spent_5,
                          spent_6, spent_7, spent_8, spent_9,
                          Cust_Category FROM pos_filtered")

#pick wich model to test
model_data$Cust_Category <- as.factor(spent_model_data$Cust_Category)
#test
a<-round(nrow(pos_filtered)*0.7)
set.seed(123)
train_ind <- sample(seq_len(nrow(model_data)), size = a)
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]

#randomForest
install.packages("randomForest")
library(randomForest)
model <- randomForest(Cust_Category ~., data = train, na.action = na.exclude)
pred <- predict(model, test, type = "response")
conf <- table(test$Cust_Category, pred)
accuracy <- sum(diag(conf))/nrow(test)
plot(model, main = "Random Forest Error Plot")




# hierarchical cluster--------------------------------------
product_name <- unique(pos_filtered$StockCode)
customers <- unique(pos_filtered$CustomerID)


y <- with(pos_filtered, table(CustomerID, StockCode))
y <- as.data.frame(y)
y <- data.frame ( table ( pos_filtered$CustomerID, pos_filtered$StockCode)[,])

install.packages("gmodels")
library(gmodels)
CrossTable(pos_filtered$CustomerID, pos_filtered$StockCode)
y <- xtabs( ~ CustomerID + StockCode, pos_filtered)
scaled_y <- scale(y)
par(mfrow = c(1,3))
data.dist <- dist(scaled_y)

saveRDS(data.dist, "data_dist.rds")

plot(hclust(data.dist), labels=customers, main="Complete
     Linkage", xlab="", sub="",ylab="")

plot(hclust(data.dist, method="average"), labels=customers,
     main="Average Linkage", xlab="", sub="",ylab="")

plot(hclust(data.dist, method="single"), labels=customers,
     main="Single Linkage", xlab="", sub="",ylab="")

# use complete linkage hierarchical clustering to cut
hc <- hclust(data.dist)
hc.clusters <- cutree(hc, k=4)
h_cluster <- table(hc.clusters, customers)


par(mfrow = c(1,1))
plot(hc, main="Complete Linkage", labels=FALSE)
hcd <- as.dendrogram(hc)

plot(cut(hcd, h= 150)$lower[[1]], main = "Upper tree of cut at h= 350", xlim = c(1, 4800))


hr <- range(hc$height) 
tol<- diff(hr)/100    # set tolerance level
lone <- c()
index <- 1
for(i in seq(1e-4+hr[1],hr[2],tol)){ 
  hcc <- rect.hclust(hc,h=i) 
  if(all(sapply(hcc,length)>1)) break
} 
str(hcc) 

dend1<- as.dendrogram(hc) 
for(i in seq(1e-4+hr[1],hr[2],tol)){ 
  dend2 <- cut(dend1,h=i) 
  if(all(sapply(dend2$lower,function(x) attr(x,'members'))>1)) break 
} 
dend2 

d2=color_branches(dend2,k=5) # auto-coloring 5 clusters of branches.
plot(d2)

install.packages("ggdendro")
install.packages("dendextend")
library(dendextend)
library(ggdendro)
heights_per_k.


