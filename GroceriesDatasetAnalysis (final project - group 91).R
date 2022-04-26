# calling libraries
library(tidyverse) # to clean the data
library(tidyr) # to clean the data
library(plyr) # to calc the sum of columns
library(ggplot2) # for plotting
library(dplyr) # to mutate and filter
library(scales) # to change the format of the plot
library(plotly) # to make a pie chart
library(ggpubr) # for K-means clustering visualization
library(arules) # for apriori algorithm
library(arulesViz) # for apriori algorithm
library(RColorBrewer) # to color
library(gtools) # Permutations
library(factoextra) # for K-means clustering visualization

# Importing the data set
setwd("D:/FCDS/DS/Final Project/R Final Project/grcDataset-master")
getwd()
data <- read.csv("grc.csv", na.strings = "")

# Cleaning the data
pivot_wider(data)

# Reordering the age row
data <- data %>% arrange(age)

png(file = "allInOne.png", width=900,height=700) # to save the chart as png
par(mfrow = c(2,2),
    oma = c(0,0,2,0),
    col.axis = "white",
    col.lab = "white",
    tck = 0)

# Comparing cash and credit totals
pieColors <- c("#A6ECB9", "#4B865B") # the colors of the chart
cashSum <- sum(data$paymentType == 'Cash') / 100 # get the percentage of Cash payment
creditSum <- sum(data$paymentType == 'Credit') / 100 # get the percentage of credit payment
compPay <- c(cashSum, creditSum) # put them both in a vector for plotting
compPay * 100
png(file = "paymentType.png", width=900, height=700) # to save the pie chart as png
# Plotting the chart as pie chart
pie(x = compPay,
     labels = compPay,
     main = "Comparing cash and credit totals",
     col = pieColors)
# a box that shows what's in the pie chart
legend("bottomright",
       c("Credit", "Cash"),
       cex=1.2,
       fill=pieColors)
box(col = "white")
dev.off()

# Comparing each age and sum of total spending
filteredData <- ddply(data,"age",numcolwise(sum)) # to calc the total spending of age rows
png(file = "age&TotalSpending.png", width=900, height=700) # to save the chart as png
# Plotting the chart as bar chart
filteredData
barplot(
  height = filteredData$total,
  name = filteredData$age,
  col = c("#0D383E", "#2291A0", "#2291A0", "#2291A0", "#2291A0", "#2291A0", "#2291A0", "#0D383E", "#2291A0", "#2291A0", "#0D383E", "#2291A0"),
  main = "Comparing each age and sum of total spending",
  xlab = "age",
  ylab = "total spending")
# a box that shows what's in the bar chart
legend("topright",
       c("over 1.5M", "under 1M"),
       fill = c("#0D383E", "#2291A0"))
box(col = "white")
dev.off()



#  Show each city total spending and arrange it by total descending
filteredData2 <- ddply(data,"city",numcolwise(sum)) # to calc the sum of total spending of city row
filteredData2 <- filteredData2[order(desc(filteredData2$total)), ] # sort the total in descending order
png(file = "city&TotalSpending.png", width=900,height=700) # to save the chart as png
# Plotting the chart as bar chart
filteredData2
barplot(
  height = filteredData2$total,
  name = filteredData2$city,
  col = c("#2E521B", "#2E521B", "#529431", "#64B43C", "#64B43C", "#64B43C", "#64B43C", "#64B43C", "#64B43C", "#64B43C", "#64B43C", "#64B43C"),
  main = "Comparing each city and sum of total spending",
  xlab = "city",
  ylab = "total spending")
# a box that shows what's in the bar chart
legend("topright",
       c("over 2.5M", "over 1.5M", "under 1M"),
       fill = c("#2E521B", "#529431", "#64B43C"))
box(col = "white")
dev.off()

# Display the distribution of total spending.
pieColors2 <- c("#479E00", "#9E8900", "#9E1C00", "#9E0035", "#9E0086") # the colors of the pie chart
filteredData3 <- ddply(data,"customer",numcolwise(sum)) # total spending for each customer
png(file = "distribution.png", width=900,height=700) # to save the chart as png
# Plotting as pie chart
pie(x = filteredData3$total,
    labels = filteredData3$customer,
    main = "The distribution of total spending",
    col = pieColors2)
box(col = "white")
filteredData3
title("all plots in one dashboard", outer = TRUE)
dev.off()

# more specific pie chart
disPie <- plot_ly(data, labels = customers, values = totals, type = 'pie')
disPie <- fig %>% layout(title = 'the distribution of total spending',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
disPie


# Clustering
dataPoints <- cbind(data$total, data$age) # declaring the data points that is the total spending of each age
rownames(dataPoints) <- data$customer # naming the rows by each customer name
colnames(dataPoints) <- c("Total", "Age") #naming the cols by "total" and "age"
dataPoints

numberOfClusters <- as.numeric(readline("Enter the number of clusters (choose from 2:4): ")) # to input the number of clusters

result <- kmeans(dataPoints, centers = numberOfClusters) # run the clustering function
data <- mutate(data, result$cluster) # add the clustering row to the data we have
colnames(data)[9] <- "cluster" # naming the col of clusters

result$cluster

# plotting clusters
png(file = "clusters.png", width=900,height=700) # to save the chart as png
fviz_cluster(result, data = dataPoints[, -5],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
dev.off()

# Apriori
write.table(data$items,"items.txt") # to save the items column as txt
tdata <- read.csv("APitems.csv",stringsAsFactors=FALSE) # read the items column is saves

transactions<-strsplit(as.vector(tdata$items), ',') #Create list of transactions vector
transaction_count <- length(transactions) # to get how many items

unique_items<-unique(unlist(transactions)) # to get unique items
unique_items

minSupport <- as.numeric(readline("Enter the minimum support: ")) # let the user input minimum support
minConfidence <- as.numeric(readline("Enter the minimum Confidence: ")) # let the user input minimum Confidence


get_occurrences <- function(item_names){
  names_matrix <- item_names
  if(!is.matrix(item_names))
    names_matrix <- matrix(item_names)
  result <- c()
  for (name_index in 1:nrow(names_matrix)) {
    name_row <- names_matrix[name_index,]
    result[name_index] <- 0
    for (tran_index in 1:length(transactions)) {
      transaction <- transactions[tran_index]
      if(length(intersect(name_row,transaction[[1]])) >= length(name_row))
      {
        result[name_index] <-result[name_index]+1
      }
    }
  }
  return(result)
}

# do single items
single_items <- data.frame(item = unique_items, stringsAsFactors=FALSE) # to data frame the unique items

single_items <- mutate(single_items,support = get_occurrences(item)/ transaction_count ) # to calc support for each item

single_items <- filter(single_items,support >= minSupport) # to filter items that has support less than the minSupport
single_items
double_combinations_matrix <- combn(single_items$item,2) # Combine the selected single item sets


# data frame the double items
double_items <- data.frame( 
  item1=double_combinations_matrix[1,],
  item2=double_combinations_matrix[2,]
  , stringsAsFactors = FALSE
)

double_items


double_items <- mutate(double_items,
                       support = get_occurrences(cbind(item1,item2))
                       /transaction_count) # to calc support for each item
double_items
double_items <- filter(double_items,support>=minSupport) # to filter items that has support less then the minSupport
double_items

all_double_items <- union(double_items$item1 ,double_items$item2) # to union double items

triple_combinations_matrix <- combn(all_double_items,3) # Combine the selected double item sets
triple_combinations_matrix
# data frame the triple items
triple_items <- data.frame(
  item1=triple_combinations_matrix[1,],
  item2=triple_combinations_matrix[2,],
  item3=triple_combinations_matrix[3,],
  stringsAsFactors = FALSE)
triple_items


triple_items <- mutate(triple_items,
                       support = get_occurrences(cbind(item1,item2,item3))
                       /transaction_count) # to calc support for each item
triple_items <- filter(triple_items,support>=minSupport) # # to filter items that has support less then the minSupport

triple_items


# create a data frame to fill with rules
rules <- data.frame(
  item1=c(),
  item2=c(),
  support=c(),
  confidence=c(),
  stringsAsFactors = FALSE)

# loop over the selected item sets to create the rules
for (r_indx in 1:nrow(triple_items)) {
  #get current one set
  row <- triple_items[r_indx,]
  #create all available items set from the current set
  permutations<- permutations(n=3,r=3,c(row$item1,row$item2,row$item3),2)
  all_available_rules <- data.frame(
    item1=permutations[,1],
    item2=permutations[,2],
    item3=permutations[,3],
    support= get_occurrences(permutations)/ transaction_count,
    confidence= get_occurrences(permutations) / get_occurrences(permutations[1,]))
  #append to rules
  rules <- rbind(
    rules,
    all_available_rules
  )
}

#to filter the rules
rules <-filter(rules,support>=minSupport & confidence>=minConfidence)
rules


png(file = "ap.png", width=900,height=700) # to save the chart as png
arules::itemFrequencyPlot(tdata, topN = 20,
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")
dev.off()
