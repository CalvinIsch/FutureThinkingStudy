# Calvin Isch
# 6/29/2019
# Analyzing the data from future thinking experiment

library(ggplot2)
library(jsonlite)
library(stringr)
library(RWeka)
library(data.table)

# Import the data:
all_data <- read.csv(file = "data_for_calvin.csv", header = TRUE)

# get the conditions we care about
yard_sale <- all_data[all_data$free_sort == "yardsale",]
camping <- all_data[all_data$free_sort == "camping",]
all_conditions <- all_data[all_data$free_sort == "camping" | all_data$free_sort == "yardsale",]

# testing some ideas on how to clean data
movement <- all_data$final_locations[69]
checkit <- fromJSON(as.character(movement))
checkit$src <- substring(checkit$src,11,str_length(checkit$src)-4)

# Look what the final list might have looked like
plot(x= c(0,1200), y= c(0,500))
text(x= checkit3$x, y= checkit3$y, labels = checkit3$src)

# Test for figuring out the right k
checkit2 <- data.table(checkit$x,checkit$y)
k <- list()
for(i in 1:15){
  k[[i]] <- kmeans(checkit[,2:3], i)
}
betweenss_totss <- list()
for(i in 1:15){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:15,betweenss_totss,type="b", ylab = "Between SS / total SS", xlab = "Clusters (k)")

# looks the between/total SS and only returns those above specific amount
k_num <- 0
for(i in 1:15){
  x <- k[[i]]$betweenss/k[[i]]$totss
  if (x > .98) {
    k_num <- i
    break 
  } else {
    next
  }
}
print(k_num)


# Create a function to put it all together and get the k-estimate for each subject
how_many_groups <- function(final,sub) {
  k_n <- 0
  k1 <- list()
  for(i in 1:15){
    k1[[i]] <- kmeans(final[,2:3], i)
  }
  betweenss_totss1 <- list()
  for(i in 1:15){
    betweenss_totss1[[i]] <- k1[[i]]$betweenss/k1[[i]]$totss
  }
  
  for(i in 1:15){
    x <- k1[[i]]$betweenss/k1[[i]]$totss
    if (x > .98) {
      k_n <- i
      print(i)
      break 
    } else {
      print(x)
      next
    }
  }
  ppt <- data.table(ID = c("subject", "k"), subject = sub, k = k_n)
  return(ppt)
}
checkit3<- fromJSON(as.character(yard_sale$final_locations[2]))

work <- how_many_groups(checkit, "Me")
work2 <- how_many_groups(checkit3, "you")


# By comparing checkit3 and checkit, it is evident that there is two much
# variance among subjects to use an automatic classifier based on SSE. 
# Instead we will have to graph them individually and then report group number
# by hand.



