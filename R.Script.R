library("tidyverse") #load packages

pumpkins <- read.csv(dir(pattern='^pumpkins_')[1]) # Read dataset

pumpkins %>% 
  select(id,city,weight_lbs) %>% # groups data by necessary variables
  arrange(desc(weight_lbs)) %>% #arranges data from heaviest pumpkin to lightest
  head(1) #since heaviest pumpkin is a top of df, head(1) will only report that value

#funciton to convert Ibs to Kg
Ibs_to_Kg <- function(x){
  y = x /  2.205          
  return(y)
}
print("Function created")

#takes pumpkin weight in pounds as 'x' input for the Ibs_to_Kg function
#creates new column based off of converted values
pumpkins$weight_Kg <- sapply(pumpkins$weight_lbs, Ibs_to_Kg) 
print("Ibs converted to Kg")

pumpkins

#uses ifelse statements to organise pumpkin wieght in kgs into three different groups
#creates new column based off of organised values
pumpkins$weight_class <- ifelse(pumpkins$weight_Kg <= 100, "light" ,
                                ifelse(pumpkins$weight_Kg > 100 & pumpkins$weight_Kg <= 500, "medium", 
                                      "heavy"))
#Confirms step has completed
print("Weight grouped")

pumpkins

#Creates scatter plot of estimated vs actual weights of pumpkins 
#saves graph as png
png("Relationship between estimated and actual weights of pumpkins.png")
ggplot(pumpkins, aes(est_weight, weight_lbs, color = weight_class)) +
  geom_point() +
  xlab("Estimated weight(lbs)") +
  ylab("Actual weight(lbs)") +
  ggtitle("Relationship between the estimated and actual weights of pumpkins")
dev.off()
print("Relationship between estimated and actual weights of pumpkins.png created")

#creates new csv file from filtered data set
write.csv(pumpkins %>%
          filter(country == c("USA", "UK", "France")), file = "pumpkins_filtered.csv")
#confirms csv has been created
print("filtered_pumpkins.csv created")

#reads created new csv file into a new data frame 
fpumpkins <- read.csv("filtered_pumpkins.csv")

#calculates mean weights for each country
fpumpkins %>%
  group_by(country) %>%
  summarise(mean_weight = mean(weight_lbs, na.rm = T))


#calculates mean weight for each country and variety and puts result into new data frame
mean_pumpkins<- fpumpkins %>%
                  group_by(country, variety) %>%
                    summarise(mean_weight = mean(weight_lbs, na.rm = T))
#takes previously created data frame and looks for smallest values for country and variety
print("lowest mean value and corresponding Country and variety")
min(mean_pumpkins$mean_weight)
mean_pumpkins$country[which.min(mean_pumpkins$mean_weight)]
mean_pumpkins$variety[which.min(mean_pumpkins$mean_weight)]

#creates box plot of distribution by country
png("Pumpkin weight distribution.png")
ggplot(fpumpkins, aes(country, weight_lbs, color = country)) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Weight(lbs)") +
  ggtitle("Weight distribution of pumpkins across different countries")
dev.off()  
#confirms graph is made
print("Pumpkin weight distribution.jpeg created")

#creates box plot of weight distribution by country
#Additionally has sub plots for each variety of pumpkin in each country
png("Pumpkin weight distribution variety.png")
ggplot(fpumpkins, aes(country, weight_lbs, color = country)) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Weight(lbs)") +
  ggtitle("Weight distribution of pumpkins across different countries") +
  facet_wrap(~ variety)
dev.off()
#confirms graph is made
print("Pumpkin weight distribution variety.png created")
print("End of R script")

         