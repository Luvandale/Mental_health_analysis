library(epitools)
library(tidyverse)
dat1 <-read.csv("/home/imalizzz/Downloads/shamiri_imputed_dataset.csv")
head(dat1)
#checking the attributes of the dataset
attributes(dat1)

# checking the summary of the dataset
summary(dat1)#This shows the summary of every column including the mean of every GAD 

#checking the no of missing values in each column
colSums(is.na(dat1)) # This shows that there are no misiing values in any of the columns

#finding the unique umber of categories under each column
rapply(dat1,function(x)length(unique(x)))

#realizing PHQ3 has option 4 which could be a data entry problem which could be dropped to assist with statistics computations  
categories <- unique(dat1$PHQ3)
categories

#table(dat1$PHQ1)
#finding the overall meanscores of the GAD
overall_mean <- colMeans(dat1[ , c(10, 11,12,13,14,15,16)])
overall_mean
#converting to data frame
data=data.frame(overall_mean)

print(data)
#histogram of overall mean
hist(overall_mean)



#EDA(Exploratory Data analysis)
#Tribe visualization
ggplot(dat1, aes("",fill = Tribe)) +
  geom_bar(position="fill") +
  geom_text(
    stat='count', 
    aes(y=after_stat(..count..),
        label=after_stat(scales::percent(..count../sum(..count..),1))),
    position=position_fill(0.5),
  ) +
  coord_polar(theta="y") +
  labs(x=NULL, y=NULL) +
  scale_fill_brewer(palette="Pastel1") +
  theme_void()

#Minority consists of 66% of the respondents whereas majority constitutes of 34%


#Gender visualization
#The questionnaire had 51% female and 49% male
ggplot(dat1, aes("",fill = Gender)) +
  geom_bar(position="fill") +
  geom_text(
    stat='count', 
    aes(y=after_stat(..count..),
        label=after_stat(scales::percent(..count../sum(..count..),1))),
    position=position_fill(0.5),
  ) +
  coord_polar(theta="y") +
  labs(x=NULL, y=NULL) +
  scale_fill_brewer(palette="Pastel1") +
  theme_void()

#School visualization
# Equivalent to using geom_bar(stat = "bin")
#The school with the highest o of respondents was olympic, followed by AGHS
ggplot(dat1, aes(x = School)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

#School resources
#Most of the schools have rich resources when it comes to mental health
ggplot(dat1, aes(x = School_Resources)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

#School resources vs School
ggplot(dat1, aes(x = School, fill = School_Resources)) +
  geom_bar(width = 0.5, color = 'gray40') +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal(base_size = 16)

# GAD ANALYSIS
#tribe
#Minority had the highest mean in GAD1,GAD2,GAD3,GAD5,GAD6,GAD7  Except for GAD4
df2 <- dat1 %>% group_by(Tribe) %>% 
  summarise(mean_GAD1=mean(GAD1),
            mean_GAD2= mean(GAD2),mean_GAD3=mean(GAD3),mean_GAD4=mean(GAD4),mean_GAD5=mean(GAD5),mean_GAD6=mean(GAD6),mean_GAD7=mean(GAD7),
            .groups = 'drop') %>%
  as.data.frame()
df2


# Load the package
library(tidyr)
library(dplyr)
library(knitr)

# transform the format
data_long <- gather(df2, mean, value, mean_GAD1:mean_GAD7) %>%
  arrange(factor(Tribe, levels = c("Majority", "Minority"))) %>% 
  mutate(Tribe=factor(Tribe, levels=unique(Tribe)))


kable(head(data_long, 10))


data_long %>%
  ggplot(aes(x = mean, y = value, fill = as.factor(Tribe))) +
  geom_bar(stat = "identity", position = "dodge") 

df2_r <- subset(df2, select = -Tribe)
df2_r
rowMeans(df2_r)

#gender
#females have the highest means of GAD from 1 to 7 compared to their male counterparts
df3 <- dat1 %>% group_by(Gender) %>% 
  summarise(mean_GAD1=mean(GAD1),
            mean_GAD2= mean(GAD2),mean_GAD3=mean(GAD3),mean_GAD4=mean(GAD4),mean_GAD5=mean(GAD5),mean_GAD6=mean(GAD6),mean_GAD7=mean(GAD7),
            .groups = 'drop') %>%
  as.data.frame()
df3
# transform the format
df3_long <- gather(df3, mean, value, mean_GAD1:mean_GAD7) %>%
  arrange(factor(Gender, levels = c("Female", "Male"))) %>% 
  mutate(Gender=factor(Gender, levels=unique(Gender)))

kable(head(df3_long, 10))

df3_long %>%
  ggplot(aes(x = mean, y = value, fill = as.factor(Gender))) +
  geom_bar(stat = "identity", position = "dodge")

df3_r <- subset(df3, select = -Gender)
df3_r
rowMeans(df3_r)

#school
df4 <- dat1 %>% group_by(School) %>% 
  summarise(mean_GAD1=mean(GAD1),
            mean_GAD2= mean(GAD2),mean_GAD3=mean(GAD3),mean_GAD4=mean(GAD4),mean_GAD5=mean(GAD5),mean_GAD6=mean(GAD6),mean_GAD7=mean(GAD7),
            .groups = 'drop') %>%
  as.data.frame()
df4

# transform the format
df4_long <- gather(df4, mean, value, mean_GAD1:mean_GAD7) %>%
  arrange(factor(School, levels = c("AGHS", "AHS", "Elite","Olympic","Starays"))) %>% 
  mutate(School=factor(School, levels=unique(School)))

kable(head(df4_long, 10))

df4_long %>%
  ggplot(aes(x = mean, y = value, fill = as.factor(School))) +
  geom_bar(stat = "identity", position = "dodge")

df4_r <- subset(df4, select = -School)
df4_r
rowMeans(df4_r)

#Age
#22 and 25 yr plds have the highest gads score
df5 <- dat1 %>% group_by(Age) %>% 
  summarise(mean_GAD1=mean(GAD1),
            mean_GAD2= mean(GAD2),mean_GAD3=mean(GAD3),mean_GAD4=mean(GAD4),mean_GAD5=mean(GAD5),mean_GAD6=mean(GAD6),mean_GAD7=mean(GAD7),
            .groups = 'drop') %>%
  as.data.frame()
df5

# transform the format
df5_long <- gather(df5, mean, value, mean_GAD1:mean_GAD7) %>%
  arrange(factor(Age, levels = c(12,13,14,15,16,17,18,19,20,20.5,21,22,23,25))) %>% 
  mutate(Age=factor(Age, levels=unique(Age)))

kable(head(df5_long, 10))

df5_long %>%
  ggplot(aes(x = value, y = mean, fill = as.factor(Age))) +
  geom_bar(stat = "identity", position = "dodge")


df5_r <- subset(df5, select = -Age)
df5_r
rowMeans(df5_r)

#School resources
df6 <- dat1 %>% group_by( School_Resources) %>% 
  summarise(mean_GAD1=mean(GAD1),
            mean_GAD2= mean(GAD2),mean_GAD3=mean(GAD3),mean_GAD4=mean(GAD4),mean_GAD5=mean(GAD5),mean_GAD6=mean(GAD6),mean_GAD7=mean(GAD7),
            .groups = 'drop') %>%
  as.data.frame()
df6

# transform the format
df6_long <- gather(df6, mean, value, mean_GAD1:mean_GAD7) %>%
  arrange(factor(School_Resources, levels = c("Medium", "Poor", "Rich"))) %>% 
  mutate(School_Resources=factor(School_Resources, levels=unique(School_Resources)))

kable(head(df6_long, 10))

df6_long %>%
  ggplot(aes(x = mean, y = value, fill = as.factor(School_Resources))) +
  geom_bar(stat = "identity", position = "dodge")


