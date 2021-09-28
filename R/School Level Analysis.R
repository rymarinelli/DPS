library(openxlsx)
library(tidyverse)
library(hrbrthemes)
library(ggridges)
library(patchwork)
library(xts)
library(dygraphs)
library(lubridate)
library(plm)
library(car)

df <- read.xlsx("https://github.com/rymarinelli/DPS/blob/main/data/Data%20and%20Codebook.xlsx?raw=true", sheet = "Data")
network <- read.xlsx("https://github.com/rymarinelli/DPS/blob/main/data/Data%20and%20Codebook.xlsx?raw=true", sheet = "Network")
codebook <- read.xlsx("https://github.com/rymarinelli/DPS/blob/main/data/Data%20and%20Codebook.xlsx?raw=true", sheet = "Codebook")

# Data Quality 
#Check for missing data 

df <- df %>% na.omit()
df <- df %>% subset(df$ACT_SS != 0) 


#High Level Reporting 

aggregate(df$ACT_SS, list(df$Subject.Area), mean)

box_plot <- ggplot(df, aes(Subject.Area, ACT_SS)) + geom_jitter(aes(color = Subject.Area), width = 0.2) + geom_boxplot(alpha = .7) +
  xlab("Subject Areas") + ylab("ACT   Scores") + ggtitle("Comparison of ACT Scores By Subject Areas") + theme(legend.background = element_rect(fill="lightblue",
                                                                                                                                               size=0.4, linetype="solid", 
                                                                                                                                               colour ="darkblue"))

English_Ridge <- ggplot(df %>% subset(Subject.Area == "English"), aes(x = ACT_SS, y = School.Name)) + geom_density_ridges(fill = "pink") + ggtitle("Distribution of English ACT Scores By School") + xlab("English ACT Scores") + ylab("Schools") + theme_gray() 

Science_Ridge <- ggplot(df %>% subset(Subject.Area == "Science"), aes(x = ACT_SS, y = School.Name)) + geom_density_ridges(fill = "lightgreen") + ggtitle("Distribution of Science ACT Scores By School") +  xlab("Science ACT Scores") + ylab("Schools") + theme_gray()

Reading_Ridge <- ggplot(df %>% subset(Subject.Area == "Reading"), aes(x = ACT_SS, y = School.Name)) + geom_density_ridges(fill = "#FFD580") + ggtitle("Distribution of Reading ACT Scores By School") + xlab("Reading ACT Scores") + ylab("Schools") + theme_gray()

Math_Ridge <- ggplot(df %>% subset(Subject.Area == "Math"), aes(x = ACT_SS, y = School.Name)) + geom_density_ridges(fill = "lightblue") + ggtitle("Distribution of Math ACT Scores By School") + xlab("Math ACT Scores") + ylab("Schools") + theme_gray()

ridge_plot_1 <- English_Ridge + Reading_Ridge 

ridge_plot_2 <- Science_Ridge + Math_Ridge

#Find the students in trouble 
#are 18 in English
# 22 in Mathematics
# 22 in Reading
# 18 in Science

# 1. The number of students per schools that do not meet these critea 
# 2.  count of students per subject per school that are not passing
# 3. Average difference per subject per school that are not passing 

student_df <- df %>% subset( (Subject.Area == "Science" & ACT_SS < 18) | (Subject.Area == "English" & ACT_SS < 18)
                             |(Subject.Area == "Reading" & ACT_SS < 22) | (Subject.Area == "Math" & ACT_SS < 22))

#quality check to confirm filtering of values to match logic
aggregate(student_df$ACT_SS, list(student_df$Subject.Area), max)

plot_df <- student_df %>% group_by(School.Name, Subject.Area) %>% count()

count_plot <- ggplot(plot_df, aes(x=School.Name, y=n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + ggtitle("Count of Students Not College Ready Per School") + ylab("Schools") + xlab("Count of Students") + scale_fill_brewer(palette="Dark2")

detail_df <- student_df %>% group_by(School.Name, Subject.Area) %>% count()

reading_count <- detail_df %>% subset(Subject.Area == "Reading")
math_count <- detail_df %>% subset(Subject.Area == "Math")
science_count <- detail_df %>% subset(Subject.Area == "Science")
english_count <- detail_df %>% subset(Subject.Area == "English")


reading_plot <- ggplot(reading_count, aes(x=School.Name, y=n)) + 
  geom_bar(stat = "identity", fill = "#FFD580") + 
  coord_flip() + ggtitle("Count of Students With Reading Scores Below Performance Goals") + xlab("Schools") + ylab("Count of Students") + scale_fill_brewer(palette="Dark2")

math_plot <- ggplot(math_count, aes(x=School.Name, y=n)) + 
  geom_bar(stat = "identity", fill = "lightblue" ) + 
  coord_flip() + ggtitle("Count of Students With Math Scores Below Performance Goals") + xlab("Schools") + ylab("Count of Students") + scale_fill_brewer(palette="Dark2")

science_plot <- ggplot(science_count, aes(x=School.Name, y=n)) + 
  geom_bar(stat = "identity", fill = "lightgreen") + 
  coord_flip() + ggtitle("Count of Students With Science Scores Below Performance Goals") + xlab("Schools") + ylab("Count of Students") + scale_fill_brewer(palette="Dark2")

english_plot <- ggplot(english_count, aes(x=School.Name, y=n)) + 
  geom_bar(stat = "identity", fill = "pink") + 
  coord_flip() + ggtitle("Count of Students With English Scores Below Performance Goals") + xlab("Schools") + ylab("Count of Students") + scale_fill_brewer(palette="Dark2")


count_plot_1 <- english_plot + reading_plot 

count_plot_2 <- science_plot + math_plot

count_plot_1
count_plot_2
