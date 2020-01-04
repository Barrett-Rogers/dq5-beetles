library(tidyverse)
library(ggplot2)
library(readxl)

# Bring in districts 
dist <- read_csv('data/districts.csv')
dist


# Bring in the district profile
profile <- read_excel('data/data_2014-15_district_profile.xlsx')
head(profile) 

summary(profile)

summary(dist)







# Notice that the mean (and everything else) is skewed because of State of Tennessee; Remove Tennessee

new_profile <- filter(profile, DISTRICT_NAME != "State of Tennessee")
summary(new_profile)






new_dist <- filter(dist, system_name != "State of Tennessee")
summary(new_dist)





max(profile, teachers)




# Plot students per teacher 

ggplot(new_profile, aes(x = TEACHERS, y = TOTAL)) +
  geom_point() +
  labs(title = 'Students VS. Teachers')





# Plot white student pct per teacher? Originally used WHITE_PCT but this isn't the one that matters 

ggplot(new_profile, aes(x = TEACHERS, y = WHITE)) +
  geom_point()




# Make new column for total ratio
new_profile$STU_TEACH <- (new_profile$TOTAL)/(new_profile$TEACHERS)


new_profile <- new_profile %>% 
  mutate(STU_TEACH = TOTAL/TEACHERS)



summary(new_profile)
View(new_profile)





# Make plot for Af_American student to teacher 

ggplot(new_profile, aes(x = TEACHERS, y = AFRICAN_AMERICAN_PCT)) +
  geom_point()


new_profile <- new_profile %>% 
  mutate(AF_AM_RATIO = AFRICAN_AMERICAN/TEACHERS)








#In new_profile, I want district to become "system"

new_prof_dist_2 <- new_profile %>% 
  rename(system = DISTRICT)

new_both <- new_prof_dist_2 %>% 
  inner_join(new_dist, by = 'system')





# AF grad ratio 

AF_AM_GRADS <- ggplot(new_both, aes(x = AF_AM_RATIO, y = grad)) +
  geom_point()

AF_AM_GRADS

AF_AM_GRADS <- ggplot(new_both, aes(x = AF_AM_RATIO, y = grad)) +
  geom_point() +
  labs(title="African American Student to Teacher Ratio vs Graduation", x="African American Ratio", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20)) 

AF_AM_GRADS




# White grad ratio 

new_both <- new_both %>% 
  mutate(WHITE_RATIO = WHITE/TEACHERS)

WHITE_GRADS <- ggplot(new_both, aes(x = WHITE_RATIO, y = grad)) +
  geom_point() +
  labs(title="White Student to Teacher Ratio vs Graduation", x="White Ratio", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20)) 

WHITE_GRADS





# Duplicate 
# new_both <- new_both %>% 
  # mutate(TOTAL_RATIO = TOTAL/TEACHERS)


# A little experiment 
TOTAL_GRADS <- ggplot(new_both, aes(x = TOTAL_RATIO, y = grad)) +
  geom_point()

TOTAL_GRADS

TOTAL_GRADS_log <- ggplot(new_both, aes(x = log(TOTAL_RATIO), y = log(grad))) +
  geom_point()

TOTAL_GRADS_log


# Find the correlation betwwen grad and student teacher ratio 

cor(new_both$grad, new_both$TOTAL_RATIO, method = "pearson", use = "complete.obs")
cor.test(new_both$grad, new_both$TOTAL_RATIO, method = "pearson", use = "complete.obs")






# Plotting ACT scores and expenditures 

ACT_money <- ggplot(new_both, aes(x = expenditures, y = act_composite)) +
  geom_point()
ACT_money






# ACT/Dropout 

ACT_dropout <- ggplot(new_both, aes(x = dropout, y = act_composite)) +
  geom_point()
ACT_dropout







# ED vs dropout and correlation 

ED_dropout <- ggplot(new_both, aes(x = dropout, y = ECONOMICALLY_DISADVANTAGED_PCT)) +
  geom_point()
ED_dropout

cor.test(new_both$dropout, new_both$ECONOMICALLY_DISADVANTAGED_PCT, method = "pearson", use = "complete.obs")
cor.test(new_both$chronic_abs, new_both$dropout, method = "pearson", use = "complete.obs")



ggplot(new_both, aes(x = AF_AM_RATIO, y = grad)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~region)





# Bring in teachers dataset and clean 

teachers <- read_csv('data/teacher_number.csv')
head(teachers)

teachers_cleaned <- filter(teachers, !grepl("Middle|Elementary|ES/MS|K-4|5-8|Primary|Intermediate|Pre-K|Childhood|K-8", SCH_NAME))
View(teachers_cleaned)






# Bring in crosswalk data 

crosswalk <- read_excel('data/data_district_to_county_crosswalk.xls')
head(crosswalk)



crosswalk <- crosswalk %>% 
  rename(system = "District Number")
View(crosswalk)


crosswalk <- crosswalk %>% 
  rename(County = "County Name")







# Merge teachers and crosswalk 

teachers_crosswalk <- teachers_cleaned %>% 
  inner_join(crosswalk, by = 'County')
View(teachers_crosswalk)





# Merge the 4 datasets 

conglomerate <- new_both %>% 
  inner_join(teachers_crosswalk, by = 'system')
View(conglomerate)



# Hari tries to help 
test <- conglomerate %>% select(DISTRICT_NAME, County, SCH_NAME, enrollment, TEACHERS.y) %>% 
  
  group_by(SCH_NAME,County) %>%
    
  summarise(TEACHERS.y = mean(TEACHERS.y, na.rm = TRUE)) #%>% 
  #ungroup() %>% View()





# Xin shares some code 
conglomerate %>% 
  group_by(County) %>% 
  summarise_if(is.numeric, mean) %>% 
  ungroup() %>% 
  View()



conglomerate %>% 
  select(SCH_NAME, County, TEACHERS.y,grad,dropout, system, DISTRICT_NAME, TEACHERS.x, TOTAL, enrollment) %>% distinct() %>% View()


new_both %>% 
  filter(new_both, DISTRICT_NAME == "Davidson County")
View()





# Merge dist and crosswalk 

dist_cross <- dist %>% 
  inner_join(crosswalk, by = 'system')








# Plot economically disadvantaged and region 

ggplot(dist_cross, aes(x = region, y = ed, fill = region)) +
  geom_boxplot() +
  labs(title="Economically Disadvantaged by Region", x="Region", y = "Economically Disadvantaged") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
        theme(text = element_text(size=20)) 









barplot(c(2,5), main="Main title",
        xlab="X axis title",
        ylab="Y axis title",
        sub="Sub-title",
        cex.main=2, cex.lab=1.7, cex.sub=1.2)







# the non-color boxplot for eco disadvantage 
boxplot(ed~region, data=dist_cross, main="Economically Disadvantaged by Region", 
        xlab="ED", ylab="Region", varwidth=TRUE)







# Black to white ratio or black to white total per region in bar plot 

#counts <- new_both$region
#table(new_both$WHITE, new_both$AFRICAN_AMERICAN)
#barplot(counts, main="A Comparison of Total White and African American Students", xlab="Region", col=c("darkblue","red"), legend = rownames(counts))
#ggplot(new_both, aes(x = WHITE, y = AFRICAN_AMERICAN)) + 
  #geom_bar(stat = "identity", position = "dodge") 


counts <- table(new_both$WHITE, new_both$AFRICAN_AMERICAN)
barplot(counts, main="White by region",
        xlab="Region", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

counts <- table(new_both$TOTAL, new_both$region)
barplot(counts, main="Total Student Count by Region", 
        xlab="Regions", ylab = "Student Count")










# THIS IS THE ACTUAL CODE FOR THE GRAPH

ggplot(new_both, aes(x = region, y = TOTAL)) + 
geom_bar(stat = "identity", position = "dodge") +
  labs(title="Total Student Count by Region", x="Region", y = "Student Count")

SW <- new_both %>% 
  filter(region == "Southwest/Memphis")


ggplot(new_both, aes(x = SW, y = TOTAL)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Total Student Count in Southwest/Memphis Region", x="Southwest/Memphis", y = "Student Count")


summary(new_both$region)


View(SW)



#Let's try to view using a scarrter plot: 
ggplot(SW, aes(x = DISTRICT_NAME, y = TOTAL)) +
  geom_point() +
  labs(title="Student Count in Southwest Region", x="Region", y = "Student Count") +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20))




# Some math 
Shelby <- SW$TOTAL %>% 
  filter(DISTRICT_NAME == "Shelby County") 

SW$TOTAL 

SW$dropout
summary(profile)

ggplot(new_both, aes(x = AFRICAN_AMERICAN_PCT, y = grad)) +
  geom_point() +
  labs(title="African American Percentage vs Graduation", x="African American Percentage", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20))





# Same thing for white 
ggplot(new_both, aes(x = WHITE_PCT, y = grad)) +
  geom_point() +
  labs(title="White Percentage vs Graduation", x="White Percentage", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20))



# 
ggplot(new_both, aes(x = region, y = grad)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Graduation Rate by Region", x="Region", y = "Graduation Rate") 



# ED by region

ggplot(new_both, aes(x = region, y = ECONOMICALLY_DISADVANTAGED_PCT, fill = region)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Economically Disadvantaged Percentage by Region", x="Region", y = "Economically Disadvantaged Percentage") 






ggplot(new_both, aes(x = HISPANIC_PCT, y = grad)) +
  geom_point() +
  labs(title="Hispanic Percentage vs Graduation", x="Hispanic Percentage", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20))







ggplot(new_both, aes(x = LIMITED_ENGLISH_PROFICIENT_PCT, y = grad)) +
  geom_point() +
  labs(title="Limited English Proficient Percentage vs Graduation", x="Limited English Proficient Percentage", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20))



ggplot(new_both, aes(x = ASIAN_PCT, y = grad)) +
  geom_point() +
  labs(title="Asian Percentage vs Graduation", x="Asian Percentage", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20))





ggplot(new_both, aes(x = NATIVE_AMERICAN_PCT, y = grad)) +
  geom_point() +
  labs(title="Native American Percentage vs Graduation", x="Native American Percentage", y = "Graduation Rate") +
  theme(axis.text.x = element_text(color = "grey20", size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, angle = 90, hjust = .5, vjust = .5, face = "plain"))+
  theme(text = element_text(size=20))