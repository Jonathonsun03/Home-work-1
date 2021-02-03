library(tidyverse)
library(ggplot2)
library(esquisse)
library(plotly)
library(formattable)

#Case Study 1 ------------------------------
dataset <- read.csv("E:/Documents/Penn-One-Drive/OneDrive - PennO365/2020-2021/SP/Datamining/Home work 1/data/Survey_results_final.csv", 
                    na.strings =  c(""))
head(dataset)
Variables <- as.tibble(colnames(dataset))
View(Variables)

datasetClean <- dataset %>%
  select(starts_with("answer.") | contains("worktime")) %>%
  rename(Age = Answer.Age,
         Education = Answer.Education,
         Gender = Answer.Gender,
         Income = Answer.HouseHoldIncome,
         Sirius = Answer.Sirius.Radio,
         Wharton = Answer.Wharton.Radio,
         Worktime = WorkTimeInSeconds)

summary(datasetClean)

#Age cleaning -----------------------------
AgeUnique <- unique(datasetClean$Age)

x <- which(grepl(AgeUnique[53], datasetClean$Age))
datasetClean$Age[x] <- NA

x <- which(grepl('Eighteen', datasetClean$Age))
datasetClean$Age[x] <- 18

x <- which(grepl(AgeUnique[58], datasetClean$Age))
datasetClean$Age[x] <- 27

x <- which(grepl(AgeUnique[51], datasetClean$Age))
datasetClean$Age[x] <- 27

unique(datasetClean$Age)

datasetClean <- datasetClean %>% 
  filter(grepl("[[:digit:]]",Age))


datasetClean$Age <- as.numeric(datasetClean$Age)
summary(datasetClean)

#Education level Factor -----------------------
datasetClean$Education <- str_remove(datasetClean$Education, "â€™")
EducationUnique <- unique(datasetClean$Education)
EducationUnique <- as.tibble(EducationUnique) %>%
  filter(!grepl("select one", value)) #This is a description line that we don't need
EducationUnique$Level <- c(3,5,4,2,1,6)
EducationUnique <- EducationUnique %>%
  arrange(Level)

datasetClean <- datasetClean %>%
  filter(Education %in% EducationUnique$value)

datasetClean$Education <- factor(datasetClean$Education, levels = EducationUnique$value)

#Gender clean -----------------------

#Income Level Factor ------------------------
IncomeUnique <- unique(na.omit(datasetClean$Income))
IncomeUnique <- as.tibble(IncomeUnique)
IncomeUnique$Level <- c("3","2","4","6","1","5")
IncomeUnique <- IncomeUnique %>%
  arrange(Level)
datasetClean <- datasetClean %>%
  filter(Income %in% IncomeUnique$value)

datasetClean$Income <- factor(datasetClean$Income, levels = IncomeUnique$value)

#worktime clean -------------------------
datasetClean$Worktime <- as.numeric(datasetClean$Worktime)

#Shared Audience
Shared <- datasetClean %>%
  filter(Sirius == "Yes" & Wharton == "Yes")

#Summary Statistics ---------------------------------
x <-  na.omit(datasetClean) %>%
  group_by(Education) %>%
  summarize(Frequency = n(),
            Mean_Age = mean(Age),
            Median_Age = median(Age),
            Mean_Worktime = mean(Worktime),
            Median_Worktime = median(Worktime))

formattable(x, align = c("l","c","c","c","c"),
            list('Frequency' = color_tile("white", "red")))


x <- na.omit(datasetClean) %>%
  group_by(Gender) %>%
  summarize(Frequency = n(),
            Mean_Age = mean(Age),
            Median_Age = median(Age),
            Mean_Worktime = mean(Worktime),
            Median_Worktime = median(Worktime))

x <- formattable(x, align = c("l","c","c","c","c"),
            list('Frequency' = color_tile("white", "red")))

na.omit(datasetClean) %>%
  group_by(Income) %>%
  summarize(Frequency = n(),
            Mean_Age = mean(Age),
            Median_Age = median(Age),
            Mean_Worktime = mean(Worktime),
            Median_Worktime = median(Worktime))

formattable(x, align = c("l","c","c","c","c"),
            list('Frequency' = color_tile("white", "red")))

#Charts Listeners -----------------------
ggplot(Shared) +
  aes(x = Income, y = Age, fill = Gender) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Education)) +
  theme(strip.text.x = element_text(
    size = 18, face = "bold"),
    text = element_text(size = 18))


ggplot(datasetClean) +
 aes(x = Age, fill = Income) +
 geom_histogram(bins = 30L) +
 scale_fill_hue() +
 theme_minimal() +
 facet_wrap(vars(Gender))

ggplot(datasetClean) +
  aes(x = Age, fill = Income) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(title = "Age by Income") +
  theme_minimal() +
  facet_wrap(vars(Gender))


ggplot(datasetClean) +
 aes(x = Income, y = Age, fill = Gender) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()

f <- datasetClean %>%
  filter(Sirius == "Yes" & Wharton == "Yes")




#Wharton Yes Sirius No
datasetClean %>%
  filter(Sirius == "No" & Wharton == "Yes") %>%
  ggplot() +
  aes(x = Education, y = Age, fill = Income) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Gender))

#wharton No sirius Yes
datasetClean %>%
  filter(Sirius == "Yes" & Wharton == "No") %>%
  ggplot() +
  aes(x = Education, y = Age, fill = Income) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Gender))


#Case Study 2 ----------------------------
library(readxl)
Casestudy2 <- read_excel("E:/Documents/Penn-One-Drive/OneDrive - PennO365/2020-2021/SP/Datamining/Home work 1/data/WomenData_06_16.xlsx")

head(Casestudy2)
Casestudy2 <- Casestudy2 %>%
  rename("Field" = "Field and sex",
         "Number" = "Degrees Awarded") 

Casestudy2 <- Casestudy2 %>%
  mutate('S&E' = Casestudy2$Field == 'Non-S&E')

Casestudy2 <- Casestudy2 %>%
  mutate('S&E' = as.character(Casestudy2$`S&E`))
Casestudy2$`S&E` <- str_replace(Casestudy2$`S&E`, 'FALSE', 'S&E')
Casestudy2$`S&E`<- str_replace(Casestudy2$`S&E`, 'TRUE', 'Non-S&E')
Casestudy2$Degree <- factor(Casestudy2$Degree)
Casestudy2$Year <- factor(Casestudy2$Year)

Casestudy2%>%
  filter(Year == 2015) %>%
  filter(Sex == "Male") %>%
  group_by(`S&E`)%>%
  summarize(Total = sum(Number))

Casestudy2%>%
  filter(Year == 2015) %>%
  filter(Sex == "Male")

Casestudy2%>%
  group_by(Year)%>%
  summarize(Total = sum(Number))

Casestudy2%>%
  group_by(Sex, Field)%>%
  summarize(Total = sum(Number))

x <- Casestudy2 %>%
  pivot_wider(names_from = Sex, values_from = "Number") %>%
  group_by(Field) %>%
  summarize(Male = sum(Male),
            Female = sum(Female))

#Case Study 2 [3.2] --------------------------
ggplot(Casestudy2%>%
         filter(Year == 2015) %>%
         filter(Sex == "Male")) +
  aes(x = `S&E`, fill = Field, weight = Number) +
  geom_bar() +
  scale_fill_brewer(palette = "RdGy") +
  labs(title = "Non-S&E compared to S&E", subtitle = "Males in sciences related fields in 2015") +
  ggthemes::theme_economist() +
  facet_wrap(vars(Degree))

ggplot(Casestudy2%>%
         filter(Year == 2015)) +
  aes(x = `S&E`, fill = Sex, weight = Number) +
  geom_bar() +
  scale_fill_brewer(palette = "RdGy") +
  labs(title = "Non-S&E compared to S&E", subtitle = "Males in sciences related fields in 2015") +
  ggthemes::theme_economist() +
  facet_wrap(vars(Degree))

ggplot(Casestudy2%>%
         filter(Year == 2015)) +
  aes(x = `S&E`, fill = Degree, weight = Number) +
  geom_bar() +
  scale_fill_brewer(palette = "RdGy") +
  labs(title = "Non-S&E compared to S&E", subtitle = "Males in sciences related fields in 2015") +
  ggthemes::theme_economist() +
  facet_wrap(vars(Sex))

#Non-S&E
Casestudy2 %>%
  filter(Sex == 'Male') %>%
  group_by(`S&E`) %>%
  summarize(Frequency = sum(Number))

Casestudy2 %>%
  filter(Sex == 'Male') %>%
  filter(!Field == 'Non-S&E') %>%
  group_by(Sex) %>%
  summarize('Total S&E' = sum(Number))
  
#Case Study 2 [3.3] ---------------------------

library(ggplot2)

ggplot(Casestudy2) +
 aes(x = Field, y = Number, fill = Degree) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal() +
 facet_grid(vars(Sex), vars())

  theme_minimal()

#Case Study 3 ---------------------------

Casestudy3long <- read.csv("E:/Documents/Penn-One-Drive/OneDrive - PennO365/2020-2021/SP/Datamining/Home work 1/data/baseball.csv", 
           na.strings =  c(""))
Casestudy3wide <- read.csv("E:/Documents/Penn-One-Drive/OneDrive - PennO365/2020-2021/SP/Datamining/Home work 1/data/MLPayData_Total.csv", 
                         na.strings =  c(""))  
head(Casestudy3wide)
#Case Study 3 4.1 ---------------------------------
Casestudy3 <- Casestudy3wide %>%
  select(Team.name.2014,p2013,p2012) %>%
  mutate(Diff = p2013 - p2012,
         Log_Diff = log(p2013) - log(p2012))

Casestudy3a <- Casestudy3wide %>%
  select(Team.name.2014,X2013.pct,X2012.pct) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year") %>%
  rename(Win_percent = value)

Casestudy3a$Year <- str_remove(Casestudy3a$Year, "X")
Casestudy3a$Year <- str_remove(Casestudy3a$Year, ".pct")

Casestudy3 <- pivot_longer(Casestudy3, cols = starts_with("p"), names_to = "Year")
Casestudy3 <- Casestudy3 %>%
  rename(Payroll = value)

Casestudy3$Year <- str_remove(Casestudy3a$Year, "p")

left_join(Casestudy3,Casestudy3a, by = "Year") %>%
  select(!Team.name.2014.y)

# THE CODE FROM CLASS -----------------------
payroll <- Casestudy3wide %>%   # first create variable: payroll and year
  select(Team.name.2014, p1998:p2014) %>% 
  pivot_longer(cols = starts_with("p"), 
               names_to = "year", 
               names_prefix = "p",
               values_to = "payroll")
payroll[1:3, 1:3] # show a few rows

win_num <- Casestudy3wide %>%  # create variable: win_num and year
  select(Team.name.2014, X1998:X2014) %>% 
  pivot_longer(cols = X1998:X2014,
               names_to = "year", 
               names_prefix = "X",
               values_to = "win_num")
#win_num[1:3, 1:3]

win_pct <- Casestudy3wide %>%  # create variable: win_pct and year
  select(Team.name.2014, X1998.pct:X2014.pct) %>% 
  pivot_longer(cols = X1998.pct:X2014.pct,
               names_to = "year",
               names_prefix = "X", 
               values_to = "win_pct") %>%
  mutate(year = substr(year, 1, 4))
#win_pct[1:3, 1:3]

# join tables into team, year, payrow, win_num, win_pct
Casestudy3wide_long <- payroll %>% 
  inner_join(win_num, by = c("Team.name.2014", "year")) %>%
  inner_join(win_pct, by = c("Team.name.2014", "year")) 

head(Casestudy3wide_long, 2)  # see first 2 rows
