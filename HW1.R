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

x <- x %>%
  mutate(More = x$Male > x$Female)

x$More <- as.character(x$More)
x$More <- str_replace(x$More, 'FALSE', 'Female')
x$More <- str_replace(x$More, 'TRUE','Male')
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

Casestudy2 %>%
  filter(Field %in% c("Computer sciences", "Mathematics and statistics")) %>%
  ggplot() +
  aes(x = Field, y = Number, fill = Sex) +
  geom_boxplot() +
  scale_fill_hue() +
  scale_fill_brewer(palette = "RdGy") +
  coord_flip() +
  ggthemes::theme_economist()

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
PayrollKey <- Casestudy3wide %>%   # first create variable: payroll and year
  select(Team.name.2014, p1998:p2014) %>%
  mutate(`Diff1998-1999` = p1999 - p1998,
         `Diff1999-2000` = p2000 - p1999,
         `Diff2001-2002` = p2001 - p2000,
         `Diff2002-2003` = p2003 - p2002,
         `Diff2003-2004` = p2004 - p2003,
         `Diff2005-2004` = p2005 - p2004,
         `Diff2006-2005` = p2006 - p2005,
         `Diff2007-2006` = p2007 - p2006,
         `Diff2008-2007` = p2008 - p2007,
         `Diff2009-2008` = p2009 - p2008,
         `Diff2010-2009` = p2010 - p2009,
         `Diff2011-2010` = p2011 - p2010,
         `Diff2012-2011` = p2012 - p2011,
         `Diff2013-2012` = p2013 - p2012,
         `Diff2014-2013` = p2014 - p2013)

payrollDiff <- Casestudy3wide %>%   # first create variable: payroll and year
  select(Team.name.2014, p1998:p2014) %>%
  mutate(Diff1998 = p1999 - p1998,
         Diff1999 = p2000 - p1999,
         Diff2001 = p2001 - p2000,
         Diff2002 = p2003 - p2002,
         Diff2003 = p2004 - p2003,
         Diff2005 = p2005 - p2004,
         Diff2006 = p2006 - p2005,
         Diff2007 = p2007 - p2006,
         Diff2008 = p2008 - p2007,
         Diff2009 = p2009 - p2008,
         Diff2010 = p2010 - p2009,
         Diff2011 = p2011 - p2010,
         Diff2012 = p2012 - p2011,
         Diff2013 = p2013 - p2012,
         Diff2014 = p2014 - p2013)


payrollDiff <- payrollDiff %>%
  select(Team.name.2014, Diff1998:Diff2014) %>%
  pivot_longer(cols = starts_with("Diff"),
               names_to = "year",
               names_prefix = "Diff",
               values_to = "Difference")

logpayrollDiff <- Casestudy3wide %>%   # first create variable: payroll and year
  select(Team.name.2014, p1998:p2014) %>%
  mutate(Diff1998 = log(p1999) - log(p1998),
         Diff1999 = log(p2000) - log(p1999),
         Diff2001 = log(p2001) - log(p2000),
         Diff2002 = log(p2003) - log(p2002),
         Diff2003 = log(p2004) - log(p2003),
         Diff2005 = log(p2005) - log(p2004),
         Diff2006 = log(p2006) - log(p2005),
         Diff2007 = log(p2007) - log(p2006),
         Diff2008 = log(p2008) - log(p2007),
         Diff2009 = log(p2009) - log(p2008),
         Diff2010 = log(p2010) - log(p2009),
         Diff2011 = log(p2011) - log(p2010),
         Diff2012 = log(p2012) - log(p2011),
         Diff2013 = log(p2013) - log(p2012),
         Diff2014 = log(p2014) - log(p2013))

logpayrollDiff <- logpayrollDiff %>%
  select(Team.name.2014, Diff1998:Diff2014) %>%
  pivot_longer(cols = starts_with("Diff"),
               names_to = "year",
               names_prefix = "Diff",
               values_to = "Log_Difference")

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
  inner_join(win_pct, by = c("Team.name.2014", "year")) %>%
  inner_join(payrollDiff, by = c("Team.name.2014", "year")) %>%
  inner_join(logpayrollDiff, by = c("Team.name.2014", "year")) %>%
  rename(Payrole_Diff = Difference,
         Log_Payrole_Diff = Log_Difference)

head(Casestudy3wide_long, 2)  # see first 2 rows


#Summary Statistics ---------------------
Casestudy3wide_long%>%
  filter(year == 2010:2014) %>%
  group_by(Team.name.2014) %>%
  summarize(Change_Payroll = sum(Log_Payrole_Diff),
            Wins = sum(win_num),
            Wins_Pct = mean(win_pct)) %>%
  arrange(desc(Change_Payroll))

#PLOTS --------------------------
f <- Casestudy3wide_long %>% 
  ggplot(aes(x = year, y = Log_Payrole_Diff, group = Team.name.2014, col = Team.name.2014)) + 
  geom_line() + 
  geom_point() +
  theme_bw() 

ggplotly(f +
           theme(legend.position = "none"))
