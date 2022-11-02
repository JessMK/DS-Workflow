#install.packages("tidycensus")
library(tidycensus)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

## API Key:

census_api_key("YOUR KEY GOES HERE",
               overwrite = FALSE, install = TRUE)

tidyverse_logo()

## Research question: How does the Annual Survey of Jails sampled
## data compare to the Correctional data published by Census?

## Part 1: Getting the data--------------------------

## we want to get information from Census, display available variables

## 2020 variables

vars20 <- load_variables(2020, "pl")

print(vars20, n = 301)

View(vars20)


## 2010 variables

vars10 <- load_variables(2010, "sf1")

print(vars10, n = 9099)

View(vars10)


## we need decennial data from 2020

pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020) %>%
  mutate(year = 2020, variable = "population")


## view decennial data

pop20


## decennial data 2010

pop10 <- get_decennial(
  geography = "state",
  variables = "H001001",
  year = 2010) %>%
  mutate(year = 2010, variable = "population")


## view decennial data

pop10


## get institutional and correctional data from Census

Inst20 <- get_decennial(
  geography = "state",
  variables = "P5_002N",
  year = 2020) %>%
  mutate(year = 2020, variable = "institutional")


Corr20 <- get_decennial(
  geography = "state",
  variables = "P5_003N",
  year = 2020) %>%
  mutate(year = 2020, variable = "correctional")


Inst10 <- get_decennial(
  geography = "state",
  variables = "P029027",
  year = 2010) %>%
  mutate(year = 2010, variable = "institutional")


Corr10 <- get_decennial(
  geography = "state",
  variables = "P042003",
  year = 2010) %>%
  mutate(year = 2010, variable = "correctional")

glimpse(Inst20)

glimpse(Corr20)

glimpse(Inst10)

glimpse(Corr10)

## ACS Vars 2015, does not have institutionalized or correctional data

all_vars_acs5 <-
  load_variables(year = 2015, dataset = "acs5")

View(all_vars_acs5)

## get population data for 2015

acs15 <- get_acs(
  geography = "state",
  variables = "B00001_001",
  year = 2015) %>%
  rename(value = "estimate") %>%
  mutate(year = 2015, variable = "population")

head(acs15)


## combine the data, one data frame at a time

Pop <- rbind(pop20, pop10)

Corr <- rbind(Corr20, Corr10)

Inst <- rbind(Inst20, Inst10)

Census <- rbind(Pop, Corr)

Census <- rbind(Census, Inst)

Census <- rbind(Census, acs15) %>%
  select(GEOID, NAME, value, year, variable)

str(Census)


## load Annual Survey of Jails Data

load("C:/Users/klein337/Desktop/
     Data Science Workflow/38408-0001-Data.rda")

ASJ20 <- da38408.0001

load("C:/Users/klein337/Desktop/
     Data Science Workflow/36760-0001-Data.rda")

ASJ15 <- da36760.0001

ASJ10 <- read_excel("C:/Users/klein337/Desktop/
                    Data Science Workflow/31261-0001-Data.xlsx")


## take a look at the data

str(ASJ20)

str(ASJ15)

str(ASJ10)


## keep important variables

ASJ20 <- ASJ20 %>%
  select(STATE, PEAKPOP)

ASJ15 <- ASJ15 %>%
  select(STATE, PEAKPOP)

ASJ10 <- ASJ10 %>%
  select(state, peakpop)


## small question- how many facilities appear in each state?

ASJ20State <- as.data.frame(table(ASJ20$STATE)) %>%
  rename(State = "Var1", Fac20 = "Freq")

ASJ15State <- as.data.frame(table(ASJ15$STATE)) %>%
  rename(Var15 = "Var1", Fac15 = "Freq")

ASJ10State <- as.data.frame(table(ASJ10$state)) %>%
  rename(Var10 = "Var1", Fac10 = "Freq")

ASJStates <- cbind(ASJ20State, ASJ15State) %>%
  select(-c(Var15)) %>%
  slice(-2)

ASJStates <- cbind(ASJStates, ASJ10State) %>%
  select(-c(Var10))

View(ASJStates)


## note: notice not every state is in there; CT/VT/HI/RI/DE, dropped AK


## Part 2: Transforming the data-----------------------------

## to keep diving into our research question,
## we want to join ASJ data with Census data.
## some transforming, cleaning and joining is required

## look at population in Annual Survey of Jails,
## peak population during reference month

## peak population in June 2020, from ASJ

ASJPeakpop20 <- ASJ20 %>%
  group_by(STATE) %>%
  summarise(TotalPeakpop = sum(PEAKPOP)) %>%
  rename(value = TotalPeakpop) %>%
  rename(NAME = STATE) %>%
  mutate(year = 2020, variable = "peak population")


## peak population in December 2015, from ASJ

ASJPeakpop15 <- ASJ15 %>%
  group_by(STATE) %>%
  summarise(PeakPop = sum(PEAKPOP, na.rm = TRUE)) %>%
  rename(value = PeakPop) %>%
  rename(NAME = STATE) %>%
  mutate(year = 2015, variable = "peak population")


## peak population in June 2010, from ASJ

ASJPeakpop10 <- ASJ10 %>%
  group_by(state) %>%
  summarise(PeakPop = sum(peakpop, na.rm = TRUE)) %>%
  rename(value = PeakPop) %>%
  rename(NAME = state) %>%
  mutate(year = 2010, variable = "peak population")

glimpse(ASJPeakpop20)

glimpse(ASJPeakpop15)

glimpse(ASJPeakpop10)


## states need to be consistent

head(ASJPeakpop20$NAME)

head(ASJPeakpop15$NAME)

head(ASJPeakpop10$NAME)

ASJPeakpop10$STATE <- ASJPeakpop15$NAME

ASJPeakpop10 <- ASJPeakpop10 %>%
  select(c(-NAME)) %>%
  rename(NAME = STATE) %>%
  select(NAME, value, year, variable)

str(ASJPeakpop10)


## Combine peak population data, from ASJ

ASJPeakPop <- rbind(ASJPeakpop20, ASJPeakpop15)

ASJPeakPop <- rbind(ASJPeakPop, ASJPeakpop10)


## remove parenthesis

ASJPeakPop$NAME <- gsub("[()]", "", ASJPeakPop$NAME)

ASJPeakPop <- ASJPeakPop %>%
  separate(NAME, c('GEOID', 'NAME'),
           extra = "merge", fill = "left")

glimpse(ASJPeakPop)

glimpse(Census)


## same format, time to join

PopData <- rbind(ASJPeakPop, Census)

glimpse(PopData)


## which states have no correctional data

NOASJ <- as.data.frame(table(PopData$NAME))

NOASJ %>%
  filter(Freq < 10)


## drop rows with no correctional data

PopData <- as.data.frame(PopData) %>%
  filter(!NAME  %in% c('Alaska', 'Connecticut', 'Delaware',
                       'Hawaii', 'Puerto Rico', 'Rhode Island',
                       'Vermont'))

table(PopData$NAME)

table(PopData$year)

table(PopData$variable)


## put pop data to the side for a minute so we can start small
## with our visualization

## get 2020 adult correctional data by state and add percent of
## total column

## count of adults in correctional facilities by state

Corr_population <- get_decennial(
  geography = "state",
  variables = "P5_003N",
  year = 2020,
  output = "wide") %>%
  mutate(percent = 100 * (P5_003N / sum(P5_003N)))

Corr_population %>%
  arrange(desc(percent))

Corr_population %>%
  arrange(percent)


## percent of adults in correctional facilities in each state

Corr_populationState <- get_decennial(
  geography = "state",
  variables = (c("P5_003N","P1_001N")),
  year = 2020,
  output = "wide") %>%
  group_by(NAME) %>%
  mutate(percent = 100 * (P5_003N / P1_001N))

Corr_populationState %>%
  arrange(desc(percent))

Corr_populationState %>%
  arrange(percent)


## if the data is too big, can subset into something more manageable

#CorrTop <- Corr_populationState %>%
#  arrange(desc(percent)) %>%
#  head(10)

#CorrBottom <- Corr_populationState %>%
#  arrange(desc(percent)) %>%
#  tail(10)

#CorrPopSub <- rbind(CorrTop, CorrBottom)

#glimpse(CorrPopSub)


## Part 3: Visualization with ggplot2--------------------------

## histogram of adult correctional population

ggplot(Corr_populationState, aes(x = percent )) +
  geom_histogram(fill ='dark green', col = 'red', bins = 10)


## boxplot of adult correctional population by state size; add state size first

Corr_populationStateSize <- Corr_populationState %>%
  mutate(size_bin = case_when(P1_001N < 1000000 ~ 'small',
                              P1_001N >= 1000000 &
                                P1_001N <= 10000000 ~ 'medium',
                              P1_001N > 10000000 ~ 'large'))

ggplot(Corr_populationStateSize, aes(x = size_bin, y = percent)) +
  geom_boxplot(color = 'blue', outlier.color = 'red')


## bar chart of adult correctional population

ggplot(Corr_populationStateSize, aes(x = reorder(NAME, percent),
                                     y = percent, fill = size_bin)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5, size = 6))+
  coord_flip()+
  labs(title = "Correctional Population by State, 2020",
       x = "State",
       y = "Percent of Adults in Correctional Facilities",
       fill = "Size of State")


## scatterplot of adult correctional population

ggplot(Corr_populationStateSize, aes(x = reorder(NAME, percent),
                                     y = percent, color = size_bin)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5, size = 6))+
  coord_flip()+
  labs(title = "Adults in Correctional Facilities by State, 2020",
       x = "State",
       y = "Percent of Adults in Corrections",
       fill = "Size of State")


## scatterplot of adult correctional population with lm

options(scipen = 999) # Disable scientific notation

ggplot(Corr_populationStateSize, aes(x = P1_001N,
                                     y = P5_003N, color = size_bin)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5, size = 6))+
  labs(title = "Adults in Correctional Facilities by State, 2020",
       x = "Population Count",
       y = "Adults in Corrections",
       fill = "Size of State")


## using ggplot with facets

## add percent, group by variable and year

PopData <- PopData %>%
  group_by(variable, year) %>%
  mutate(percent = 100 * (value/sum(value)))

head(PopData)


## bar chart by category, with all states, combining years

ggplot(PopData, aes(x=NAME, y=percent, fill = as.factor(year)))+
  geom_col()+
  labs(x = "State",
       y = "Percent of Population",
       fill = "Year") +
  theme(axis.text.x = element_text(angle=90,hjust=.2,vjust=0.5, size = 6))+
  facet_wrap(~variable)


## next we want by variable group, including only top states,
## separating years

## first assign a unique rank, take the top 6 states of each
## variable, and keep only those 13 states

PopData <- PopData %>%
  group_by(variable, year) %>%
  mutate(rank = dense_rank(desc(value)))

PopDataTop6 <- PopData %>%
  filter(rank <= 6)

table(PopDataTop6$NAME)

PopDataTop6 <- PopData %>%
  filter(NAME  %in% c('Arizona', 'California',
                      'Florida', 'Georgia', 'Illinois',
                      'Louisiana', 'Michigan', 'New York',
                      'Ohio', 'Pennsylvania', 'Tennessee',
                      'Texas', 'Virginia'))

table(PopDataTop6$NAME)


## time to visualize and see what graph answers our question best

## bar chart by variable group

ggplot(PopDataTop6, aes(x=NAME, y=percent))+
  geom_col()+
  ylab("Percent of Pop")+
  facet_wrap(variable~year) +
  theme(axis.text.x = element_text(angle=90,hjust=.2,vjust=0.5, size = 6))


## stacked bar chart, all variable groups combined

ggplot(PopDataTop6, aes(x=year, y=percent))+
  geom_col(aes(fill = variable))+
  ylab("Percent of Pop")+
  theme(axis.text.x = element_text(angle=90,hjust=.2,vjust=0.5, size = 6))


## grouped bar chart, by year

ggplot(PopDataTop6, aes(x=year, y=percent))+
  geom_bar(aes(fill = variable), stat = "identity", position ="dodge")+
  ylab("Percent of Pop")+
  theme(axis.text.x = element_text(angle=90,hjust=.2,vjust=0.5, size = 6))


## grouped bar chart, by variable group

ggplot(PopDataTop6, aes(x=variable, y=percent))+
  geom_bar(aes(fill = as.factor(year)), stat = "identity", position ="dodge")+
  ylab("Percent of Pop")+
  theme(axis.text.x = element_text(angle=90,hjust=.2,vjust=0.5, size = 6))

