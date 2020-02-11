#ASSIGNMENT DATA ANALYTICS R SCRIPT
#PLEASE BE SURE TO SET AS WORKING DIRECTORY THE SOURCE PATH OF THIS FILE.

#Dependency Packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("reshape")
install.packages("gtools")
install.packages("rmarkdown")
#Calling Libraries
library(readxl)
library(ggplot2)
library(reshape)
library(gtools)

#Obtaining information from each one of the sheets inside the excel file
obtain_sheets <- function(filename, valid = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!valid) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#Creating data frame which contains the key indicators diagnoses, death and treatment
regionhiv_into_frames <- function(region,name,i, j, k,l) {
  print(name)
  y <- region[c(k:l) , c(i:j)]
  y[] <- lapply(y, gsub, pattern='<', replacement='')
  y[] <- lapply(y, as.numeric)
  col_headings <- c(2009:2018)
  names(y) <- col_headings
  if (k==3)  data_hiv[[name]] <<- y 
  if (k==9)  data_death[[name]] <<- y 
  if (k==76)  data_treatment[[name]] <<- y 
  if (k==89)  data_treatment[[name]] <<- y 
}

#Creating new list in order to store information inside of the to create new data frames to analyze our data
data_hiv<- list()
data_death<- list()
data_treatment<- list()
all_sheets_hiv <- suppressMessages(obtain_sheets("Country_Tables_2019_updated.xlsx"))
frames <- list(all_sheets_hiv$England, all_sheets_hiv$Wales, all_sheets_hiv$Northern_Ireland, all_sheets_hiv$Scotland, all_sheets_hiv$London, all_sheets_hiv$Midlands_and_East_of_England, all_sheets_hiv$North_of_England, all_sheets_hiv$South_of_England)
names <- c("England", "Wales", "Northern_Ireland", "Scotland", "London", "Midlands_and_East_of_England", "North_of_England", "South_of_England")
three<-rep(3,8)
five <- rep(5,8)
eight <- rep(8,8)
seventeen<-rep(17,8)

#Using mapply function to call our regionhiv_into_frames function several times with several different parameters to optimize information
mapply(regionhiv_into_frames, frames, names, eight, seventeen, three, five)
mapply(regionhiv_into_frames, frames, names, eight, seventeen, three+6, five+6)
mapply(regionhiv_into_frames, frames, names, eight, seventeen, three+73, five+73)
regionhiv_into_frames(all_sheets_hiv$England, "England", 8, 17, 89, 91)

#Plotting our cumulative density function
dfE<-as.data.frame(t(data_hiv$England))
dfW<-as.data.frame(t(data_hiv$Wales))
dfN<-as.data.frame(t(data_hiv$Northern_Ireland))
dfS<-as.data.frame(t(data_hiv$Scotland))
cbind(dfE,dfW,dfN,dfS)
dfE$`5` <- dfE$`5`+ dfW$`5`+ dfN$`5` + dfS$`5`

ggplot(dfE, aes(x=(`5`))) + stat_ecdf(geom = "step") + labs(x = "diagnoses") + labs(y = "cumulative density") + ggtitle("Cumulative Density")

#United kingdom population according to the ons.gov.uk through years 2009 to 2018 for more info: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/overviewoftheukpopulation/august2019 
uk_population_years <- c(62260500, 62759500, 63285100, 63705000, 64105700, 64596800, 65110000, 65648100, 66040200, 66435600)

#Creating histograms in order to check the percent of diagnoses in England 
df2<-as.data.frame(t(data_hiv$England))
df2$`5` <- dfE$`5`/uk_population_years
barplot(df2$`5`, main="Percentage: Diagnoses in UK population ",
        names.arg=2009:2018,
        ylim=c(0,0.00020),
        xlab="Years",
        ylab="Percentile in UK Population")

colnames(df2)<-c("1","2", "HIV Diagnoses/UK Population")
df2$`1` <- NULL
df2$`2` <- NULL
View(t(df2))
df<-dfE
# Density Plot
den <- ggplot(df, aes(x=(`5`))) + 
  geom_density(color="darkblue", fill="lightblue")+
  labs(x = "diagnoses")+
  ggtitle("Density plot")

# Add mean line
den + geom_vline(aes(xintercept=mean((`5`))), color="blue" ,linetype="dashed", size=1)

#Plotting our line total diagnoses 
df<-dfE
max(df$`5`)
ggplot(data=df, aes(x=2009:2018, y=(`5`), group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()+  
  geom_line(aes(y=(max(`5`))), color = "red", show.legend = TRUE) +
  geom_line(aes(y=(min(`5`))), color = "blue", show.legend = TRUE) +
  geom_line(aes(y=(mean(`5`))), color = "purple", show.legend = TRUE) +
  annotate("text", label = "Maximum Diagnoses", x = 2017, y = 6500, color = "red") +
  annotate("text", label = "Minimum Diagnoses", x = 2017, y = 4300, color = "blue") +
  annotate("text", label = "Mean", x = 2017, y = 5750, color = "purple") +
  labs(x = "Years from 2009 to 2018")+
  labs(y = "HIV diagnoses")+
  ggtitle("HIV Diagnoses from 2009 to 2018")
max(df$`5`)

##################################################################################################################

#Creating new data frames for each one of the key indicators but in separated ones to manipulate them separately
df_treatment <- as.data.frame(t(data_treatment$England)) 
df_hiv <- as.data.frame(t(data_hiv$England)) 
df_death <- as.data.frame(t(data_death$England)) 

df_deathE<-as.data.frame(t(data_death$England))
df_deathW<-as.data.frame(t(data_death$Wales))
df_deathN<-as.data.frame(t(data_death$Northern_Ireland))
df_deathS<-as.data.frame(t(data_death$Scotland))
df_death$`9` <- df_deathE$`9` + df_deathW$`9` + df_deathN$`9` + df_deathS$`9`
df_death$`10` <- df_deathE$`10` + df_deathW$`10` + df_deathN$`10` + df_deathS$`10`
df_death$`11` <- df_deathE$`11` + df_deathW$`11` + df_deathN$`11` + df_deathS$`11`

df_treatmentE<-as.data.frame(t(data_treatment$England))
df_treatmentW<-as.data.frame(t(data_treatment$Wales))
df_treatmentN<-as.data.frame(t(data_treatment$Northern_Ireland))
df_treatmentS<-as.data.frame(t(data_treatment$Scotland))
df_treatment$`89` <- df_treatmentE$`89` + df_treatmentW$`76` + df_treatmentN$`77` + df_treatmentS$`78`
df_treatment$`90` <- df_treatmentE$`90` + df_treatmentW$`76` + df_treatmentN$`77` + df_treatmentS$`78`
df_treatment$`91` <- df_treatmentE$`91` + df_treatmentW$`76` + df_treatmentN$`77` + df_treatmentS$`78`

df_hivE<-as.data.frame(t(data_hiv$England))
df_hivW<-as.data.frame(t(data_hiv$Wales))
df_hivN<-as.data.frame(t(data_hiv$Northern_Ireland))
df_hivS<-as.data.frame(t(data_hiv$Scotland))
df_hiv$`3` <- df_hivE$`3` + df_hivW$`3` + df_hivN$`3` + df_hivS$`3`
df_hiv$`4` <- df_hivE$`4` + df_hivW$`4` + df_hivN$`4` + df_hivS$`4`
df_hiv$`5` <- df_hivE$`5` + df_hivW$`5` + df_hivN$`5` + df_hivS$`5`

#Plotting HIV Diagnoses in Males and Females
df_hiv$`3`
hiv_mf = ggplot() + 
  geom_line(data = df_hiv, aes(x = 2009:2018, y = (`3`)), color = "blue") +
  geom_line(data = df_hiv, aes(x = 2009:2018, y = (`4`)), color = "red")+
  geom_ribbon(data = df_hiv, aes(x=2009:2018, ymax=(`3`), ymin=(`4`)), fill="plum1", alpha=.5) +
  xlab('Years') +
  ylab('Diagnoses')+
  ggtitle("HIV Diagnoses in males and females")+
  annotate("text", label = "Male Diagnoses", x = 2017, y = 4100, color = "blue") +
  annotate("text", label = "Female Diagnoses", x = 2017, y = 1000, color = "red") 
hiv_mf

#Plotting HIV Death in Males and Females
death_mf = ggplot() + 
  geom_line(data = df_death, aes(x = 2009:2018, y = (`9`)), color = "blue") +
  geom_line(data = df_death, aes(x = 2009:2018, y = (`10`)), color = "red")+
  geom_ribbon(data = df_death, aes(x=2009:2018, ymax=(`9`), ymin=(`10`)), fill="plum1", alpha=.5) +
  xlab('Years') +
  ylab('Death')+
  ggtitle("HIV Death in males and females")+
  annotate("text", label = "Male Death", x = 2017, y = 260, color = "blue") +
  annotate("text", label = "Female Death", x = 2017, y = 50, color = "red") 
death_mf

#Plotting HIV Treatment in Males and Females
treatment_mf = ggplot() + 
  geom_line(data = df_treatment, aes(x = 2009:2018, y = (`89`)), color = "blue") +
  geom_line(data = df_treatment, aes(x = 2009:2018, y = (`90`)), color = "red")+
  geom_ribbon(data = df_treatment, aes(x=2009:2018, ymax=(`89`), ymin=(`90`)), fill="plum1", alpha=.5) +
  xlab('Years') +
  ylab('Treatment')+
  ggtitle("HIV Treatment in males and females")+
  annotate("text", label = "Male Treatment", x = 2017, y = 62000, color = "blue") +
  annotate("text", label = "Female Treatment", x = 2017, y = 30000, color = "red") 
treatment_mf
##################################################################################################################

#Creating vectors for years and cities
years <- c(2009:2018)
cities_names <- c("Wales", "Northern_Ireland", "Scotland", "London", "Midlands_and_East_of_England", "North_of_England", "South_of_England")

#Since we need to distribute every data frame in a transpose we need to assign new data frames to new variables so we can store them later in a list for manipulation.
df_wales_hiv<-as.data.frame(t(data_hiv$Wales))
df_northern_hiv<-as.data.frame(t(data_hiv$Northern_Ireland))
df_scotland_hiv<-as.data.frame(t(data_hiv$Scotland))
df_london_hiv<-as.data.frame(t(data_hiv$London))
df_midlands_hiv<-as.data.frame(t(data_hiv$Midlands_and_East_of_England))
df_north_hiv<-as.data.frame(t(data_hiv$North_of_England))
df_south_hiv<-as.data.frame(t(data_hiv$South_of_England))

df_wales_death<-as.data.frame(t(data_death$Wales))
df_northern_death<-as.data.frame(t(data_death$Northern_Ireland))
df_scotland_death<-as.data.frame(t(data_death$Scotland))
df_london_death<-as.data.frame(t(data_death$London))
df_midlands_death<-as.data.frame(t(data_death$Midlands_and_East_of_England))
df_north_death<-as.data.frame(t(data_death$North_of_England))
df_south_death<-as.data.frame(t(data_death$South_of_England))

df_wales_treatment<-as.data.frame(t(data_treatment$Wales))
df_northern_treatment<-as.data.frame(t(data_treatment$Northern_Ireland))
df_scotland_treatment<-as.data.frame(t(data_treatment$Scotland))
df_london_treatment<-as.data.frame(t(data_treatment$London))
df_midlands_treatment<-as.data.frame(t(data_treatment$Midlands_and_East_of_England))
df_north_treatment<-as.data.frame(t(data_treatment$North_of_England))
df_south_treatment<-as.data.frame(t(data_treatment$South_of_England))

#Manipulating data frames in a new list for each one of the key indicators
hiv_frames <- list(df_wales_hiv, df_northern_hiv, df_scotland_hiv, df_london_hiv, df_midlands_hiv, df_north_hiv, df_south_hiv)
hiv_frames_death <- list(df_wales_death, df_northern_death, df_scotland_death, df_london_death, df_midlands_death, df_north_death, df_south_death)
hiv_frames_treatment <- list(df_wales_treatment, df_northern_treatment, df_scotland_treatment, df_london_treatment, df_midlands_treatment, df_north_treatment, df_south_treatment)

#Merging data frames diagnoses by their rows
merged.data.frame = Reduce(function(...) merge(..., all=T), hiv_frames)
single_hiv_frame <- do.call("rbind", hiv_frames)

#Merging data frames death by their rows
merged.data.frame = Reduce(function(...) merge(..., all=T), hiv_frames_death)
single_death_frame <- do.call("rbind", hiv_frames_death)

#Merging data frames treatment by their rows
mapped_frames_treatment<-Map(cbind, hiv_frames_treatment, cities = cities_names)
merged.data.frame = Reduce(function(...) merge(..., all=T), mapped_frames_treatment)
single_treatment_frame <- do.call("rbind", mapped_frames_treatment)

#merging data frames by their columns all key indicators in one
list_hiv_frames <- list(single_hiv_frame, single_death_frame, single_treatment_frame)
hiv_all_frame <- do.call("cbind", list_hiv_frames)

#adding new column which includes the years
vec_years <- rep(2009:2018, 7)
hiv_all_frame$years <- vec_years

#assigning names to columns according to the variable
colnames(hiv_all_frame) <- c("m_diagnose", "f_diagnose", "t_diagnose", "m_death", "f_death", "t_death", "m_treatment", "f_treatment", "t_treatment", "cities", "years")

#Plotting every city key indicator in order to compare against each other
plot_hiv <- ggplot(data=hiv_all_frame)
ggplot(hiv_all_frame, aes(x =years, y = t_diagnose, color = cities, group = cities)) + 
  geom_point() + geom_line()  + ggtitle("HIV Diagnoses v. Years in Cities")
ggplot(hiv_all_frame, aes(x =years, y = t_treatment, color = cities, group = cities)) + 
  geom_point() + geom_line()  + ggtitle("HIV Treatments v. Years in Cities")
ggplot(hiv_all_frame, aes(x =years, y = t_death, color = cities, group = cities)) + 
  geom_point() + geom_line()  + ggtitle("HIV Death v. Years in Cities")

#plotting points to find any important differences
plot_hiv + geom_point(aes(x=t_diagnose, y=t_death, color=cities))  + ggtitle("HIV Death v. Diagnoses in Cities") #important there are more deaths in bigger cities
plot_hiv + geom_point(aes(x=t_treatment, y=t_death, color=cities))  + ggtitle("HIV Death v. Treatment in Cities") #important there are more deaths in bigger cities
plot_hiv + geom_point(aes(x=t_diagnose, y=t_treatment, color=cities))  + ggtitle("HIV Treatment v. Diagnoses in Cities")

plot_hiv + geom_point(aes(x=m_death, y=f_death, color=cities))  + ggtitle("HIV Female Death v. Male Death in Cities")
plot_hiv + geom_point(aes(x=m_diagnose, y=f_diagnose, color=cities))  + ggtitle("HIV Female Diagnose v. Male Diagnose in Cities")
plot_hiv + geom_point(aes(x=m_treatment, y=f_treatment, color=cities))  + ggtitle("HIV Female Treatment v. Male Treatment in Cities")

#Viewing summaries in a different window for better interpretation
View(summary(hiv_all_frame))
View(hiv_all_frame)

#Standard Deviation calculations for each one of the indicator and each one of the cities.
sd_indicators <- c(sd(hiv_all_frame$t_diagnose),sd(hiv_all_frame$t_death),sd(hiv_all_frame$t_treatment))

sd_diagnose <- c(sd(hiv_all_frame[hiv_all_frame$cities == "Wales",  ]$t_diagnose),sd(hiv_all_frame[hiv_all_frame$cities == "Northern_Ireland",  ]$t_diagnose),sd(hiv_all_frame[hiv_all_frame$cities == "Scotland",  ]$t_diagnose),sd(hiv_all_frame[hiv_all_frame$cities == "London",  ]$t_diagnose),
                       sd(hiv_all_frame[hiv_all_frame$cities == "Midlands_and_East_of_England",  ]$t_diagnose),sd(hiv_all_frame[hiv_all_frame$cities == "South_of_England",  ]$t_diagnose),sd(hiv_all_frame[hiv_all_frame$cities == "North_of_England",  ]$t_diagnose))

sd_death <- c(sd(hiv_all_frame[hiv_all_frame$cities == "Wales",  ]$t_death),sd(hiv_all_frame[hiv_all_frame$cities == "Northern_Ireland",  ]$t_death),sd(hiv_all_frame[hiv_all_frame$cities == "Scotland",  ]$t_death),sd(hiv_all_frame[hiv_all_frame$cities == "London",  ]$t_death),
              sd(hiv_all_frame[hiv_all_frame$cities == "Midlands_and_East_of_England",  ]$t_death),sd(hiv_all_frame[hiv_all_frame$cities == "South_of_England",  ]$t_death),sd(hiv_all_frame[hiv_all_frame$cities == "North_of_England",  ]$t_death))
#important death standard deviation is quite similar

sd_treatment <- c(sd(hiv_all_frame[hiv_all_frame$cities == "Wales",  ]$t_treatment),sd(hiv_all_frame[hiv_all_frame$cities == "Northern_Ireland",  ]$t_treatment),sd(hiv_all_frame[hiv_all_frame$cities == "Scotland",  ]$t_treatment),sd(hiv_all_frame[hiv_all_frame$cities == "London",  ]$t_treatment),
                  sd(hiv_all_frame[hiv_all_frame$cities == "Midlands_and_East_of_England",  ]$t_treatment),sd(hiv_all_frame[hiv_all_frame$cities == "South_of_England",  ]$t_treatment),sd(hiv_all_frame[hiv_all_frame$cities == "North_of_England",  ]$t_treatment))
 

##################################################################################################################

#Important question: Is propagation of HIV the same in bigger cities and small cities. Suggest Clustering 3 cities and 4 cities. Or isolate London.
#United Kingdom important cities and regions populations, information obtained from the following links:
#https://ec.europa.eu/eurostat/en/web/population-demography-migration-projections/statistics-illustrated
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates
#https://www.google.com/search?sxsrf=ACYBGNQIVzpsB6VZnK59KAx-OKljDsTicQ%3A1573411267035&ei=w1nIXabYAZeT8gLjzpSIAQ&q=london+population+2008&oq=london+population+2008&gs_l=psy-ab.3..0i203.7838.12347..12442...1.1..0.76.1533.23......0....1..gws-wiz.......0i71j35i39i19j35i39j0i67j0j0i22i30.fI4KlV4XSXE&ved=0ahUKEwjmxaDdpeDlAhWXiVwKHWMnBREQ4dUDCAs&uact=5
wales_population_years <- c(3038900, 3050000, 3063800, 3074100, 3082400, 3092000, 3099100, 3113200, 3125200, 3138600)
northernireland_population_years <- c(1793300, 1804800, 1814300, 1823600, 1829700, 1840500, 1851600, 1862100, 1870800, 1881600)
scotland_population_years <- c(5231900, 5262200, 5299900, 5313600, 5327700, 5347600, 5373000, 5404700, 5424800, 5438100)
london_population_years <- c(7878000, 8003000, 8134000, 8257000, 8363000, 8478000, 8603000, 8718000, 8797000, 8900000)
midlands_population_years <- c(4472000, 4507000, 4533000, 4568000, 4598000, 4637000, 4677000, 4724000, 4772000, 4804000)
southofengland_population_years <- c(2575000, 2587000, 2597000, 2602000, 2610000, 2619000, 2625000, 2637000, 2645000, 2658000)
northofengland_population_years <- c(6986000+5227000, 7020000+5261000, 7052000+5289000, 7084000+5340000, 7103000+5378000, 7133000+5423000, 7174000+5471000, 7220000+5516000, 7259000+5559000, 7292000 + 5600000)

#Would be worth obtaining the rate of diagnoses in each city by dividing city Diagnoses/city population and obtaining the mean for the 10 years we are analyzing so we can get a rate of the diagnoses in each city.
rate_diagnoses <- c(mean(hiv_all_frame$t_diagnose[hiv_all_frame$cities == "Wales"]/wales_population_years),mean(hiv_all_frame$t_diagnose[hiv_all_frame$cities == "Northern_Ireland"]/northernireland_population_years), mean(hiv_all_frame$t_diagnose[hiv_all_frame$cities == "Scotland"]/scotland_population_years),mean(hiv_all_frame$t_diagnose[hiv_all_frame$cities == "London"]/london_population_years),
                    mean(hiv_all_frame$t_diagnose[hiv_all_frame$cities == "Midlands_and_East_of_England"]/midlands_population_years),mean(hiv_all_frame$t_diagnose[hiv_all_frame$cities == "North_of_England"]/southofengland_population_years),mean(hiv_all_frame$t_diagnose[hiv_all_frame$cities == "South_of_England"]/northofengland_population_years))

#Printing in console standard deviation results
cat("Standard Deviation Key Indicators (Diagnoses,Death,Treatment): ", sd_indicators)
cat("Standard Deviation Diagnoses (Cities: W,NI,S,L,MEE,SE,NE): ", sd_diagnose)
cat("Standard Deviation Death (Cities: W,NI,S,L,MEE,SE,NE): ", sd_death)
cat("Standard Deviation Treatment (Cities: W,NI,S,L,MEE,SE,NE): ", sd_treatment)
cat("Diagnoses Rate (Diagnoses/Population) from 2009 to 2018  (Cities: W,NI,S,L,MEE,SE,NE): ", rate_diagnoses)


