#R Task
install.packages("readxl")
install.packages("ggplot2")
install.packages("reshape")
library(readxl)
library(ggplot2)
library(reshape)

#following function is for importing the excel sheets
obtain_sheets <- function(filename, valid = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!valid) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

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

mapply(regionhiv_into_frames, frames, names, eight, seventeen, three, five)
mapply(regionhiv_into_frames, frames, names, eight, seventeen, three+6, five+6)
mapply(regionhiv_into_frames, frames, names, eight, seventeen, three+73, five+73)
regionhiv_into_frames(all_sheets_hiv$England, "England", 8, 17, 89, 91)

df<-as.data.frame(t(data_hiv$England))

ggplot(df, aes(x=(`5`))) + stat_ecdf(geom = "step") + labs(x = "diagnoses") + labs(y = "cumulative density") + ggtitle("Cumulative Density")

#analyze against total population
#united kingdom according to the ons.gov.uk through years 2009 to 2018 for more info: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/overviewoftheukpopulation/august2019 
uk_population_years <- c(62260500, 62759500, 63285100, 63705000, 64105700, 64596800, 65110000, 65648100, 66040200, 66435600)
df2<-as.data.frame(t(data_hiv$England))
df2$`5` <- df2$`5`/uk_population_years
barplot(df2$`5`, main="Percentage: Diagnoses in UK population ",
        names.arg=2009:2018,
        ylim=c(0,0.00020),
        xlab="Years",
        ylab="Percentile in UK Population")

# Basic density
den <- ggplot(df, aes(x=(`5`))) + 
  geom_density(color="darkblue", fill="lightblue")+
  labs(x = "diagnoses")+
  ggtitle("Density plot")
# Add mean line
den + geom_vline(aes(xintercept=mean((`5`))), color="blue" ,linetype="dashed", size=1)

df<-as.data.frame(t(data_hiv$England))
ggplot(data=df, aes(x=2009:2018, y=(`5`), group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()+  
  geom_line(aes(y=(max(`5`))), color = "red", show.legend = TRUE) +
  geom_line(aes(y=(min(`5`))), color = "blue", show.legend = TRUE) +
  geom_line(aes(y=(mean(`5`))), color = "purple", show.legend = TRUE) +
  annotate("text", label = "Maximum Diagnoses", x = 2017, y = 6000, color = "red") +
  annotate("text", label = "Minimum Diagnoses", x = 2017, y = 3950, color = "blue") +
  annotate("text", label = "Mean", x = 2017, y = 5250, color = "purple") +
  labs(x = "Years from 2009 to 2018")+
  labs(y = "HIV diagnoses")+
  ggtitle("HIV Diagnoses from 2009 to 2018")

##################################################################################################################
##################################################################################################################
##################################################################################################################

df_death<-as.data.frame(t(data_death$England))
df_treatment <- as.data.frame(t(data_treatment$England)) 
df_hiv <- as.data.frame(t(data_hiv$England)) 

hiv_mf = ggplot() + 
  geom_line(data = df_hiv, aes(x = 2009:2018, y = (`3`)), color = "blue") +
  geom_line(data = df_hiv, aes(x = 2009:2018, y = (`4`)), color = "red")+
  geom_ribbon(data = df_hiv, aes(x=2009:2018, ymax=(`3`), ymin=(`4`)), fill="plum1", alpha=.5) +
  xlab('Years') +
  ylab('Diagnoses')+
  ggtitle("HIV Diagnoses in males and females")+
  annotate("text", label = "Male Diagnoses", x = 2017, y = 3800, color = "blue") +
  annotate("text", label = "Female Diagnoses", x = 2017, y = 1000, color = "red") 
hiv_mf

death_mf = ggplot() + 
  geom_line(data = df_death, aes(x = 2009:2018, y = (`9`)), color = "blue") +
  geom_line(data = df_death, aes(x = 2009:2018, y = (`10`)), color = "red")+
  geom_ribbon(data = df_death, aes(x=2009:2018, ymax=(`9`), ymin=(`10`)), fill="plum1", alpha=.5) +
  xlab('Years') +
  ylab('Death')+
  ggtitle("HIV Death in males and females")+
  annotate("text", label = "Male Death", x = 2017, y = 210, color = "blue") +
  annotate("text", label = "Female Death", x = 2017, y = 10, color = "red") 
death_mf

treatment_mf = ggplot() + 
  geom_line(data = df_treatment, aes(x = 2009:2018, y = (`89`)), color = "blue") +
  geom_line(data = df_treatment, aes(x = 2009:2018, y = (`90`)), color = "red")+
  geom_ribbon(data = df_treatment, aes(x=2009:2018, ymax=(`89`), ymin=(`90`)), fill="plum1", alpha=.5) +
  xlab('Years') +
  ylab('Treatment')+
  ggtitle("HIV Treatment in males and females")+
  annotate("text", label = "Male Treatment", x = 2017, y = 61000, color = "blue") +
  annotate("text", label = "Female Treatment", x = 2017, y = 25000, color = "red") 
treatment_mf

