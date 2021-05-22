# banner("Part 1" , "Data Cleaning ", emph = T)

############################################################################
############################################################################
###                                                                      ###
###                                PART 1                                ###
###                            DATA CLEANING                             ###
###                                                                      ###
############################################################################
############################################################################

# Install all packages

install.packages("readr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("stringr")
install.packages("data.table")
install.packages("psych")
install.packages("bannerCommenter")
install.packages("viridis")

# Load all libraries to use

library(bannerCommenter)
library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(data.table)
library(psych)
library(gmodels)
library(viridis)

#set working directory

#setwd("C:/Users/Hp/Desktop/DMV")

# Read data and store in a data frame

df = read_csv("CrimeDataSet.csv")
dim(df)

# count number of NA across all columns in the dataframe
apply(is.na(df),2,sum)

str(df)

# Date which was in char is converted to Date by removing time stamp (12:00:00 using Lubridate library)
df$`Date Rptd`= as.Date(mdy_hms(df$`Date Rptd`))
View(df)
df$`DATE OCC`=as.Date(mdy_hms(df$`DATE OCC`))

# Time format modified accordingly for better readibility
#df$`TIME OCC`=gsub(x= df$`TIME OCC`,pattern= "2020-12-13",replacement = "",fixed=T)


#replacing col names for easy accessibility
names(df)[13]="Sex"
names(df)[12]="Age"
names(df)[4]="Time" 

df$Time = format(strptime(df$Time,format = "%H%M"),format = "%H:%M")

# structure of new df
str(df)

#replace all 0 age values with NA
df$Age[df$Age == 0]= NA
df$Age[df$Age > 80 ]= NA
df$Age[df$Age < 10 ]= NA


CrossTable(df$Age)


# replace age is NA with mean(age) for M and F

df$Age[df$Sex=="M" & is.na(df$Age)]=round(mean(df$Age[df$Sex=="M"],na.rm = T))
df$Age[df$Sex=="F" & is.na(df$Age)]=round(mean(df$Age[df$Sex=="F"],na.rm = T))

table(df$Sex)

# Translaste all instances for Sex other than F and M to NA
df$Sex[df$Sex == "H"]= NA
df$Sex[df$Sex == "N"]= NA
df$Sex[df$Sex == "X"]= NA

#replace all NA val in sex with -

df$Sex[is.na(df$Sex)]="-"

#REMOVE UNWANTED COLUMNS

df=df[-c(8)]
df=df[-c(7,10,14,16,20,21,22,23,24,25,26,27)]
df=df[-c(13)]

#Rename column names for readibiltiy
names(df)[2]="Date_rptd"
names(df)[3]="Date_occ"
names(df)[6]="Area_name"
names(df)[7]="Crime_code"
names(df)[8]="Crime_code_desc"
names(df)[11]="Victim_descent"
names(df)[12]="Premise_Description"
names(df)[14]="Status_desc"
names(df)[5]="Area_code"

#DR_No and Area code data type changed to int

df$DR_NO=as.numeric(df$DR_NO)
df$AREA=as.numeric(df$AREA)


#All NA values are replaced with '-' 
df$Age[is.na(df$Age)]="-"
df$Victim_descent[is.na(df$Victim_descent)]="-"
df$Sex[is.na(df$Sex)]="-"
df$Premise_Description[is.na(df$Premise_Description)]="-"
df$Status[is.na(df$Status)]="-"
df$Sex[is.na(df$Sex)]="-"

str(df)

apply(is.na(df),2,sum)

dim(df)

# Column added to calculate delay of case reporting
df= df%>%
  mutate(delay=Date_rptd-Date_occ)

#Delay is stored as char. This is changed to Int
df$delay=as.numeric(df$delay)
describe(df$delay)

#df$Time = sprintf("%04d",as.numeric(df$Time))

df$Age=as.numeric(df$Age)

####################################################################################################################

#banner("Part 2" , "Data Exploration ", emph = T)

###########################################################################
###########################################################################
###                                                                     ###
###                                PART 2                               ###
###                          DATA EXPLORATION                           ###
###                                                                     ###
###########################################################################
###########################################################################



CrossTable(df$Area_code)

#Calculate Crime cases for each area
Crime_by_area= df%>%
  select(Area_code,Area_name)%>%
  group_by(Area_code,Area_name)%>%
  tally()%>%
  arrange(desc(n))
names(Crime_by_area)[3]="count_crime"


head(Crime_by_area)


#Calculate Crime cases for each crime type
Crime_by_type= df%>%
  select(Crime_code,Crime_code_desc)%>%
  group_by(Crime_code,Crime_code_desc)%>%
  tally()%>%
  arrange(desc(n))
names(Crime_by_type)[3]="count_crime"

head(Crime_by_type)

#Plot for crime based on area
ggplot(Crime_by_area,aes(x=reorder(Area_code,-count_crime),y=count_crime))+geom_bar(stat = "identity",color = "blue")+
  ggtitle("Total Crime by the Area code")+
  labs(y="Area Code",x = "Total crime")+
  theme(axis.title.x = element_text(colour = "red"),axis.title.y = element_text(colour = "red"))


CrossTable(df$Age)
#group ages in groups
df = df %>% mutate(age_category = if_else(Age>60,"Elderly",ifelse(Age>18,"Adult","Juvenile")))
CrossTable(df$age_category)

CrossTable(df$Victim_descent)

#Plot graph for  victim descents
Des= as.data.frame(table(df$Victim_descent))
names(Des)[1]="Victim_des"
names(Des)[2]="Count_cases"
ggplot(Des,aes(x=Victim_des,y=Count_cases))+geom_point(size = 2)+
  scale_y_continuous(label=comma)+
  labs(x="Victim Descent", y = "Total crime cases")

CrossTable(df$Sex)

str(df)
################################################################################
#Data written in final file for visualisation
fwrite(df,"FinalCleanedData1.csv")

#################################################################################
