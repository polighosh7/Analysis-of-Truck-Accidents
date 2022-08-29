# load the data
setwd("C:/Users/Poli Ghosh/Documents/Course_Material/Sem 2/Big Data/Project")
library("readxl")
install.packages("data.table")
library(data.table)
install.packages("ggplot2")
library(ggplot2)

# xlsx files
avg_m <- read_excel("Avg_Mileage.xlsx")
dri_m <- read_excel("Drivermileage.xlsx")
geo_r <- read_excel("Geo_Risk.xlsx")
rsk_fct <- read_excel("Risk Factor.xlsx")
trk_m <- read_excel("Truck_mileage.xlsx")

#boxplot
boxplot(avg_m$avgmpg, 
        main = "Average Mileage", 
        ylab = "Avg Mpg", horizontal=FALSE)      # Observed no outliers

boxplot(dri_m$totmiles, 
        main = "Total Miles", 
        ylab = "Total Miles", horizontal=FALSE)      # Observed outliers

boxplot(rsk_fct$riskfactor, 
        main = "Risk Factor", 
        ylab = "Risk Factor", horizontal=FALSE)      # Observed outliers

boxplot(geo_r$velocity, 
        main = "Velocity", 
        ylab = "Velocity", horizontal=FALSE)      # Observed no outliers

table(geo_r$event_ind)    # 0's - 7469, 1's - 451

ggplot(geo_r, aes(geo_r$event_ind), position = "dodge") +  labs(x='Events',y='No. of Count', title='Event distribution' )+ geom_bar( width=0.5, fill='lightblue', color='darkblue')


model<-lm(riskfactor ~ totmiles + events + velocity + idling_ind, data=geo_r)
summary(model)


