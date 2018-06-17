library(plyr)
library(dplyr)
library(doBy)

master_data <- read.csv(
  'C:/Users/Praveen/Documents/MSDS/ISQS5347-AdvStatisticalmethods/stats project/Master_Collision_Data.csv',
  header = TRUE)


sub_data <- master_data[c('Date','Time','Borough','X._of_Injured','X._of_Killed')]

sub_data$Date <- as.Date(sub_data$Date,'%m/%d/%Y')

sub_data$Time <- as.POSIXct((strptime(sub_data$Time,format = '%H:%M')),tz = '')

sub_data <- filter(sub_data,(!sub_data$Borough=='') & strftime(sub_data$Date,'%Y') == '2017' )

sub_data$Borough <- as.character(sub_data$Borough)

sub_data$hour_group <- cut(
  sub_data$Time,breaks = '1 hour',
  labels = paste(0:23,1:24,sep='-'),
  right = FALSE)

sub_data$Borough <- factor(sub_data$Borough)

str(sub_data)


long_data <- sub_data %>% group_by(Borough,hour_group) %>% summarise(count = n())

library(reshape2)
## Contingency Table
cont_table <- dcast(long_data,hour_group~Borough,value.var = 'count')
cont_table <- mutate(cont_table,total = apply(cont_table[2:6],1,sum))
row.names(cont_table) <- cont_table[,1]
cont_table <- cont_table[-1]
cont_table <- rbind(cont_table,'Total'= apply(cont_table,2,sum))

###Extracting 'Number of Injured' and 'Number of Killed'
prob = cbind(ddply(sub_data,'hour_group',function(x){ sum(x$X._of_Killed)}),
             ddply(sub_data,'hour_group',function(x){ sum(x$X._of_Injured)})[2])

prob <- mutate(prob,hour = 1:24)

colnames(prob) <- c('hour_group','Killed_Count','Injured_Count')

##QQ plot

qqnorm(prob$Injured_Count)
qqline(prob$Injured_Count)

# creating function for calculating Skewness of given vector of data
fun_skew <- function(x, mean, sd) {
  prob = 1 / length(x)
  out = c()
  for (i in 1:length(x)) {
    a = (x[i] - mean) / sd
    out = c(out, (a ^ 3) * prob)
  }
  return (sum(out))
}

# creating function for calculating Kurtosis
fun_kurtosis <- function(x, mean, sd) {
  prob = 1 / length(x)
  out = c()
  for (i in 1:length(x)) {
    a = (x[i] - mean) / sd
    out = c(out, ((a ^ 4) * prob))
  }
  return (sum(out) - 3)
}

# calculating Kurtosis and skewness of 
mean = mean(prob$Injured_Count)
sd = sd(prob$Injured_Count)
kurtosis = fun_kurtosis(prob$Injured_Count,mean,sd)
skewness = fun_skew(prob$Injured_Count,mean,sd)  

paste('Skewness of given dataset :',skewness)
paste('kurtosis of given dataset : ',kurtosis)

#Density plot of 'Injured Count'
plot(density(prob$Injured_Count),main = 'Density plot of Injured_Count column',xlab = 'Injured Count')



#Seasonal grouping of data
sn_data <- master_data[c('Date','Time','Borough','X._of_Injured','X._of_Killed')]

sn_data <- filter(sn_data,Borough=='MANHATTAN')

sn_data$Date <- as.Date(sn_data$Date,'%m/%d/%Y')

sn_data$Time <- as.POSIXct((strptime(sn_data$Time,format = '%H:%M')),tz = '')


sn_data <- filter(sn_data,((strftime(sn_data$Date,'%Y') == '2015')|(strftime(sn_data$Date,'%Y') == '2016')) )


sn_data$Borough <- as.character(sn_data$Borough)

sn_data$Borough <- factor(sn_data$Borough)



sn_data <- sn_data %>% group_by(.,Date) %>% tally(.,sort = TRUE)

month <- format(sn_data$Date,'%b')
season <- c()

s = 1

for (s in 1:length(month))
{
  if ((month[s]=='Jan') || (month[s]=='Dec')  || (month[s]=='Feb'))  {   season[s]='Winter' }
  if (month[s]=='Sep' || month[s]=='Oct'  || month[s]=='Nov')  {   season[s]='Fall'}
  if (month[s]=='Mar' || month[s]=='Apr'  || month[s]=='May')  {     season[s]='Spring'}
  if (month[s]=='Jun' || month[s]=='Jul'  || month[s]=='Aug')  {     season[s]='Summer'}
}

sn_data <- cbind(season,sn_data)

sn_ss<-filter(sn_data,season==c("Summer","Spring"))
sn_fw<-filter(sn_data,season==c("Fall","Winter"))

var.test(sn_ss$n,sn_fw$n,conf.level = 0.9)

#F0 < Falpha/2 i.e. F0<0.1
#Null hypothesis rejected and both populations dont have same variance
#Now applying two sample t-test based on the result of F-test which being no evidence of same variance

t.test(sn_fw$n,sn_ss$n,var.equal = FALSE,alternative="less",conf.level = 0.9)
#p-value of 0.003 is less that alpha = 0.1 hence we reject Null hypothesis i.e. sn_fw-sn_ss>0


#Anova table
anovatable <- dcast(long_data,hour_group~Borough,value.var = 'count')
anovatable <- cont_table[3:7]

anovastack<-stack(anovatable)
names(anovastack) = c("Collisions","Borough")
collisionsaov <- aov(Collisions ~ Borough, data = anovastack)
summary(collisionsaov)

F0 = 21.18

Falpha = qf(0.9,4,115)

#As observed from ANOVA test, there is high difference between boroughs in mean occurence of accidents every hour

TukeyHSD(collisionsaov, ordered = TRUE)


#upon running th Tukey HSD test , only pairs QUEENS-MANHATTAN , BROOKLYN-MANHATTAN , BROOKLYN-QUEENS 
#show evidence of equal mean whereas all other pairs shouls very high variability
