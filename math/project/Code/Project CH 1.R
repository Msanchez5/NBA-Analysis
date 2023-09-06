##########################################
#Beginning of part one
##########################################

#Store the data for part one under the name nba
nba = read.csv("~/math/data/all_seasons.csv")
head(nba)
attach(nba)

#check for any blanks in the data
sum(is.na(nba))

###############################
###Exploratory Data Analysis###
###############################

sort(summary(nba$country),decreasing=TRUE)
str(nba)
names(nba)
dim(nba)

nba$country= as.factor(nba$country)
nba$player_name= as.factor(nba$player_name)
nba$team_abbreviation= as.factor(nba$team_abbreviation)
nba$draft_year= as.factor(nba$draft_year)
nba$draft_round= as.factor(nba$draft_round)
nba$draft_number= as.factor(nba$draft_number)
nba$season= as.factor(nba$season)
nba$college= as.factor(nba$college)

plot(player_height,gp)

plot(age,net_rating,ylim = c(16,40), ylab = "Age", xlab = "Net")
plot(player_height,pts)

###############################################################################################
#Find exploratory data
#look for different combinations: pts for each country, height by each year, country vs country
###############################################################################################

##################################################################
#Using aggregate to find the mean and median for different metrics
##################################################################

height.year<- data.frame()

group.means<-ddply(nba,c("draft_year","height"),summarise,mean=mean(Length))
group.means

summary(nba$player_height)
summary(nba$player_weight)

#By country
aggregate(x=nba$pts,by=list(nba$country=="China"),FUN=mean)
aggregate()
plot(aggregate(x=nba$pts,by=list(nba$country),FUN=mean))

aggregate(x=nba$player_height,by=list(nba$country,nba$season),FUN=mean)
plot(aggregate(x=nba$player_height,by=list(nba$country,nba$season),FUN=mean))

plot(aggregate(x=nba$player_height,by=list(nba$country),FUN=median))

plot(density(nba$player_height))
plot(density(nba$player_weight))

aggregate(x=nba$age,by=list(nba$country),FUN=mean)
plot(density(nba$age))

#By season

aggregate(x=nba$player_height,by=list(nba$season),FUN=median)
plot(aggregate(x=nba$player_height,by=list(nba$season),FUN=median))

aggregate(x=nba$reb,by=list(nba$season),FUN=mean)

aggregate(x=nba$reb,by=list(nba$season==nba$draft_year),FUN=mean)

aggregate(x=nba$age,by=list(nba$season),FUN=mean)

#BY draft

aggregate(x=nba$pts,by=list(nba$draft_number),FUN=mean)

aggregate(x=nba$player_height,by=list(nba$draft_number),FUN=mean)

aggregate(x=nba$reb,by=list(nba$draft_number),FUN=mean)


#By Team

aggregate(x=nba$player_height,by=list(nba$team_abbreviation),FUN=median)
plot(aggregate(x=nba$player_height,by=list(nba$team_abbreviation),FUN=median))

aggregate(x=nba$player_height,by=list(nba$team_abbreviation,nba$season),FUN=mean)

aggregate(x=nba$pts,by=list(nba$team_abbreviation),FUN=mean)

aggregate(x=nba$age,by=list(nba$team_abbreviation),FUN=mean)

aggregate(x=nba$draft_number,by=list(nba$team_abbreviation),FUN=mean)

#By age

aggregate(x=nba$player_height,by=list(nba$age),FUN=mean)

aggregate(x=nba$pts,by=list(nba$age),FUN=mean)

aggregate(x=nba$reb,by=list(nba$age),FUN=mean)

aggregate(x=nba$usg_pct,by=list(nba$age),FUN=mean)

aggregate(x=nba$gp,by=list(nba$age),FUN=mean)

#By games played

aggregate(x=nba$player_height,by=list(nba$gp),FUN=mean)

aggregate(x=nba$gp,by=list(nba$team_abbreviation),FUN=mean)

aggregate(x=nba$player_height,by=list(nba$net_rating),FUN=mean)

plot(aggregate(x=nba$gp,by=list(nba$country),FUN=median))

plot(aggregate(x=nba$net_rating,by=list(nba$player_height),FUN=median),ylim=c(-200,300))

summary(nba$net_rating)


#NEW

aggregate(x=nba$player_height,by=list(nba$country),FUN=median)
aggregate(x=nba$player_height,by=list(subset(nba$country=="China"),nba$season),FUN=median)
plot(aggregate(nba$player_height,by=list(Country=nba$country=="China",nba$season),FUN=median))

#############################################################
#Using aggregate to find different metrics withing continents
#############################################################

library(ggplot2)
library(dplyr)
library(countrycode)

agg=aggregate(x=nba$player_height,by=list(Continent=countrycode(nba$country, origin = 'country.name', destination = 'continent'),Season=nba$season),FUN=median)
plot(agg$Season,agg$x,log="y")
aggregate(x=nba$player_height,by=list(Continent=countrycode(nba$country, origin = 'country.name', destination = 'continent')),FUN=median)
aggregate(x=nba$net_rating,by=list(countrycode(nba$country, origin = 'country.name', destination = 'continent')),FUN=median)
plot

#Distribution

data.frame(aggregate(x=nba$player_height,by=list(nba$country),FUN=median),summary(nba$country))
agg=aggregate(x=nba$player_height,by=list(nba$country),FUN=median)
agg[order(agg$x), ]
summary(nba$country)

#only china

agg.ch=aggregate(x=nba$player_height,by=list(Season=nba$season),FUN=median,data=subset(nba,nba$country=="China"))
plot(agg$Season,agg$x)
(ggplot(agg.ch, aes(x=Season, y=x, group=Season)) + geom_boxplot( fill="skyblue", notch=TRUE) +
  geom_jitter( size=1, color="orange", width=0.2))+scale_y_log10()

nbac=nba[which(nba$country=='China'),]

plot(aggregate(x=nbac$player_height,by=list(nbac$season),FUN=median),log=(nbac$player_height))
aggregate(x=nbac$net_rating,by=list(nbac$season),FUN=median)


#Tanzania
nba.ta=nba[which(nba$country=='Tanzania'),]
aggregate(x=nba.ta$player_height,by=list(nba.ta$season),FUN=median)
aggregate(x=nba.ta$pts,by=list(nba.ta$season),FUN=median)

#South Korea
nba.sk=nba[which(nba$country=='South Korea'),]
aggregate(x=nba.sk$player_height,by=list(nba.sk$season),FUN=median)
aggregate(x=nba.sk$pts,by=list(nba.sk$season),FUN=median)

#Cabo Verde
nba.cv=nba[which(nba$country=='Cabo Verde'),]
aggregate(x=nba.cv$player_height,by=list(nba.cv$season),FUN=median)
aggregate(x=nba.cv$pts,by=list(nba.cv$season),FUN=median)



#Nigeria
nba.ni=nba[which(nba$country=='Nigeria'),]
aggregate(x=nba.ni$player_height,by=list(nba.ni$season),FUN=median)
plot(aggregate(x=nba.ni$player_height,by=list(nba.ni$season),FUN=median))
aggregate(x=nba.ni$pts,by=list(nba.ni$season),FUN=median)

#Puerto Rico
nba.pr=nba[which(nba$country=='Puerto Rico'),]
aggregate(x=nba.pr$player_height,by=list(nba.pr$season),FUN=median)
plot(aggregate(x=nba.pr$player_height,by=list(nba.pr$season),FUN=median),type = "O")
aggregate(x=nba.pr$pts,by=list(nba.pr$season),FUN=median)

#Senegal
nba.se=nba[which(nba$country=='Senegal'),]
aggregate(x=nba.se$player_height,by=list(nba.se$season),FUN=median)
plot(aggregate(x=nba.se$player_height,by=list(nba.se$season),FUN=median),type = "o")
aggregate(x=nba.se$pts,by=list(nba.se$season),FUN=median)


#First pick
nbaone=nba[which(nba$draft_number==1),]
aggregate(x=nbaone$player_height,by=list(nbaone$season),FUN=median)
aggregate(x=nbaone$pts,by=list(nbaone$season),FUN=median)

plot(aggregate(x=nbaone$player_height,by=list(nbaone$season),FUN=median))

#####################################
# Top 10 country by representation (In order)
#
#####################################

# only usa
nbau=nba[which(nba$country=='USA'),]
agg.us=aggregate(x=log(nbau$player_height),by=list(Season=nbau$season),FUN=median)
plot(aggregate(x=nbau$player_height,by=list(nbau$season),FUN=median))


boxplot(nbau$player_height~nbau$season)
boxplot(log(nbau$player_height)~nbau$season)
boxplot(nbau$player_height~nbau$season,log="y")

plot(nbau$player_height,nbau$player_height)

library(aplpack)
bagplot(agg.us$Season,agg.us$x)
aggregate(x=log(nbau$pts),by=list(nbau$season),FUN=median)
aggregate(x=nbau$reb,by=list(nbau$season),FUN=median)

#France
nbaf=nba[which(nba$country=='France'),]
agg.fr=aggregate(x=nbaf$player_height,by=list(season=nbaf$season),FUN=median)

boxplot(agg.fr$x~agg.fr$season)
aggregate(x=nbaf$pts,by=list(nbaf$season),FUN=median)

plot(nbaf$player_height,nbaf$pts)

boxplot(nbaf$player_height~nbaf$season)
boxplot(log(nbaf$player_height)~nbaf$season)
boxplot(nbaf$player_height~nbaf$season,log="y")


#Canada
nba.ca=nba[which(nba$country=='Canada'),]
aggregate(x=nba.ca$player_height,by=list(nba.ca$season),FUN=median)
aggregate(x=nba.ca$pts,by=list(nba.ca$season),FUN=median)

p=boxplot(nba.ca$player_height~nba.ca$season)
boxplot(nba.ca$player_height~nba.ca$season)
boxplot(log(nba.ca$player_height)~nba.ca$season)

plot(nba.ca$player_height,nba.ca$pts)

p + scale_y_continuous(trans = "log10")

#Australia
nba.au=nba[which(nba$country=='Australia'),]
aggregate(x=nba.au$player_height,by=list(nba.au$season),FUN=median)
aggregate(x=nba.au$pts,by=list(nba.au$season),FUN=median)

boxplot(nba.au$player_height~nba.au$season)
boxplot(nba.au$player_height~nba.au$season,log="y")

#Brazil
nba.br=nba[which(nba$country=='Brazil'),]
aggregate(x=nba.br$player_height,by=list(nba.br$season),FUN=median)
aggregate(x=nba.br$pts,by=list(nba.br$season),FUN=median)

boxplot(nba.br$player_height~nba.br$season)
boxplot(nba.br$player_height~nba.br$season,log="y")

#spain
nba.sp=nba[which(nba$country=='Spain'),]
agg.sp=aggregate(x=nba.sp$player_height,by=list(Season=nba.sp$season),FUN=median)

boxplot(agg.sp$x~agg.sp$Season,log="y",notch=TRUE)
aggregate(x=nbas$pts,by=list(nbas$season),FUN=median)

boxplot(nba.sp$player_height~nba.sp$season)
boxplot(nba.sp$player_height~nba.sp$season,log="y")


#Slovenia
nba.sl=nba[which(nba$country=='Slovenia'),]
aggregate(x=nba.sl$player_height,by=list(nba.sl$season),FUN=median)
aggregate(x=nba.sl$pts,by=list(nba.sl$season),FUN=median)

boxplot(nba.sl$player_height~nba.sl$season)
boxplot(nba.sl$player_height~nba.sl$season,log="y")

#Turkey
nba.tu=nba[which(nba$country=='Turkey'),]
aggregate(x=nba.tu$player_height,by=list(nba.tu$season),FUN=median)
aggregate(x=nba.tu$pts,by=list(nba.tu$season),FUN=median)

boxplot(nbau$player_height~nbau$season)
boxplot(nbau$player_height~nbau$season,log="y")

#Croatia
nba.cr=nba[which(nba$country=='Croatia'),]
aggregate(x=nba.cr$player_height,by=list(nba.cr$season),FUN=median)
aggregate(x=nba.cr$pts,by=list(nba.cr$season),FUN=median)

boxplot(nbau$player_height~nbau$season)
boxplot(nbau$player_height~nbau$season,log="y")

#Serbia
nba.se=nba[which(nba$country=='Serbia'),]
aggregate(x=nba.se$player_height,by=list(nba.se$season),FUN=median)
aggregate(x=nba.se$pts,by=list(nba.se$season),FUN=median)

boxplot(nbau$player_height~nbau$season)
boxplot(nbau$player_height~nbau$season,log="y")


#################################################
##  More than 30 players and median height ~ 213##
#################################################


#Lithuania
nba.li=nba[which(nba$country=='Lithuania'),]
agg.li=aggregate(x=nba.li$player_height,by=list(Season=nba.li$season),FUN=median)
plot(aggregate(x=nba.li$player_height,by=list(nba.li$season),FUN=median))

boxplot(agg.li$x~agg.li$Season)

aggregate(x=nba.li$pts,by=list(nba.li$season),FUN=median)

boxplot(nba.li$player_height~nba.li$season)
boxplot(nba.li$player_height~nba.li$season,log="y")


#Nigeria
nba.ni=nba[which(nba$country=='Nigeria'),]
aggregate(x=nba.ni$player_height,by=list(nba.ni$season),FUN=median)
plot(aggregate(x=nba.ni$player_height,by=list(nba.ni$season),FUN=median))
aggregate(x=nba.ni$pts,by=list(nba.ni$season),FUN=median)

p=boxplot(nba.ni$player_height~nba.ni$season)
boxplot(nba.ni$player_height~nba.ni$season,log="y")
library(scales)
plot(nba.ni$season,nba.ni$player_height) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x))

#################################################
# Since linear regression is not optimal based on comparisons from EDA, we opted for Quantile regression
#Quantile regression##
#################################################

library(quantreg)
##########
#only usa#
##########

nbau=nba[which(nba$country=='USA'),]
agg.us=aggregate(x=log(nbau$player_height),by=list(Season=nbau$season),FUN=median)
plot(aggregate(x=nbau$player_height,by=list(nbau$season),FUN=median))


boxplot(nbau$player_height~nbau$season)
boxplot(log(nbau$player_height)~nbau$season)

par(mfrow=c(1,1))
par(mar=c(1,1,1,1))

x<-cbind(nbau$gp,nbau$pts,nbau$oreb_pct,nbau$dreb_pct,nbau$usg_pct)
lm.fit1= lm(nbau$player_height~x)
summary(lm.fit1)
plot(lm.fit1)

lm.fit2= lm(log(nbau$player_weight)~nbau$player_height+I(nbau$player_height^2))
summary(lm.fit2)
plot(lm.fit2)

lm.fit2= lm(nba$ts_pct~nba$player_height)
summary(lm.fit2)
plot(lm.fit2)

rqfit50 <- rq(nbau$player_height~x, tau = 0.50)
rqfit75 <- rq(nbau$player_height~x, tau = 0.75)
anova(rqfit50,rqfit75)
summary(rqfit,se="boot")

quantall<- rq(nbau$ts_pct~nbau$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
quantall<- rq(nbau$usg_pct~nbau$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

summary(rqfit90 <- rq(nbau$ts_pct~nbau$player_height, tau = 0.90))
summary(rqfit30 <- rq(nbau$ts_pct~nbau$player_height, tau = 0.30))
anova(rqfit90,rqfit30)
summary(rqfit90 <- rq(nbau$usg_pct~nbau$player_height, tau = 0.90))
summary(rqfit30 <- rq(nbau$usg_pct~nbau$player_height, tau = 0.30))
anova(rqfit90,rqfit30)


##########
#By Season
##########

rqfit <- rq(nbau$player_height~nbau$season)
rqfit
summary(rqfit)

quantall<- rq(nbau$player_height~nbau$season, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall,se="iid")
plot(quantall.plot)

plot(nbau$season,nbau$player_height, main = "nbau$player_height~nbau$season")
abline(lm(nbau$player_height~nbau$season), col = "red", lty = 2)
abline(rq(nbau$player_height~nbau$season), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)

############
#France only
############

nbaf=nba[which(nba$country=='France'),]
agg.fr=aggregate(x=nbaf$player_height,by=list(season=nbaf$season),FUN=median)

boxplot(agg.fr$x~agg.fr$season)
aggregate(x=nbaf$pts,by=list(nbaf$season),FUN=median)

plot(nbaf$player_height,nbaf$pts)

boxplot(nbaf$player_height~nbaf$season)
boxplot(log(nbaf$player_height)~nbaf$season)
boxplot(nbaf$player_height~nbaf$season,log="y")

x.f<-cbind(nbaf$gp,nbaf$pts,nbaf$oreb_pct,nbaf$dreb_pct,nbaf$usg_pct)


lm.fitf= lm(log(nbaf$player_weight)~nbaf$player_height)
summary(lm.fitf)
plot(lm.fitf)


rqfit50 <- rq(nbaf$player_height~x.f, tau = 0.50)
rqfit75 <- rq(nbaf$player_height~x.f, tau = 0.75)
anova(rqfit50,rqfit75)

quantall<- rq(nbaf$player_height~x.f, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)


rqfit <- rq(nbaf$player_height~nbaf$season)
rqfit

plot(nbaf$season,nbaf$player_height, main = "nbau$player_height~nbau$season")
abline(lm(nbaf$player_height~nbaf$season), col = "red", lty = 2)
abline(rq(nbaf$player_height~nbaf$season), col = "blue", lty = 2)

quantall<- rq(nbaf$ts_pct~nbaf$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
quantall<- rq(nbaf$usg_pct~nbaf$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
plot(nbaf$player_height,nbaf$usg_pct)
summary(rqfit90 <- rq(nbaf$ts_pct~nbaf$player_height, tau = 0.90))
summary(rqfit30 <- rq(nbaf$ts_pct~nbaf$player_height, tau = 0.30))
anova(rqfit90,rqfit30)

summary(rqfit90 <- rq(nbaf$usg_pct~nbaf$player_height, tau = 0.90))
summary(rqfit30 <- rq(nbaf$usg_pct~nbaf$player_height, tau = 0.30))
anova(rqfit90,rqfit30)
lm.fitf= lm(nbaf$usg_pct~nbaf$player_height)
summary(lm.fitf)
plot(lm.fitf)


############
#Canada only
############

nba.ca=nba[which(nba$country=='Canada'),]
aggregate(x=nba.ca$player_height,by=list(nba.ca$season),FUN=median)
aggregate(x=nba.ca$pts,by=list(nba.ca$season),FUN=median)

p=boxplot(nba.ca$player_height~nba.ca$season)
boxplot(nba.ca$player_height~nba.ca$season)
boxplot(log(nba.ca$player_height)~nba.ca$season)

plot(nba.ca$player_height,nba.ca$pts)

p + scale_y_continuous(trans = "log10")


x.ca<-cbind(nba.ca$gp,nba.ca$pts,nba.ca$oreb_pct,nba.ca$dreb_pct,nba.ca$usg_pct)


lm.fitc= lm(nba.ca$player_height~x.ca)
summary(lm.fitc)
plot(lm.fitc)


rqfit50 <- rq(nba.ca$player_height~x.ca, tau = 0.50)
rqfit75 <- rq(nba.ca$player_height~x.ca, tau = 0.75)
anova(rqfit50,rqfit75)

quantall<- rq(nba.ca$player_height~x.ca, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)


rqfit <- rq(nba.ca$player_height~nba.ca$season)
rqfit

plot(nba.ca$season,nba.ca$player_height, main = "nbau$player_height~nbau$season")
abline(lm(nba.ca$player_height~nba.ca$season), col = "red", lty = 2)
abline(rq(nba.ca$player_height~nba.ca$season), col = "blue", lty = 2)

quantall<- rq(nba.ca$ts_pct~nba.ca$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
quantall<- rq(nba.ca$usg_pct~nba.ca$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
plot(nba.ca$player_height,nba.ca$usg_pct)
summary(rqfit90 <- rq(nba.ca$ts_pct~nba.ca$player_height, tau = 0.95))
summary(rqfit30 <- rq(nba.ca$ts_pct~nba.ca$player_height, tau = 0.30))
anova(rqfit90,rqfit30)

summary(rqfit90 <- rq(nba.ca$usg_pct~nba.ca$player_height, tau = 0.90))
summary(rqfit30 <- rq(nba.ca$usg_pct~nba.ca$player_height, tau = 0.30))
anova(rqfit90,rqfit30)

##########################################
#Players picked first overall in the draft
##########################################

nbaone=nba[which(nba$draft_number==1),]
aggregate(x=nbaone$player_height,by=list(nbaone$season),FUN=median)
aggregate(x=nbaone$pts,by=list(nbaone$season),FUN=median)

plot(aggregate(x=nbaone$player_height,by=list(nbaone$season),FUN=median))

plot(nbaone$season,nbaone$player_height)

lm.fit5= lm(nbaone$player_height~nbaone$season)
summary(lm.fit5)
plot(lm.fit5)

quantall<- rq(nbaone$ts_pct~nbaone$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
quantall<- rq(nbaone$usg_pct~nbaone$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
plot(nbaone$player_height,nba.ca$usg_pct)
summary(rqfit90 <- rq(nbaone$ts_pct~nbaone$player_height, tau = 0.95))
summary(rqfit30 <- rq(nbaone$ts_pct~nbaone$player_height, tau = 0.30))
anova(rqfit90,rqfit30)

summary(rqfit90 <- rq(nbaone$usg_pct~nbaone$player_height, tau = 0.90))
summary(rqfit30 <- rq(nbaone$usg_pct~nbaone$player_height, tau = 0.30))
anova(rqfit90,rqfit30)

x.one<-cbind(nbaone$gp,nbaone$pts,nbaone$oreb_pct,nbaone$dreb_pct,nbaone$usg_pct)


lm.fito= lm(nbaone$player_height~x.one)
summary(lm.fito)
plot(lm.fito)


rqfit25 <- rq(nbaone$player_height~x.one, tau = 0.25)
rqfit75 <- rq(nbaone$player_height~x.one, tau = 0.75)
anova(rqfit25,rqfit75)

quantall<- rq(nbaone$player_height~x.one, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

###############################
#Categorizing un-drafted players
#In the data, un-drafted players were categorized as a character in the column
#Now un-Drafted players with categorized as an integer with the number 0
###############################

nba.un=data.frame(nba,un)
un<-ifelse(nba$draft_number =="undrafted",0,nba$draft_number)
summary(nba.un)
str(nba.un)
nba.un$un<-as.numeric(nba.un$un)
plot(nba.un$un,nba.un$player_height)

lm.fit4= lm(nba.un$un~nba.un$player_height)
summary(lm.fit4)
plot(lm.fit4)

lm.fit4= lm(nba$player_height~nba$player_weight)
summary(lm.fit4)
plot(lm.fit4)

##############################################
#Players drafted in the top 14 picks (lottery)
##############################################

nba.lot=nba[which(nba$draft_number<15),]

x.lot<-cbind(nba.lot$gp,nba.lot$pts,nba.lot$oreb_pct,nba.lot$dreb_pct,nba.lot$usg_pct)
names(nba.lot)
names(nba.un)
summary(nba.lot$un)
lm.fitl= lm(nba.lot$player_height~x.lot)
summary(lm.fitl)
plot(lm.fitl)


rqfit25 <- rq(nba.lot$player_height~x.lot, tau = 0.25)
rqfit75 <- rq(nba.lot$player_height~x.lot, tau = 0.75)
anova(rqfit25,rqfit75)

quantall<- rq(nba.lot$player_height~x.lot, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

quantall<- rq(nba.lot$ts_pct~nba.lot$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
quantall<- rq(nba.lot$usg_pct~nba.lot$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
plot(nba.ca$player_height,nba.ca$usg_pct)
summary(rqfit90 <- rq(nba.lot$ts_pct~nba.lot$player_height, tau = 0.95))
summary(rqfit30 <- rq(nba.lot$ts_pct~nba.lot$player_height, tau = 0.30))
anova(rqfit90,rqfit30)

summary(rqfit90 <- rq(nba.lot$usg_pct~nba.lot$player_height, tau = 0.90))
summary(rqfit30 <- rq(nba.lot$usg_pct~nba.lot$player_height, tau = 0.30))
anova(rqfit90,rqfit30)



#################################################
##Here we combine players to get a mean or median of their whole career##
#################################################

nba.num <- data.frame(nba$player_name, as.numeric(as.character(nba$draft_year)))


un<-ifelse(nba$draft_year =="undrafted",0,as.numeric(as.character(nba.num$draft_year)))
nba.un=data.frame(nba,un)

x.p<-cbind(nba$player_name, nba$player_height,nba$player_weight, nba$gp,nba$pts, nba$reb, nba$usg_pct, nba$ts_pct, nba$team_abbreviation)

nba.players <- do.call(`data.frame`, aggregate(x.p~cbind(player_name=sub('\\d+$', '',player_name)), nba, median))
names(nba.players)

library(data.table)
nms <- c("player_name", "draft_year", "player_height","player_weight", "gp","pts", "reb", "usg_pct", "ts_pct", "Team")
setnames(nba.players, nms)
names(nba.players)

plot(nba.players$draft_year ,nba.players$player_height)

############################################
#Quantile regression on the combined players data
############################################

quantall<- rq(nba.players$ts_pct~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
quantall<- rq(nba.players$usg_pct~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)
plot(nba.ca$player_height,nba.ca$usg_pct)
summary(rqfit90 <- rq(nba.players$ts_pct~nba.players$player_height, tau = 0.95))
summary(rqfit30 <- rq(nba.players$ts_pct~nba.players$player_height, tau = 0.30))
anova(rqfit90,rqfit30)

summary(rqfit90 <- rq(nba.players$usg_pct~nba.players$player_height, tau = 0.90))
summary(rqfit30 <- rq(nba.players$usg_pct~nba.players$player_height, tau = 0.30))
anova(rqfit90,rqfit30)




##########################################################
##Here we will focus on Height and weight of the players##
##########################################################
par(mar=c(1,1,1,1))
plot(nba.players$player_weight ,nba.players$player_height)
boxplot(nba.players$player_weight~nba.players$player_height)

lm.playerl= lm(nba.players$player_weight~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)

library(quantreg)

rqfit25 <- rq(nba.players$player_weight~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$player_weight~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$player_weight~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

########################################
#Here we try to split the data by height
########################################

nba.p2T=nba.players[which(nba.players$player_height>209),]

nba.p2M=nba.players[which(nba.players$player_height>190 & nba.players$player_height <210),]

nba.p2S=nba.players[which(nba.players$player_height<191),]

plot(nba.p2T$player_height,nba.p2T$pts)
text(nba.p2T$player_height, nba.p2T$pts, nba.p2T$player_name)
boxplot(nba.p2T$pts~nba.p2T$player_height)

plot(nba.p2M$player_height,nba.p2M$pts)
text(nba.p2M$player_height, nba.p2M$pts, nba.p2M$player_name)
boxplot(nba.p2M$pts~nba.p2M$player_height)

plot(nba.p2S$player_height,nba.p2S$pts)
text(nba.p2S$player_height, nba.p2S$pts, row.names(nba.p2S$player_name), cex=0.6, pos=4, col="red")
text(nba.p2S$player_height, nba.p2S$pts, nba.p2S$player_name)
boxplot(nba.p2S$pts~nba.p2S$player_height)


########################################################
##Here we see the comparison between Height and points##
########################################################

plot(nba.players$player_height,nba.players$pts)
boxplot(nba.players$pts~nba.players$player_height)

lm.playerl= lm(nba.players$pts~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)

library(quantreg)

rqfit25 <- rq(nba.players$pts~nba.players$player_height, tau = 0.5)
summary(rqfit25)
rqfit75 <- rq(nba.players$pts~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$pts~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

######################################################################################
#Since POISSON regression is best used for count data, we decided to use it for points here
######################################################################################

summary(m1 <- glm(nba.players$pts ~ nba.players$player_height, family="poisson"))
plot(m1)


plot(nba.p2T$player_height,nba.p2T$pts)
text(nba.p2T$player_height, nba.p2T$pts, nba.p2T$player_name)
boxplot(nba.p2T$pts~nba.p2T$player_height)

plot(nba.p2M$player_height,nba.p2M$pts)
text(nba.p2M$player_height, nba.p2M$pts, nba.p2M$player_name)
boxplot(nba.p2M$pts~nba.p2M$player_height)

plot(nba.p2S$player_height,nba.p2S$pts)
text(nba.p2S$player_height, nba.p2S$pts, row.names(nba.p2S$player_name), cex=0.6, pos=4, col="red")
text(nba.p2S$player_height, nba.p2S$pts, nba.p2S$player_name)
boxplot(nba.p2S$pts~nba.p2S$player_height)



##########################################################
##Here we see the comparison between Height and rebounds##
##########################################################

plot(nba.players$player_height,nba.players$reb )
boxplot(nba.players$reb~nba.players$player_height)

lm.playerl= lm(nba.players$reb~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)

library(quantreg)

rqfit25 <- rq(nba.players$reb~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$reb~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$reb~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

###################
#POISSON regression
###################

summary(m1 <- glm(nba.players$reb ~ nba.players$player_height, family="poisson"))
plot(m1)

######################################################################################
#Here we try to find who the tallest players are and determine how their stats compare
#also, we try to find other players in our data that we split by height
######################################################################################

plot(nba.p2T$player_height,nba.p2T$reb)
text(nba.p2T$player_height, nba.p2T$reb, nba.p2T$player_name,pos=2)
boxplot(nba.p2T$reb~nba.p2T$player_height)

nba.Gheorghe=nba[which(nba$player_name=="Shawn Bradley"),]
View(nba.Gheorghe)

nba.Gheorghe=nba[which(nba$team_abbreviation=="DAL" & nba$season=="2002-03"),]

plot(nba.Gheorghe$player_height, nba.Gheorghe$pts )

View(nba.Gheorghe)


plot(nba.p2M$player_height,nba.p2M$reb)
text(nba.p2M$player_height, nba.p2M$reb, nba.p2M$player_name)
boxplot(nba.p2M$reb~nba.p2M$player_height)

plot(nba.p2S$player_height,nba.p2S$reb)
text(nba.p2S$player_height, nba.p2S$reb, row.names(nba.p2S$player_name), cex=0.6, pos=4, col="red")
text(nba.p2S$player_height, nba.p2S$reb, nba.p2S$player_name)
boxplot(nba.p2S$reb~nba.p2S$player_height)


#####################################################
##Here we compare the Height and assists of players##
#####################################################

plot(nba.players$player_height,nba.players$ast )
boxplot(nba.players$ast~nba.players$player_height)

lm.playerl= lm(nba.players$ast~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)

library(quantreg)

rqfit25 <- rq(nba.players$ast~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$ast~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$ast~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

###################
#POISSON regression
###################
summary(m1 <- glm(nba.players$ast ~ nba.players$player_height, family="poisson"))
plot(m1)




#####################################################
##Here we compare the players Height and Net rating##
#####################################################

plot(nba.players$player_height,nba.players$net_rating )
boxplot(nba.players$net_rating~nba.players$player_height)

lm.playerl= lm(nba.players$net_rating~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)

library(quantreg)

rqfit25 <- rq(nba.players$net_rating~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$net_rating~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$net_rating~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

####################################################################
##Here we compare the players Height and Offensive rebound percent##
####################################################################

plot(nba.players$player_height,nba.players$oreb_pct )
boxplot(nba.players$oreb_pct~nba.players$player_height)

lm.playerl= lm(nba.players$oreb_pct~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)


library(quantreg)

rqfit25 <- rq(nba.players$oreb_pct~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$oreb_pct~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$oreb_pct~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

####################################################################
##Here we compare the players Height and Defensive rebound percent##
####################################################################

plot(nba.players$player_height,nba.players$dreb_pct )
boxplot(nba.players$dreb_pct~nba.players$player_height)

lm.playerl= lm(nba.players$dreb_pct~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)


library(quantreg)

rqfit25 <- rq(nba.players$dreb_pct~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$dreb_pct~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$dreb_pct~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

########################################################
##Here we compare the players Height and Usage percent##
########################################################

plot(nba.players$player_height,nba.players$usg_pct )
boxplot(nba.players$usg_pct~nba.players$player_height)

lm.playerl= lm(nba.players$usg_pct~nba.players$player_height )
summary(lm.playerl)
plot(lm.playerl)


library(quantreg)

rqfit25 <- rq(nba.players$usg_pct~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$usg_pct~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$usg_pct~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

###############################################################
##Here we determine what was the Best season  based on points##
###############################################################

t<-aggregate(x=nba$pts,by=list(nba$season),FUN=median)
b<-aggregate(x=nba$reb,by=list(nba$season),FUN=median)
data.frame(t,b)

nba.season10=nba[which(nba$season=="2009-10"),]
nba.season97=nba[which(nba$season=="1996-97"),]
nba.season20=nba[which(nba$season=="2020-21"),]
plot(nba.season97$player_height,nba.season97$pts)
plot(nba.season20$player_height,nba.season20$pts)
hist(nba.season97$player_height)
hist(nba.season20$player_height)
summary(m1 <- glm(nba.season97$pts ~ nba.season97$player_height, family="poisson"))
summary(m1 <- glm(nba.season20$pts ~ nba.season20$player_height, family="poisson"))



nba.season10T=nba[which(nba$season=="2009-10" & nba$player_height>203),]
nba.season10S=nba[which(nba$season=="2009-10" & nba$player_height<204),]

nba.season21=nba[which(nba$season=="2020-21"),]


aggregate(x=nba.season10$pts,by=list(nba.season10$team_abbreviation),FUN=median)
aggregate(x=nba.season10T$pts,by=list(nba.season10T$team_abbreviation),FUN=median)
aggregate(x=nba.season10S$pts,by=list(nba.season10S$team_abbreviation),FUN=median)

plot(nba.season10T$player_height,nba.season10T$pts )
text(nba.season10T$player_height, nba.season10T$pts, nba.season10T$player_name)

lm.playerl= lm(nba.season10T$ast~nba.season10T$player_height )
summary(lm.playerl)
plot(lm.playerl)

library(quantreg)

rqfit25 <- rq(nba.players$ast~nba.players$player_height, tau = 0.25)
rqfit75 <- rq(nba.players$ast~nba.players$player_height, tau = 0.75)
anova(rqfit25,rqfit75)

####################################################################################
#Justification in using quantile regression. 
#The plot below includes linear regression coefficient line with dash lines as the confidence intervals.
#This shows whether quantile would better use than linear regression in a different range
####################################################################################

quantall<- rq(nba.players$ast~nba.players$player_height, tau = seq(0.25,0.95, by = 0.05))
quantall.plot <- summary(quantall)
plot(quantall.plot)

########
#POISSON
########

summary(m1 <- glm(nba.players$ast ~ nba.players$player_height, family="poisson"))
plot(m1)

############################################################################
#Based on points, we determined these were the best teams for each category#
############################################################################

#GSW (all players)
nba.season10=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="GSW"),]
plot(nba.season10$player_height,nba.season10$pts)
text(nba.season10$player_height, nba.season10$pts, nba.season10$player_name)

summary(m1 <- glm(nba.season10$pts ~ nba.season10$player_height, family="poisson"))


#LAL (players > 210)
nba.season10T=nba[which(nba$season=="2009-10" & nba$player_height>209 & nba$team_abbreviation=="LAL"),]
plot(nba.season10T$player_height,nba.season10T$pts)
text(nba.season10T$player_height, nba.season10T$pts, nba.season10T$player_name)

#NYK (players > 203)
nba.season10T=nba[which(nba$season=="2009-10" & nba$player_height>203 & nba$team_abbreviation=="NYK"),]
plot(nba.season10T$player_height,nba.season10T$pts)
text(nba.season10T$player_height, nba.season10T$pts, nba.season10T$player_name)

#NOH (players < 190)
nba.season10S=nba[which(nba$season=="2009-10" & nba$player_height<190 & nba$team_abbreviation=="NOH"),]
plot(nba.season10S$player_height,nba.season10S$pts)
text(nba.season10S$player_height, nba.season10S$pts, nba.season10S$player_name)


hist(nba$player_height)

b<-aggregate(x=nba$reb,by=list(nba$season),FUN=mean)


#################################################################
##Here we try to determine the Best Team by rebounds and points##
#################################################################

t<-aggregate(x=nba$pts,by=list(nba$team_abbreviation ),FUN=mean)
b<-aggregate(x=nba$reb,by=list(nba$team_abbreviation ),FUN=mean)
data.frame(t,b)


###############################################
#The team with the best points was New Orleans#
###############################################

nba.NOP=nba[which(nba$team_abbreviation=="NOK"),]

dim(nba.NOP)
plot(nba.NOP$player_height,nba.NOP$pts )
text(nba.NOP$player_height, nba.NOP$pts, nba.NOP$player_name)

summary(m1 <- glm(nba.NOP$pts ~ nba.NOP$player_height, family="poisson"))
plot(m1)

##################################################
#The team with the best rebounds was Golden State#
##################################################

nba.GSW=nba[which(nba$team_abbreviation=="GSW"),]


plot(nba.GSW$player_height,nba.GSW$reb )
text(nba.GSW$player_height, nba.GSW$reb, nba.GSW$player_name)

summary(m1 <- glm(nba.GSW$pts ~ nba.GSW$player_height, family="poisson"))
plot(m1)

###################################################
##Indiana was determined to have the tallest team##
###################################################

aggregate(x=nba$player_height ,by=list(nba$team_abbreviation ),FUN=mean)
b<-aggregate(x=nba$reb,by=list(nba$team_abbreviation ),FUN=mean)
data.frame(t,b)

nba.IND=nba[which(nba$team_abbreviation=="IND"),]

plot(nba.IND$player_height,nba.IND$pts )
text(nba.IND$player_height, nba.IND$pts, nba.IND$player_name)

summary(m1 <- glm(nba.IND$pts ~ nba.IND$player_height, family="poisson"))
plot(m1)

plot(nba.IND$player_height,nba.IND$reb )
text(nba.IND$player_height, nba.IND$reb, nba.IND$player_name)

summary(m1 <- glm(nba.IND$reb ~ nba.IND$player_height, family="poisson"))
plot(m1)


#########################################################################################################
##Based on points we determined that LeBron James, Kevin Durant, and Allen Iverson were the top scorers##
#########################################################################################################

agg<-aggregate(x=nba$pts ,by=list(nba$player_name ),FUN=mean)
agg[order(agg$x,decreasing=TRUE), ]

nba.LB=nba[which(nba$player_name =="LeBron James"),]
nba.LB$player_height

nba.KD=nba[which(nba$player_name =="Kevin Durant"),]
nba.KD$player_height

nba.AI=nba[which(nba$player_name =="Allen Iverson"),]
nba.AI$player_height

###########################################################################################
##Here we determined that the 2009-10 season was the best in terms of points and rebounds##
##Thus, we analyzed each team during this season by using poisson regression##
###########################################################################################

nba.season10=nba[which(nba$season=="2009-10"),]
summary(nba.season10$team_abbreviation)

nba.season10ATL=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="ATL"),]
plot(nba.season10ATL$player_height,nba.season10ATL$reb )
text(nba.season10ATL$player_height, nba.season10ATL$reb, nba.season10ATL$player_name)
x.atlreb<-summary(m1 <- glm(nba.season10ATL$reb ~ nba.season10ATL$player_height, family="poisson"))
x.atlpts<-summary(m1 <- glm(nba.season10ATL$pts ~ nba.season10ATL$player_height, family="poisson"))

nba.season10BOS=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="BOS"),]
x.bosreb<-summary(m1 <- glm(nba.season10BOS$reb ~ nba.season10BOS$player_height, family="poisson"))
x.bospts<-summary(m1 <- glm(nba.season10BOS$pts ~ nba.season10BOS$player_height, family="poisson"))

nba.season10CHA=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="CHA"),]
x.chareb<-summary(m1 <- glm(nba.season10CHA$reb ~ nba.season10CHA$player_height, family="poisson"))
x.chapts<-summary(m1 <- glm(nba.season10CHA$pts ~ nba.season10CHA$player_height, family="poisson"))

nba.season10CHI=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="CHI"),]
x.chireb<-summary(m1 <- glm(nba.season10CHI$reb ~ nba.season10CHI$player_height, family="poisson"))
x.chipts<-summary(m1 <- glm(nba.season10CHI$pts ~ nba.season10CHI$player_height, family="poisson"))

nba.season10CLE=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="CLE"),]
x.clereb<-summary(m1 <- glm(nba.season10CLE$reb ~ nba.season10CLE$player_height, family="poisson"))
x.clepts<-summary(m1 <- glm(nba.season10CLE$pts ~ nba.season10CLE$player_height, family="poisson"))

nba.season10DAL=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="DAL"),]
x.dalreb<-summary(m1 <- glm(nba.season10DAL$reb ~ nba.season10DAL$player_height, family="poisson"))
x.dalpts<-summary(m1 <- glm(nba.season10DAL$pts ~ nba.season10DAL$player_height, family="poisson"))

nba.season10DEN=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="DEN"),]
x.denreb<-summary(m1 <- glm(nba.season10DEN$reb ~ nba.season10DEN$player_height, family="poisson"))
x.denpts<-summary(m1 <- glm(nba.season10DEN$pts ~ nba.season10DEN$player_height, family="poisson"))

nba.season10DET=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="DET"),]
x.detreb<-summary(m1 <- glm(nba.season10DET$reb ~ nba.season10DET$player_height, family="poisson"))
x.detpts<-summary(m1 <- glm(nba.season10DET$pts ~ nba.season10DET$player_height, family="poisson"))

nba.season10GSW=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="GSW"),]
x.gswreb<-summary(m1 <- glm(nba.season10GSW$reb ~ nba.season10GSW$player_height, family="poisson"))
x.gswpts<-summary(m1 <- glm(nba.season10GSW$pts ~ nba.season10GSW$player_height, family="poisson"))

nba.season10HOU=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="HOU"),]
x.houreb<-summary(m1 <- glm(nba.season10HOU$reb ~ nba.season10HOU$player_height, family="poisson"))
x.houpts<-summary(m1 <- glm(nba.season10HOU$pts ~ nba.season10HOU$player_height, family="poisson"))

nba.season10IND=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="IND"),]
x.indreb<-summary(m1 <- glm(nba.season10IND$reb ~ nba.season10IND$player_height, family="poisson"))
x.indpts<-summary(m1 <- glm(nba.season10IND$pts ~ nba.season10IND$player_height, family="poisson"))

nba.season10LAC=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="LAC"),]
x.lacreb<-summary(m1 <- glm(nba.season10LAC$reb ~ nba.season10LAC$player_height, family="poisson"))
x.lacpts<-summary(m1 <- glm(nba.season10LAC$pts ~ nba.season10LAC$player_height, family="poisson"))

nba.season10LAL=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="LAL"),]
x.lalreb<-summary(m1 <- glm(nba.season10LAL$reb ~ nba.season10LAL$player_height, family="poisson"))
x.lalpts<-summary(m1 <- glm(nba.season10LAL$pts ~ nba.season10LAL$player_height, family="poisson"))

nba.season10MEM=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="MEM"),]
x.memreb<-summary(m1 <- glm(nba.season10MEM$reb ~ nba.season10MEM$player_height, family="poisson"))
x.mempts<-summary(m1 <- glm(nba.season10MEM$pts ~ nba.season10MEM$player_height, family="poisson"))

nba.season10MIA=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="MIA"),]
x.miareb<-summary(m1 <- glm(nba.season10MIA$reb ~ nba.season10MIA$player_height, family="poisson"))
x.miapts<-summary(m1 <- glm(nba.season10MIA$pts ~ nba.season10MIA$player_height, family="poisson"))

nba.season10MIL=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="MIL"),]
x.milreb<-summary(m1 <- glm(nba.season10MIL$reb ~ nba.season10MIL$player_height, family="poisson"))
x.milpts<-summary(m1 <- glm(nba.season10MIL$pts ~ nba.season10MIL$player_height, family="poisson"))

nba.season10MIN=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="MIN"),]
x.minreb<-summary(m1 <- glm(nba.season10MIN$reb ~ nba.season10MIN$player_height, family="poisson"))
x.minpts<-summary(m1 <- glm(nba.season10MIN$pts ~ nba.season10MIN$player_height, family="poisson"))

nba.season10NJN=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="NJN"),]
x.njnreb<-summary(m1 <- glm(nba.season10NJN$reb ~ nba.season10NJN$player_height, family="poisson"))
x.njnpts<-summary(m1 <- glm(nba.season10NJN$pts ~ nba.season10NJN$player_height, family="poisson"))

nba.season10NOH=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="NOH"),]
x.nohreb<-summary(m1 <- glm(nba.season10NOH$reb ~ nba.season10NOH$player_height, family="poisson"))
x.nohpts<-summary(m1 <- glm(nba.season10NOH$pts ~ nba.season10NOH$player_height, family="poisson"))

nba.season10NYK=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="NYK"),]
x.nykreb<-summary(m1 <- glm(nba.season10NYK$reb ~ nba.season10NYK$player_height, family="poisson"))
x.nykpts<-summary(m1 <- glm(nba.season10NYK$pts ~ nba.season10NYK$player_height, family="poisson"))

nba.season10OKC=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="OKC"),]
x.okcreb<-summary(m1 <- glm(nba.season10OKC$reb ~ nba.season10OKC$player_height, family="poisson"))
x.okcpts<-summary(m1 <- glm(nba.season10OKC$pts ~ nba.season10OKC$player_height, family="poisson"))

nba.season10ORL=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="ORL"),]
x.orlreb<-summary(m1 <- glm(nba.season10ORL$reb ~ nba.season10ORL$player_height, family="poisson"))
x.orlpts<-summary(m1 <- glm(nba.season10ORL$pts ~ nba.season10ORL$player_height, family="poisson"))

nba.season10PHI=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="PHI"),]
x.phireb<-summary(m1 <- glm(nba.season10PHI$reb ~ nba.season10PHI$player_height, family="poisson"))
x.phipts<-summary(m1 <- glm(nba.season10PHI$pts ~ nba.season10PHI$player_height, family="poisson"))

nba.season10PHX=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="PHX"),]
x.phxreb<-summary(m1 <- glm(nba.season10PHX$reb ~ nba.season10PHX$player_height, family="poisson"))
x.phxpts<-summary(m1 <- glm(nba.season10PHX$pts ~ nba.season10PHX$player_height, family="poisson"))

nba.season10POR=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="POR"),]
x.porreb<-summary(m1 <- glm(nba.season10POR$reb ~ nba.season10POR$player_height, family="poisson"))
x.porpts<-summary(m1 <- glm(nba.season10POR$pts ~ nba.season10POR$player_height, family="poisson"))

nba.season10SAC=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="SAC"),]
x.sacreb<-summary(m1 <- glm(nba.season10SAC$reb ~ nba.season10SAC$player_height, family="poisson"))
x.sacpts<-summary(m1 <- glm(nba.season10SAC$pts ~ nba.season10SAC$player_height, family="poisson"))

nba.season10SAS=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="SAS"),]
x.sasreb<-summary(m1 <- glm(nba.season10SAS$reb ~ nba.season10SAS$player_height, family="poisson"))
x.saspts<-summary(m1 <- glm(nba.season10SAS$pts ~ nba.season10SAS$player_height, family="poisson"))

nba.season10TOR=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="TOR"),]
x.torreb<-summary(m1 <- glm(nba.season10TOR$reb ~ nba.season10TOR$player_height, family="poisson"))
x.torpts<-summary(m1 <- glm(nba.season10TOR$pts ~ nba.season10TOR$player_height, family="poisson"))

nba.season10UTA=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="UTA"),]
x.utareb<-summary(m1 <- glm(nba.season10UTA$reb ~ nba.season10UTA$player_height, family="poisson"))
x.utapts<-summary(m1 <- glm(nba.season10UTA$pts ~ nba.season10UTA$player_height, family="poisson"))

nba.season10WAS=nba[which(nba$season=="2009-10" & nba$team_abbreviation=="WAS"),]
x.wasreb<-summary(m1 <- glm(nba.season10WAS$reb ~ nba.season10WAS$player_height, family="poisson"))
x.waspts<-summary(m1 <- glm(nba.season10WAS$pts ~ nba.season10WAS$player_height, family="poisson"))

data.frame(x.clereb$coefficients,x.dalreb$coefficients,x.denreb$coefficients,x.detreb$coefficients,x.gswreb$coefficients,
                 x.houreb$coefficients)
x.atlreb$coefficients
x.bosreb$coefficients
x.chareb$coefficients
x.chireb$coefficients
x.clereb$coefficients
x.dalreb$coefficients
x.denreb$coefficients
x.detreb$coefficients
x.gswreb$coefficients
x.houreb$coefficients
x.indreb$coefficients
x.lacreb$coefficients
x.lalreb$coefficients
x.memreb$coefficients
x.miareb$coefficients
x.milreb$coefficients
x.minreb$coefficients
x.njnreb$coefficients
x.nohreb$coefficients
x.nykreb$coefficients
x.okcreb$coefficients
x.orlreb$coefficients
x.phireb$coefficients
x.phxreb$coefficients
x.porreb$coefficients
x.sacreb$coefficients
x.sasreb$coefficients
x.torreb$coefficients
x.utareb$coefficients
x.wasreb$coefficients

x.atlpts$coefficients
x.bospts$coefficients
x.chapts$coefficients
x.chipts$coefficients
x.clepts$coefficients
x.dalpts$coefficients
x.denpts$coefficients
x.detpts$coefficients
x.gswpts$coefficients
x.houpts$coefficients
x.indpts$coefficients
x.lacpts$coefficients
x.lalpts$coefficients
x.mempts$coefficients
x.miapts$coefficients
x.milpts$coefficients
x.minpts$coefficients
x.njnpts$coefficients
x.nohpts$coefficients
x.nykpts$coefficients
x.okcpts$coefficients
x.orlpts$coefficients
x.phipts$coefficients
x.phxpts$coefficients
x.porpts$coefficients
x.sacpts$coefficients
x.saspts$coefficients
x.torpts$coefficients
x.utapts$coefficients
x.waspts$coefficients


##############################################################################################################
##Here we determine that the Los Angeles Lakers were the most consistent team in terms  ONE TEAM ALL SEASONS##
##We analyzed the Lakers for each season we had in the data using poisson regression##
##############################################################################################################

############################
#BEST points & BEST rebounds
############################

nba.LAL=nba[which(nba$team_abbreviation=="LAL"),]

plot(nba.LAL$player_height,nba.LAL$reb )
text(nba.LAL$player_height, nba.LAL$reb, nba.LAL$player_name)


as.integer(nba.LAL97$reb) 
nba.LAL97=nba.LAL[which(nba.LAL$season=="1996-97"),]
(x.LAL97reb<-summary(m1 <- glm(nba.LAL97$reb ~ nba.LAL97$player_height, family="poisson")))
x.LAL97pts<-summary(m1 <- glm(nba.LAL97$pts ~ nba.LAL97$player_height, family="poisson"))
x.LAL97reb$coefficients

nba.LAL98=nba.LAL[which(nba.LAL$season=="1997-98"),]
x.LAL98reb<-summary(m1 <- glm(nba.LAL98$reb ~ nba.LAL98$player_height, family="poisson"))
x.LAL98pts<-summary(m1 <- glm(nba.LAL98$pts ~ nba.LAL98$player_height, family="poisson"))

nba.LAL99=nba.LAL[which(nba.LAL$season=="1998-99"),]
x.LAL99reb<-summary(m1 <- glm(nba.LAL99$reb ~ nba.LAL99$player_height, family="poisson"))
x.LAL99pts<-summary(m1 <- glm(nba.LAL99$pts ~ nba.LAL99$player_height, family="poisson"))

nba.LAL00=nba.LAL[which(nba.LAL$season=="1999-00"),]
x.LAL00reb<-summary(m1 <- glm(nba.LAL00$reb ~ nba.LAL00$player_height, family="poisson"))
x.LAL00pts<-summary(m1 <- glm(nba.LAL00$pts ~ nba.LAL00$player_height, family="poisson"))

nba.LAL01=nba.LAL[which(nba.LAL$season=="2000-01"),]
x.LAL01reb<-summary(m1 <- glm(nba.LAL01$reb ~ nba.LAL01$player_height, family="poisson"))
x.LAL01pts<-summary(m1 <- glm(nba.LAL01$pts ~ nba.LAL01$player_height, family="poisson"))

nba.LAL02=nba.LAL[which(nba.LAL$season=="2001-02"),]
x.LAL02reb<-summary(m1 <- glm(nba.LAL02$reb ~ nba.LAL02$player_height, family="poisson"))
x.LAL02pts<-summary(m1 <- glm(nba.LAL02$pts ~ nba.LAL02$player_height, family="poisson"))

nba.LAL03=nba.LAL[which(nba.LAL$season=="2002-03"),]
x.LAL03reb<-summary(m1 <- glm(nba.LAL03$reb ~ nba.LAL03$player_height, family="poisson"))
x.LAL03pts<-summary(m1 <- glm(nba.LAL03$pts ~ nba.LAL03$player_height, family="poisson"))

nba.LAL04=nba.LAL[which(nba.LAL$season=="2003-04"),]
x.LAL04reb<-summary(m1 <- glm(nba.LAL04$reb ~ nba.LAL04$player_height, family="poisson"))
x.LAL04pts<-summary(m1 <- glm(nba.LAL04$pts ~ nba.LAL04$player_height, family="poisson"))

nba.LAL05=nba.LAL[which(nba.LAL$season=="2004-05"),]
x.LAL05reb<-summary(m1 <- glm(nba.LAL05$reb ~ nba.LAL05$player_height, family="poisson"))
x.LAL05pts<-summary(m1 <- glm(nba.LAL05$pts ~ nba.LAL05$player_height, family="poisson"))

nba.LAL06=nba.LAL[which(nba.LAL$season=="2005-06"),]
x.LAL06reb<-summary(m1 <- glm(nba.LAL06$reb ~ nba.LAL06$player_height, family="poisson"))
x.LAL06pts<-summary(m1 <- glm(nba.LAL06$pts ~ nba.LAL06$player_height, family="poisson"))

nba.LAL07=nba.LAL[which(nba.LAL$season=="2006-07"),]
x.LAL07reb<-summary(m1 <- glm(nba.LAL07$reb ~ nba.LAL07$player_height, family="poisson"))
x.LAL07pts<-summary(m1 <- glm(nba.LAL07$pts ~ nba.LAL07$player_height, family="poisson"))

nba.LAL08=nba.LAL[which(nba.LAL$season=="2007-08"),]
x.LAL08reb<-summary(m1 <- glm(nba.LAL08$reb ~ nba.LAL08$player_height, family="poisson"))
x.LAL08pts<-summary(m1 <- glm(nba.LAL08$pts ~ nba.LAL08$player_height, family="poisson"))

nba.LAL09=nba.LAL[which(nba.LAL$season=="2008-09"),]
x.LAL09reb<-summary(m1 <- glm(nba.LAL09$reb ~ nba.LAL09$player_height, family="poisson"))
x.LAL09pts<-summary(m1 <- glm(nba.LAL09$pts ~ nba.LAL09$player_height, family="poisson"))

nba.LAL10=nba.LAL[which(nba.LAL$season=="2009-10"),]
x.LAL10reb<-summary(m1 <- glm(nba.LAL10$reb ~ nba.LAL10$player_height, family="poisson"))
x.LAL10pts<-summary(m1 <- glm(nba.LAL10$pts ~ nba.LAL10$player_height, family="poisson"))

nba.LAL11=nba.LAL[which(nba.LAL$season=="2010-11"),]
x.LAL11reb<-summary(m1 <- glm(nba.LAL11$reb ~ nba.LAL11$player_height, family="poisson"))
x.LAL11pts<-summary(m1 <- glm(nba.LAL11$pts ~ nba.LAL11$player_height, family="poisson"))

nba.LAL12=nba.LAL[which(nba.LAL$season=="2011-12"),]
x.LAL12reb<-summary(m1 <- glm(nba.LAL12$reb ~ nba.LAL12$player_height, family="poisson"))
x.LAL12pts<-summary(m1 <- glm(nba.LAL12$pts ~ nba.LAL12$player_height, family="poisson"))

nba.LAL13=nba.LAL[which(nba.LAL$season=="2012-13"),]
x.LAL13reb<-summary(m1 <- glm(nba.LAL13$reb ~ nba.LAL13$player_height, family="poisson"))
x.LAL13pts<-summary(m1 <- glm(nba.LAL13$pts ~ nba.LAL13$player_height, family="poisson"))

nba.LAL14=nba.LAL[which(nba.LAL$season=="2013-14"),]
x.LAL14reb<-summary(m1 <- glm(nba.LAL14$reb ~ nba.LAL14$player_height, family="poisson"))
x.LAL14pts<-summary(m1 <- glm(nba.LAL14$pts ~ nba.LAL14$player_height, family="poisson"))

nba.LAL15=nba.LAL[which(nba.LAL$season=="2014-15"),]
x.LAL15reb<-summary(m1 <- glm(nba.LAL15$reb ~ nba.LAL15$player_height, family="poisson"))
x.LAL15pts<-summary(m1 <- glm(nba.LAL15$pts ~ nba.LAL15$player_height, family="poisson"))

nba.LAL16=nba.LAL[which(nba.LAL$season=="2015-16"),]
x.LAL16reb<-summary(m1 <- glm(nba.LAL16$reb ~ nba.LAL16$player_height, family="poisson"))
x.LAL16pts<-summary(m1 <- glm(nba.LAL16$pts ~ nba.LAL16$player_height, family="poisson"))

nba.LAL17=nba.LAL[which(nba.LAL$season=="2016-17"),]
x.LAL17reb<-summary(m1 <- glm(nba.LAL17$reb ~ nba.LAL17$player_height, family="poisson"))
x.LAL17pts<-summary(m1 <- glm(nba.LAL17$pts ~ nba.LAL17$player_height, family="poisson"))

nba.LAL18=nba.LAL[which(nba.LAL$season=="2017-18"),]
x.LAL18reb<-summary(m1 <- glm(nba.LAL18$reb ~ nba.LAL18$player_height, family="poisson"))
x.LAL18pts<-summary(m1 <- glm(nba.LAL18$pts ~ nba.LAL18$player_height, family="poisson"))

nba.LAL19=nba.LAL[which(nba.LAL$season=="2018-19"),]
x.LAL19reb<-summary(m1 <- glm(nba.LAL19$reb ~ nba.LAL19$player_height, family="poisson"))
x.LAL19pts<-summary(m1 <- glm(nba.LAL19$pts ~ nba.LAL19$player_height, family="poisson"))

nba.LAL20=nba.LAL[which(nba.LAL$season=="2019-20"),]
x.LAL20reb<-summary(m1 <- glm(nba.LAL20$reb ~ nba.LAL20$player_height, family="poisson"))
x.LAL20pts<-summary(m1 <- glm(nba.LAL20$pts ~ nba.LAL20$player_height, family="poisson"))

nba.LAL21=nba.LAL[which(nba.LAL$season=="2020-21"),]
x.LAL21reb<-summary(m1 <- glm(nba.LAL21$reb ~ nba.LAL21$player_height, family="poisson"))
x.LAL21pts<-summary(m1 <- glm(nba.LAL21$pts ~ nba.LAL21$player_height, family="poisson"))


x.LAL97reb$coefficients
x.LAL98reb$coefficients
x.LAL99reb$coefficients
x.LAL00reb$coefficients
x.LAL01reb$coefficients
x.LAL02reb$coefficients
x.LAL03reb$coefficients
x.LAL04reb$coefficients
x.LAL05reb$coefficients
x.LAL06reb$coefficients
x.LAL07reb$coefficients
x.LAL08reb$coefficients
x.LAL09reb$coefficients
x.LAL10reb$coefficients
x.LAL11reb$coefficients
x.LAL12reb$coefficients
x.LAL13reb$coefficients
x.LAL14reb$coefficients
x.LAL15reb$coefficients
x.LAL16reb$coefficients
x.LAL17reb$coefficients
x.LAL18reb$coefficients
x.LAL19reb$coefficients
x.LAL20reb$coefficients
x.LAL21reb$coefficients


x.LAL97pts$coefficients
x.LAL98pts$coefficients
x.LAL99pts$coefficients
x.LAL00pts$coefficients
x.LAL01pts$coefficients
x.LAL02pts$coefficients
x.LAL03pts$coefficients
x.LAL04pts$coefficients
x.LAL05pts$coefficients
x.LAL06pts$coefficients
x.LAL07pts$coefficients
x.LAL08pts$coefficients
x.LAL09pts$coefficients
x.LAL10pts$coefficients
x.LAL11pts$coefficients
x.LAL12pts$coefficients
x.LAL13pts$coefficients
x.LAL14pts$coefficients
x.LAL15pts$coefficients
x.LAL16pts$coefficients
x.LAL17pts$coefficients
x.LAL18pts$coefficients
x.LAL19pts$coefficients
x.LAL20pts$coefficients
x.LAL21pts$coefficients

#Below is future and 
#######################
##  Countries for one season##
#######################

nba.season10=nba[which(nba$season=="2009-10"),]
summary(nba.season10$country)
nba.us=nba[which(nba$country=="USA"),]
summary(nba.us$season)

nba.us10=nba.season10[which(nba.season10$country=="USA"),]
(x.usreb<-summary(m1 <- glm(nba.us10$reb ~ nba.us10$player_height, family="poisson")))
(x.uspts<-summary(m1 <- glm(nba.us10$pts ~ nba.us10$player_height, family="poisson")))


##################################################################
#Chapter 1 continued#
################################################################


nba = read.csv("~/math/data/all_seasons.csv")
attach(nba)



##################################
#Using GAM with data split by era#
##################################

#Data is from 96-97 to 21-22
#Era 1: 97-2004

#Era 2: 05-2013

#Era 3: 14-22

#########
##ERA 1##
#########
str(nba)
nba97= nba[which((nba$season=="1996-97")),]
nba98= nba[which((nba$season=="1997-98")),]
nba99= nba[which((nba$season=="1998-99")),]
nba00= nba[which((nba$season=="1999-00")),]
nba01= nba[which((nba$season=="2000-01")),]
nba02= nba[which((nba$season=="2001-02")),]
nba03= nba[which((nba$season=="2002-03")),]
nba04= nba[which((nba$season=="2003-04")),]

nba.era1 <- rbind(nba97,nba98,nba99,nba00,nba01,nba02,nba03,nba04)
#View(nba.era1)

p<-aggregate(x=nba.era1$pts,by=list(nba.era1$team_abbreviation ),FUN=mean)
r<-aggregate(x=nba.era1$reb,by=list(nba.era1$team_abbreviation ),FUN=mean)
t<-aggregate(x=nba.era1$ts_pct,by=list(nba.era1$team_abbreviation ),FUN=mean)
u<-aggregate(x=nba.era1$usg_pct,by=list(nba.era1$team_abbreviation ),FUN=mean)

p <- p[order(p$x,decreasing=T),]
r <- r[order(r$x,decreasing=T),]
t <- t[order(t$x,decreasing=T),]
u <- u[order(u$x,decreasing=T),]


e1.p= nba

#########
##ERA 2##
#########

nba05= nba[which((nba$season=="2004-05")),]
nba06= nba[which((nba$season=="2005-06")),]
nba07= nba[which((nba$season=="2006-07")),]
nba08= nba[which((nba$season=="2007-08")),]
nba09= nba[which((nba$season=="2008-09")),]
nba10= nba[which((nba$season=="2009-10")),]
nba11= nba[which((nba$season=="2010-11")),]
nba12= nba[which((nba$season=="2011-12")),]
nba13= nba[which((nba$season=="2012-13")),]

nba.era2 <- rbind(nba05,nba06,nba07,nba08,nba09,nba10,nba11,nba12,nba13)
#View(nba.era2)

p<-aggregate(x=nba.era2$pts,by=list(nba.era2$team_abbreviation ),FUN=mean)
r<-aggregate(x=nba.era2$reb,by=list(nba.era2$team_abbreviation ),FUN=mean)
t<-aggregate(x=nba.era2$ts_pct,by=list(nba.era2$team_abbreviation ),FUN=mean)
u<-aggregate(x=nba.era2$usg_pct,by=list(nba.era2$team_abbreviation ),FUN=mean)

p <- p[order(p$x,decreasing=T),]
r <- r[order(r$x,decreasing=T),]
t <- t[order(t$x,decreasing=T),]
u <- u[order(u$x,decreasing=T),]

data.frame(p,r,t,u)

#########
##ERA 3##
#########

nba14= nba[which((nba$season=="2013-14")),]
nba15= nba[which((nba$season=="2014-15")),]
nba16= nba[which((nba$season=="2015-16")),]
nba17= nba[which((nba$season=="2016-17")),]
nba18= nba[which((nba$season=="2017-18")),]
nba19= nba[which((nba$season=="2018-19")),]
nba20= nba[which((nba$season=="2019-20")),]
nba21= nba[which((nba$season=="2020-21")),]
nba22= nba[which((nba$season=="2021-22")),]

nba.era3 <- rbind(nba14,nba15,nba16,nba17,nba18,nba19,nba20,nba21,nba22)

#View(nba.era3)

p<-aggregate(x=nba.era3$pts,by=list(nba.era3$team_abbreviation ),FUN=mean)
r<-aggregate(x=nba.era3$reb,by=list(nba.era3$team_abbreviation ),FUN=mean)
t<-aggregate(x=nba.era3$ts_pct,by=list(nba.era3$team_abbreviation ),FUN=mean)
u<-aggregate(x=nba.era3$usg_pct,by=list(nba.era3$team_abbreviation ),FUN=mean)

p <- p[order(p$x,decreasing=T),]
r <- r[order(r$x,decreasing=T),]
t <- t[order(t$x,decreasing=T),]
u <- u[order(u$x,decreasing=T),]

data.frame(p,r,t,u)

####################################################################################
#Add variable for win shares
#Multiple variable analysis
#Decision Tree/Random Forrest

####################################################################################
#Era comparison
library(mgcv)
#Points
plot(gam(nba.era1$pts ~ s(nba.era1$player_height), method="REML"))
#ERA 1 seems to have an even distribution in average points, with the middle of heights
#having the highest

plot(gam(nba.era2$pts ~ s(nba.era2$player_height), method="REML"))
#ERA 2, the big men take a big jump. 
#Here we can see that the taller players wwere averaging higher than the others

plot(gam(nba.era3$pts ~ s(nba.era3$player_height), method="REML"))
#ERA 3, the big man take a big dip here
#After leading in the previuos era, bigs name have the worst points among the height

plot(gam(nba.era1$pts ~ s(nba.era1$age), method="REML"))
plot(gam(nba.era2$pts ~ s(nba.era2$age), method="REML"))
plot(gam(nba.era3$pts ~ s(nba.era3$age), method="REML"))

plot(gam(nba.era1$pts ~ s(nba.era1$net_rating), method="REML"))
plot(gam(nba.era2$pts ~ s(nba.era2$net_rating), method="REML"))
plot(gam(nba.era3$pts ~ s(nba.era3$net_rating), method="REML"))

#TRUE SHOOTING
plot(gam(nba.era1$ts_pct ~ s(nba.era1$player_height), method="REML"))
#ERA 1 seems to show that the middle height have the advantage in true shooting

plot(gam(nba.era2$ts_pct ~ s(nba.era2$player_height), method="REML"))
#ERA 2, I am not sure how to analyze this graph
#But the big men seem to be in highest for true shooting

plot(gam(nba.era3$ts_pct ~ s(nba.era3$player_height), method="REML"))
#ERA 3, here the big men take the advantage

plot(gam(nba.era1$ts_pct ~ s(nba.era1$age), method="REML"))
plot(gam(nba.era2$ts_pct ~ s(nba.era2$age), method="REML"))
plot(gam(nba.era3$ts_pct ~ s(nba.era3$age), method="REML"))

#Usage percentage
plot(gam(nba.era1$usg_pct ~ s(nba.era1$player_height), method="REML"))
#ERA 1, in usage the big men were the primary players being used in this era

plot(gam(nba.era2$usg_pct ~ s(nba.era2$player_height), method="REML"))
#ERA 2, here we can see the smallest players taking a big leap
#Although the big men are still high, the small players are right with them.

plot(gam(nba.era3$usg_pct ~ s(nba.era3$player_height), method="REML"))
#ERA 3, similar to era 2 the big men and the smallest players the primary used players in this era.

plot(gam(nba.era1$usg_pct ~ s(nba.era1$age), method="REML"))
plot(gam(nba.era2$usg_pct ~ s(nba.era2$age), method="REML"))
plot(gam(nba.era3$usg_pct ~ s(nba.era3$age), method="REML"))

plot(gam(nba.era1$usg_pct ~ s(nba.era1$net_rating), method="REML"))
plot(gam(nba.era2$usg_pct ~ s(nba.era2$net_rating), method="REML"))
plot(gam(nba.era3$usg_pct ~ s(nba.era3$net_rating), method="REML"))

#GP

plot(gam(nba.era1$gp ~ s(nba.era1$age), method="REML"))
plot(gam(nba.era2$gp ~ s(nba.era2$age), method="REML"))
plot(gam(nba.era3$gp ~ s(nba.era3$age), method="REML"))

plot(gam(nba.era1$gp ~ s(nba.era1$player_height), method="REML"))
plot(gam(nba.era2$gp ~ s(nba.era2$player_height), method="REML"))
plot(gam(nba.era3$gp ~ s(nba.era3$player_height), method="REML"))


##########################
#Los Angeles Lakers
##########################

#The lakers during this time were on the come up, they had just drafted
#a young promising player in kobe bryant and they had traded for a dominant big man in shaquille o'neil.
#The lakers as a team were also dominant when shaq and kobe were playing their best.
#between the 97-2004 season, the lakers had a winning perceentage of 69.7%
#This was this was 5% higher than the runner up and 6% higher than the spurs, who we will look at later

#TEAM WINNIGN % PER ERA
#ERA 1: .697
#ERA 2: .614
#ERA 3: .404


nba.e1.lal=nba.era1[which((nba.era1$team_abbreviation=="LAL")),]
nba.e3.lal=nba.era3[which((nba.era3$team_abbreviation=="LAL")),]

#POINTS
plot(gam(nba.e1.lal$pts ~ s(nba.e1.lal$player_height), method="REML"))
#ERA 1, here we can see that for the lakers the big men were doing the most scoring
#This lines up with shaq being the dominant player on the team

plot(gam(nba.e2.lal$pts ~ s(nba.e2.lal$player_height), method="REML"))
#ERA 2, here the big men take a massive dip.
#This makes sense since during this time shaq has left and the time was led by a smaller player in kobe

plot(gam(nba.e3.lal$pts ~ s(nba.e3.lal$player_height), method="REML"))
#ERA 3, ALthough the big men are still low
#the points continue to rise towrds taller players, this is due to kobe retiring halfway through this era

#True Shooting

plot(gam(nba.e1.lal$ts_pct ~ s(nba.e1.lal$player_height), method="REML"))
#ERA 1, Here we see similar to points, because shaq is the main player he will have the higher shooting percentage

plot(gam(nba.e2.lal$ts_pct ~ s(nba.e2.lal$player_height), method="REML"))
#ERA 2, Here we can see that the smallest player taking a piece of the top, with the big men dipping a bit

plot(gam(nba.e3.lal$ts_pct ~ s(nba.e3.lal$player_height), method="REML"))
#ERA 3, I do not know how to analze this graph
#But from what i can tell, it seems the taller you are the higher percentage


#USG
plot(gam(nba.e1.lal$usg_pct ~ s(nba.e1.lal$player_height), method="REML"))
#ERA 1, Same as previuos, since shaq is the best player on the team during this time
#He will be the player used the most

plot(gam(nba.e2.lal$usg_pct ~ s(nba.e2.lal$player_height), method="REML"))
#ERA 2,

plot(gam(nba.e3.lal$usg_pct ~ s(nba.e3.lal$player_height), method="REML"))
#ERA 3,

############################
#San Antonio Spurs
############################

#The spurs were heading into one of their worst seasons in history, in 96-97, due to their star big, david robinson, man getting injured.
#But this led them to drafting what would become another dominant big man in tim duncan.
#During this period the Spurs had a winning percentage of 63.9%, only behind Utah and L.A.
#They also had their share of winning championship, winning 2 in this span.

#TEAM WINNIGN % PER ERA
#ERA 1: .639
#ERA 2: .705
#ERA 3: .612

nba.e1.sas=nba.era1[which((nba.era1$team_abbreviation=="SAS")),]
nba.e2.sas=nba.era2[which((nba.era2$team_abbreviation=="SAS" & nba.era2$ts_pct > 0 & nba.era2$ts_pct < 1)),]
nba.e3.sas=nba.era3[which((nba.era3$team_abbreviation=="SAS")),]
nba.e1.sas.2=nba.e1.sas[which((nba.e1.sas$pts == ceiling(nba.e1.sas$pts) & nba.e1.sas$reb == ceiling(nba.e1.sas$reb))),]

#POINTS
plot(gam(nba.e1.sas$pts ~ s(nba.e1.sas$player_height), method="REML"))

plot(gam(nba.e2.sas$pts ~ s(nba.e2.sas$player_height), method="REML"))

plot(gam(nba.e3.sas$pts ~ s(nba.e3.sas$player_height), method="REML"))

#TRUE SHOOTING
plot(gam(nba.e1.sas$ts_pct ~ s(nba.e1.sas$player_height), method="REML"))

plot(gam(nba.e2.sas$ts_pct ~ s(nba.e2.sas$player_height), method="REML"))

plot(gam(nba.e3.sas$ts_pct ~ s(nba.e3.sas$player_height), method="REML"))

#USAGE
plot(gam(nba.e1.sas$usg_pct ~ s(nba.e1.sas$player_height), method="REML"))

plot(gam(nba.e2.sas$usg_pct ~ s(nba.e2.sas$player_height), method="REML"))

plot(gam(nba.e3.sas$usg_pct ~ s(nba.e3.sas$player_height), method="REML"))

#########################
#Golden State Warriors
#########################

#For most the time during this time span, the warriors were never considered a good until around ERA 3
#Wasn't until the warriors got Stephen curry in 2009 draft that the warriors would start their turn around that amass to 3 championships in 2015, 2017, and 2018.

#TEAM WINNIGN % PER ERA
#ERA 1: .324
#ERA 2: .442	
#ERA 3: .675

nba.e1.gsw=nba.era1[which((nba.era1$team_abbreviation=="GSW" & nba.era1$ts_pct > 0)),]
nba.e2.gsw=nba.era2[which((nba.era2$team_abbreviation=="GSW" & nba.era2$ts_pct > 0)),]
nba.e3.gsw=nba.era3[which((nba.era3$team_abbreviation=="GSW" & nba.era3$ts_pct > 0)),]

#POINTS
plot(gam(nba.e1.gsw$pts ~ s(nba.e1.gsw$player_height), method="REML"))

plot(gam(nba.e2.gsw$pts ~ s(nba.e2.gsw$player_height), method="REML"))

plot(gam(nba.e3.gsw$pts ~ s(nba.e3.gsw$player_height), method="REML"))

#TRUE SHOOTING
plot(gam(nba.e1.gsw$ts_pct ~ s(nba.e1.gsw$player_height), method="REML"))

plot(gam(nba.e2.gsw$ts_pct ~ s(nba.e2.gsw$player_height), method="REML"))

plot(gam(nba.e3.gsw$ts_pct ~ s(nba.e3.gsw$player_height), method="REML"))

#USAGE
plot(gam(nba.e1.gsw$usg_pct ~ s(nba.e1.gsw$player_height), method="REML"))

plot(gam(nba.e2.gsw$usg_pct ~ s(nba.e2.gsw$player_height), method="REML"))

plot(gam(nba.e3.gsw$usg_pct ~ s(nba.e3.gsw$player_height), method="REML"))

########################
#GAM EDA
########################

#Continent

library(ggplot2)
library(dplyr)
library(countrycode)

Continent.e1=countrycode(nba.era1$country, origin = 'country.name', destination = 'continent')
nba.cont.e1=data.frame(nba.era1,Continent.e1)

Continent.e2=countrycode(nba.era2$country, origin = 'country.name', destination = 'continent')
nba.cont.e2=data.frame(nba.era2,Continent.e2)

Continent.e3=countrycode(nba.era3$country, origin = 'country.name', destination = 'continent')
nba.cont.e3=data.frame(nba.era3,Continent.e3)


######################################################################
#AFRICA
nba.e1.afr=nba.cont.e1[which((nba.cont.e1$Continent.e1=="Africa")),]
nba.e2.afr=nba.cont.e2[which((nba.cont.e2$Continent.e2=="Africa")),]
nba.e3.afr=nba.cont.e3[which((nba.cont.e3$Continent.e3=="Africa")),]

#POINTS
plot(gam(nba.e1.afr$pts ~ s(nba.e1.afr$player_height), method="REML"))
plot(gam(nba.e2.afr$pts ~ s(nba.e2.afr$player_height), method="REML"))
plot(gam(nba.e3.afr$pts ~ s(nba.e3.afr$player_height), method="REML"))

#TRUE SHOOTING
plot(gam(nba.e1.afr$ts_pct ~ s(nba.e1.afr$player_height), method="REML"))
plot(gam(nba.e2.afr$ts_pct ~ s(nba.e2.afr$player_height), method="REML"))
plot(gam(nba.e3.afr$ts_pct ~ s(nba.e3.afr$player_height), method="REML"))

#USAGE
plot(gam(nba.e1.afr$usg_pct ~ s(nba.e1.afr$player_height), method="REML"))
plot(gam(nba.e2.afr$usg_pct ~ s(nba.e2.afr$player_height), method="REML"))
plot(gam(nba.e3.afr$usg_pct ~ s(nba.e3.afr$player_height), method="REML"))

#################################################################################
#AMERICAS
nba.e1.am=nba.cont.e1[which((nba.cont.e1$Continent.e1=="Americas")),]
nba.e2.am=nba.cont.e2[which((nba.cont.e2$Continent.e2=="Americas")),]
nba.e3.am=nba.cont.e3[which((nba.cont.e3$Continent.e3=="Americas")),]

#POINTS
plot(gam(nba.e1.am$pts ~ s(nba.e1.am$player_height), method="REML"))
plot(gam(nba.e2.am$pts ~ s(nba.e2.am$player_height), method="REML"))
plot(gam(nba.e3.am$pts ~ s(nba.e3.am$player_height), method="REML"))

#TRUE SHOOTING
plot(gam(nba.e1.am$ts_pct ~ s(nba.e1.am$player_height), method="REML"))
plot(gam(nba.e2.am$ts_pct ~ s(nba.e2.am$player_height), method="REML"))
plot(gam(nba.e3.am$ts_pct ~ s(nba.e3.am$player_height), method="REML"))

#USAGE
plot(gam(nba.e1.am$usg_pct ~ s(nba.e1.am$player_height), method="REML"))
plot(gam(nba.e2.am$usg_pct ~ s(nba.e2.am$player_height), method="REML"))
plot(gam(nba.e3.am$usg_pct ~ s(nba.e3.am$player_height), method="REML"))
#####################################################################
#ASIA
#######################################################################
#EUROPE
nba.e1.eu=nba.cont.e1[which((nba.cont.e1$Continent.e1=="Americas")),]
nba.e2.eu=nba.cont.e2[which((nba.cont.e2$Continent.e2=="Americas")),]
nba.e3.eu=nba.cont.e3[which((nba.cont.e3$Continent.e3=="Americas")),]

#POINTS
plot(gam(nba.e1.eu$pts ~ s(nba.e1.eu$player_height), method="REML"))
plot(gam(nba.e2.eu$pts ~ s(nba.e2.eu$player_height), method="REML"))
plot(gam(nba.e3.eu$pts ~ s(nba.e3.eu$player_height), method="REML"))

#TRUE SHOOTING
plot(gam(nba.e1.eu$ts_pct ~ s(nba.e1.eu$player_height), method="REML"))
plot(gam(nba.e2.eu$ts_pct ~ s(nba.e2.eu$player_height), method="REML"))
plot(gam(nba.e3.eu$ts_pct ~ s(nba.e3.eu$player_height), method="REML"))

#USAGE
plot(gam(nba.e1.eu$usg_pct ~ s(nba.e1.eu$player_height), method="REML"))
plot(gam(nba.e2.eu$usg_pct ~ s(nba.e2.eu$player_height), method="REML"))
plot(gam(nba.e3.eu$usg_pct ~ s(nba.e3.eu$player_height), method="REML"))
#########################################################################
#OCEANIA

########################################################################
#Lottery

nba.e1.lot1=nba.era1[which((nba.era1$draft_number=="1")),]
nba.e1.lot2=nba.era1[which((nba.era1$draft_number=="2")),]
nba.e1.lot3=nba.era1[which((nba.era1$draft_number=="3")),]

nba.e2.afr=nba.era2[which((nba.era2$Continent.e2=="Africa")),]
nba.e3.afr=nba.era3[which((nba.era3$Continent.e3=="Africa")),]

plot(gam(nba.e1.lot1$usg_pct ~ s(nba.e1.lot1$pts), method="REML"))
plot(gam(nba.e1.lot2$usg_pct ~ s(nba.e1.lot2$pts), method="REML"))
plot(gam(nba.e1.lot3$usg_pct ~ s(nba.e1.lot3$pts), method="REML"))

plot(aggregate(x=nba$age,by=list(nba$draft_year),FUN=mean))

###############################################
#End of chapter 1
###############################################



