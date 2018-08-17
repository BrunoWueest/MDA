##### Media-driven accountability
# draft template for book manuscript-analysis
# bw, 11.4.2018, ms 27.04.2018

# clear workspace
rm(list = ls())

# set working directory
setwd("/Users/noerber/Desktop/PhD/MDA/Data/Media-Data/")
getwd()

### PREPARATION ###
#------------------

#load corpus
input <- readRDS("MDA_corpus-100418.RData")

head(input)
unique(input$Entity_Name)

#get only the metropolitanorganizations
sampl <- c("grand lyon",
           "greater london authority (gla)",
           "verband region stuttgart (vrs)",
           "libero",
           "syndicat des transports d'ile-de-france (stif)",
           "syndicat mixte des transports pour le rhÃ´ne et l'agglomÃ©ration lyonnaise (sytral)",
           "transport for london",
           "verkehrs- und tarifverbund stuttgart (vvs)",
           "verkehrsverbund berlin-brandenburg (vbb)",
           "west midlands integrated transport authority (wmita)",
           "zÃ¼rcher verkehrsverbund (zvv)")

corpus <- input[input$Entity_Name %in% sampl,]

#recode several independent variables (basically: generate dummies from character vectors of interest)
vars <- c("Entity_Name",
          "Media_Type",
          "Media_Country",
          "Media_Source",
          "Tonality_Verbalized",
          "actor.type",
          "policy.scope"
#          "territorial.scope",
#          "policy.output"
)

for (var in vars){
  corpus[,c(var)] <- factor(corpus[,c(var)])
  tmp_vars <- as.data.frame(model.matrix(~ corpus[,c(var)] - 1))
  tmp_vars[,c("(Intercept)")] <- NULL
  colnames(tmp_vars) <- gsub("corpus[, c(var)]", "", colnames(tmp_vars), fixed = T)
  colnames(tmp_vars) <- paste(var, colnames(tmp_vars), sep = "_")
  corpus <- cbind(corpus, tmp_vars)
}

colnames(corpus) <- tolower(colnames(corpus))

corpus$functional_scope_informative <- corpus$functional.scope..new..informative
corpus$functional_scope_informative[corpus$functional_scope_informative == "yes"] <- "1"
corpus$functional_scope_informative[corpus$functional_scope_informative == "no"] <- "0"
corpus$functional_scope_informative <- as.numeric(corpus$functional_scope_informative)

corpus$functional_scope_implementing <- corpus$functional.scope..new..implementing
corpus$functional_scope_implementing[corpus$functional_scope_implementing == "yes"] <- "1"
corpus$functional_scope_implementing[corpus$functional_scope_implementing == "no"] <- "0"
corpus$functional_scope_implementing <- as.numeric(corpus$functional_scope_implementing)

corpus$functional_scope_decisive <- corpus$functional.scope..new..decisive
corpus$functional_scope_decisive[corpus$functional_scope_decisive == "yes"] <- "1"
corpus$functional_scope_decisive[corpus$functional_scope_decisive == "no"] <- "0"
corpus$functional_scope_decisive <- as.numeric(corpus$functional_scope_decisive)

corpus <- corpus[order(corpus$entity_id),]

unique(corpus$entity_id[corpus$`entity_name_zã¼rcher verkehrsverbund (zvv)`==1],)

#Entity-IDs
#33=Grand Lyon
#34=Greater London Authority
#59=Libero
#71=STIF
#72=SYTRAL
#73=Transport for London
#84=Verband Region Stuttgart
#85=Verband und Tarifverbund Stuttgart
#86=Verkehrsverbund Berlin Brandenburg
#87=West Midlands Integrated Transport Authority
#93=ZVV

### DESCRIPTIVE STATISTICS ###
#-----------------------------

### general coverage (salience over all entities)

# aggregate number of articles per week (or day, or month? and weight by number of words? if yes, how?)
min(corpus$article_date)
max(corpus$article_date)

require(lubridate)
days <- data.frame(days = seq(as.Date("2005-01-11"), as.Date("2015-08-20"), by="days")) #the period for which articles are available for the three entities
days$weeks <- week(days$days)
days$years <- year(days$days)

corpus$article_date <- as.Date(corpus$article_date)
corpus$n <- 1

# take the logged number of words for the weighted aggregation (?!)
hist(corpus$article_word_count)
corpus$n_w <- log1p(corpus$article_word_count) 
hist(corpus$n_w)
dat_base <- merge(corpus, days, by.x = "article_date", by.y = "days", all = T)

#add zeros for days without coverage
dat_base$n[is.na(dat_base$n)] <- 0
dat_base$n_w[is.na(dat_base$n_w)] <- 0

# get n of articles per week and n of articles weighted by article length per week
require(plyr)
dat <- ddply(dat_base, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat <- dat[order(dat$year, dat$weeks),]

# there is no article after week 35 of 2015, so we drop the observations after this week
dat <- dat[!(dat$year == 2015 & dat$weeks > 34),]
# week 53 is odd as well, so lets get rid of it as well
dat <- dat[dat$week != 53,]
# there is one row without date, remove
dat <- dat[!is.na(dat$weeks),]

#do we have censored data?
hist(dat$n, breaks = 100)
hist(dat$n_w, breaks = 100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat$n)
adf.test(dat$n_w)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat$n) # monthly seasonality (seas = 4; monthly); Metro organization: number is 10 --> 2.5 months?
findfrequency(dat$n_w) # monthly seasonality (seas = 4; monthly); Metro organization: number is 10 --> 2.5 months?

#Build a time series
dat$n_ts <- ts(dat$n)
dat$n_w_ts <- ts(dat$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat$n_ts_sm <- sma(dat$n_ts, order = 4)$fitted
dat$n_w_ts_sm <- sma(dat$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat$week <- as.Date(paste(dat$years, dat$weeks, 1, sep="-"), "%Y-%U-%u")
plot(dat$week, dat$n_ts_sm, type = "l")
plot(dat$week, dat$n_w_ts_sm, type = "l")

# illustrate peaks and slumps
#All
pdf(file=paste("./Results/Graphs/timeline.pdf", sep=""), paper="special", width=9, height=6)
plot(dat$week, dat$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="Metropolitan Governors")
abline(h = mean(dat$n_ts_sm))
abline(h = mean(dat$n_ts_sm)+sd(dat$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat$n_ts_sm)-sd(dat$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="Metropolitan Governors")
abline(h = mean(dat$n_ts_sm))
abline(h = mean(dat$n_ts_sm)+sd(dat$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat$n_ts_sm)-sd(dat$n_ts_sm), lty = "dashed")
dev.off()
# > here, we will have to qualitatively check special times

dat_high <- dat[dat$n_ts_sm>mean(dat$n_ts_sm)+sd(dat$n_ts_sm),]

#Descriptive Statistics: Independent Variables
table(dat_base$entity_name)
act.typ <- table(dat_base$actor.type)
png(filename=paste("./Results/Graphs/ActorType.png", sep=""), width=9, height=6, units="in", res=600)
barplot(act.typ,main="Actor Type",ylab="Number of Articles")
dev.off()
pol_scop <- table(dat_base$policy.scope)
png(filename=paste("./Results/Graphs/PolicyScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(pol_scop,main="Policy Scope",ylab="Number of Articles")
dev.off()
fun_scop <- table(dat_base$functional.scope..old.)
png(filename=paste("./Results/Graphs/FunctScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(fun_scop,main="Functional Scope",ylab="Number of Articles")
dev.off()

### SALIENCE GRAPHS BY INDIVIDUAL GOVERNOR ###
{
#Grand Lyon
#----------
dat_gly <- dat_base[dat_base$entity_name=="grand lyon",]
dat_gly <- ddply(dat_gly, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_gly <- dat_gly[order(dat_gly$years, dat_gly$weeks),]

# remove the missing values
dat_gly <- dat_gly[!is.na(dat_gly$weeks),]

adf.test(dat_gly$n)
adf.test(dat_gly$n_w)
#stationary time-series

#Build a time series
dat_gly$n_ts <- ts(dat_gly$n)
dat_gly$n_w_ts <- ts(dat_gly$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_gly$n_ts_sm <- sma(dat_gly$n_ts, order = 4)$fitted
dat_gly$n_w_ts_sm <- sma(dat_gly$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_gly$week <- as.Date(paste(dat_gly$years, dat_gly$weeks, 1, sep="-"), "%Y-%U-%u")
plot(dat_gly$week, dat_gly$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",main="Grand Lyon")
plot(dat_gly$week, dat_gly$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",main="Grand Lyon")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_grandlyon.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_gly$week, dat_gly$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",main="Grand Lyon")
abline(h = mean(dat_gly$n_ts_sm))
abline(h = mean(dat_gly$n_ts_sm)+sd(dat_gly$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_gly$n_ts_sm)-sd(dat_gly$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_grandlyon.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_gly$week, dat_gly$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="Grand Lyon")
abline(h = mean(dat_gly$n_ts_sm))
abline(h = mean(dat_gly$n_ts_sm)+sd(dat_gly$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_gly$n_ts_sm)-sd(dat_gly$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_gly$peak <- 0
dat_gly$peak[dat_gly$n_ts_sm>mean(dat_gly$n_ts_sm)+sd(dat_gly$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_gly$entity_id <- 33


#-> bind all the individual metro organizations datasets together and (i.e. staple them)
#-> merge only then with the full dataset

#Remove certain columns from gly-data as there are columns in the other dataframe called like this
dat_gly$n <- NULL
dat_gly$n_w <- NULL

#Greater London Authority
#------------------------
dat_gla <- dat_base[dat_base$entity_name=="greater london authority (gla)",]
dat_gla <- ddply(dat_gla, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_gla <- dat_gla[order(dat_gla$years, dat_gla$weeks),]

# remove the missing values
dat_gla <- dat_gla[!is.na(dat_gla$weeks),]

adf.test(dat_gla$n)
adf.test(dat_gla$n_w)
#stationary time-series

#Build a time series
dat_gla$n_ts <- ts(dat_gla$n)
dat_gla$n_w_ts <- ts(dat_gla$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_gla$n_ts_sm <- sma(dat_gla$n_ts, order = 4)$fitted
dat_gla$n_w_ts_sm <- sma(dat_gla$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_gla$week <- as.Date(paste(dat_gla$years, dat_gla$weeks, 1, sep="-"), "%Y-%U-%u")
dat_gla <- dat_gla[order(dat_gla$week),]
plot(dat_gla$week, dat_gla$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",main="Greater London Authority")
plot(dat_gla$week, dat_gla$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",main="Greater London Authority")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_gla.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_gla$week, dat_gla$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",main="Greater London Authority")
abline(h = mean(dat_gla$n_ts_sm))
abline(h = mean(dat_gla$n_ts_sm)+sd(dat_gla$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_gla$n_ts_sm)-sd(dat_gla$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_gla.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_gla$week, dat_gla$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="Greater London Authority")
abline(h = mean(dat_gla$n_ts_sm))
abline(h = mean(dat_gla$n_ts_sm)+sd(dat_gla$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_gla$n_ts_sm)-sd(dat_gla$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_gla$peak <- 0
dat_gla$peak[dat_gla$n_ts_sm>mean(dat_gla$n_ts_sm)+sd(dat_gla$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_gla$entity_id <- 34

#Remove certain columns from gla-data as there are columns in the other dataframe called like this
dat_gla$n <- NULL
dat_gla$n_w <- NULL


#Libero
#------
dat_libero <- dat_base[dat_base$entity_name=="libero",]
dat_libero <- ddply(dat_libero, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_libero <- dat_libero[order(dat_libero$years, dat_libero$weeks),]

# remove the missing values
dat_libero <- dat_libero[!is.na(dat_libero$weeks),]

adf.test(dat_libero$n)
adf.test(dat_libero$n_w)
#stationary time-series

#Build a time series
dat_libero$n_ts <- ts(dat_libero$n)
dat_libero$n_w_ts <- ts(dat_libero$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_libero$n_ts_sm <- sma(dat_libero$n_ts, order = 4)$fitted
dat_libero$n_w_ts_sm <- sma(dat_libero$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_libero$week <- as.Date(paste(dat_libero$years, dat_libero$weeks, 1, sep="-"), "%Y-%U-%u")
dat_libero <- dat_libero[order(dat_libero$week),]
plot(dat_libero$week, dat_libero$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",main="Libero")
plot(dat_libero$week, dat_libero$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",main="Libero")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_libero.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_libero$week, dat_libero$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",main="Libero")
abline(h = mean(dat_libero$n_ts_sm))
abline(h = mean(dat_libero$n_ts_sm)+sd(dat_libero$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_libero$n_ts_sm)-sd(dat_libero$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_libero.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_libero$week, dat_libero$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="Libero")
abline(h = mean(dat_libero$n_ts_sm))
abline(h = mean(dat_libero$n_ts_sm)+sd(dat_libero$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_libero$n_ts_sm)-sd(dat_libero$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_libero$peak <- 0
dat_libero$peak[dat_libero$n_ts_sm>mean(dat_libero$n_ts_sm)+sd(dat_libero$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_libero$entity_id <- 59

#Remove certain columns from libero-data as there are columns in the other dataframe called like this
dat_libero$n <- NULL
dat_libero$n_w <- NULL


#STIF
#----
dat_stif <- dat_base[dat_base$entity_name=="syndicat des transports d'ile-de-france (stif)",]
dat_stif <- ddply(dat_stif, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_stif <- dat_stif[order(dat_stif$years, dat_stif$weeks),]

# remove the missing values
dat_stif <- dat_stif[!is.na(dat_stif$weeks),]

adf.test(dat_stif$n)
adf.test(dat_stif$n_w)
#stationary time-series

#Build a time series
dat_stif$n_ts <- ts(dat_stif$n)
dat_stif$n_w_ts <- ts(dat_stif$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_stif$n_ts_sm <- sma(dat_stif$n_ts, order = 4)$fitted
dat_stif$n_w_ts_sm <- sma(dat_stif$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_stif$week <- as.Date(paste(dat_stif$years, dat_stif$weeks, 1, sep="-"), "%Y-%U-%u")
dat_stif <- dat_stif[order(dat_stif$week),]
plot(dat_stif$week, dat_stif$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",main="Syndicat des Transports d'Ile-de-France \n (STIF)")
plot(dat_stif$week, dat_stif$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",main="Syndicat des Transports d'Ile-de-France \n (STIF)")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_stif.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_stif$week, dat_stif$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",main="Syndicat des Transports d'Ile-de-France \n (STIF)")
abline(h = mean(dat_stif$n_ts_sm))
abline(h = mean(dat_stif$n_ts_sm)+sd(dat_stif$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_stif$n_ts_sm)-sd(dat_stif$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_stif.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_stif$week, dat_stif$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="Syndicat des Transports d'Ile-de-France \n (STIF)")
abline(h = mean(dat_stif$n_ts_sm))
abline(h = mean(dat_stif$n_ts_sm)+sd(dat_stif$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_stif$n_ts_sm)-sd(dat_stif$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_stif$peak <- 0
dat_stif$peak[dat_stif$n_ts_sm>mean(dat_stif$n_ts_sm)+sd(dat_stif$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_stif$entity_id <- 71


#-> bind all the individual metro organizations datasets together and (i.e. staple them)
#-> merge only then with the full dataset

#Remove certain columns from stif-data as there are columns in the other dataframe called like this
dat_stif$n <- NULL
dat_stif$n_w <- NULL


#SYTRAL
#-----
dat_sytral <- dat_base[dat_base$entity_id==72,]
dat_sytral <- ddply(dat_sytral, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_sytral <- dat_sytral[order(dat_sytral$years, dat_sytral$weeks),]

# remove the missing values
dat_sytral <- dat_sytral[!is.na(dat_sytral$weeks),]

adf.test(dat_sytral$n)
adf.test(dat_sytral$n_w)
#stationary time-series

#Build a time series
dat_sytral$n_ts <- ts(dat_sytral$n)
dat_sytral$n_w_ts <- ts(dat_sytral$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_sytral$n_ts_sm <- sma(dat_sytral$n_ts, order = 4)$fitted
dat_sytral$n_w_ts_sm <- sma(dat_sytral$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_sytral$week <- as.Date(paste(dat_sytral$years, dat_sytral$weeks, 1, sep="-"), "%Y-%U-%u")
dat_sytral <- dat_sytral[order(dat_sytral$week),]
plot(dat_sytral$week, dat_sytral$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",
     main="Syndicat Mixte des Transports \n pour Le Rhône et l'Agglomération Lyonnaise \n (SYTRAL)")
plot(dat_sytral$week, dat_sytral$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",
     main="Syndicat Mixte des Transports \n pour Le Rhône et l'Agglomération Lyonnaise \n (SYTRAL)")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_sytral.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_sytral$week, dat_sytral$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",
     main="Syndicat Mixte des Transports \n pour Le Rhône et l'Agglomération Lyonnaise \n (SYTRAL)")
abline(h = mean(dat_sytral$n_ts_sm))
abline(h = mean(dat_sytral$n_ts_sm)+sd(dat_sytral$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_sytral$n_ts_sm)-sd(dat_sytral$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_sytral.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_sytral$week, dat_sytral$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="Syndicat Mixte des Transports \n pour Le Rhône et l'Agglomération Lyonnaise \n (SYTRAL)")
abline(h = mean(dat_sytral$n_ts_sm))
abline(h = mean(dat_sytral$n_ts_sm)+sd(dat_sytral$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_sytral$n_ts_sm)-sd(dat_sytral$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_sytral$peak <- 0
dat_sytral$peak[dat_sytral$n_ts_sm>mean(dat_sytral$n_ts_sm)+sd(dat_sytral$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_sytral$entity_id <- 72


#-> bind all the individual metro organizations datasets together and (i.e. staple them)
#-> merge only then with the full dataset

#Remove certain columns from sytral-data as there are columns in the other dataframe called like this
dat_sytral$n <- NULL
dat_sytral$n_w <- NULL


#Transport for London
#--------------------
dat_tfl <- dat_base[dat_base$entity_id==73,]
dat_tfl <- ddply(dat_tfl, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_tfl <- dat_tfl[order(dat_tfl$years, dat_tfl$weeks),]

# remove the missing values
dat_tfl <- dat_tfl[!is.na(dat_tfl$weeks),]

adf.test(dat_tfl$n)
adf.test(dat_tfl$n_w)
#stationary time-series

#Build a time series
dat_tfl$n_ts <- ts(dat_tfl$n)
dat_tfl$n_w_ts <- ts(dat_tfl$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_tfl$n_ts_sm <- sma(dat_tfl$n_ts, order = 4)$fitted
dat_tfl$n_w_ts_sm <- sma(dat_tfl$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_tfl$week <- as.Date(paste(dat_tfl$years, dat_tfl$weeks, 1, sep="-"), "%Y-%U-%u")
dat_tfl <- dat_tfl[order(dat_tfl$week),]
plot(dat_tfl$week, dat_tfl$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",
     main="Transport for London")
plot(dat_tfl$week, dat_tfl$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",
     main="Transport for London")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_tfl.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_tfl$week, dat_tfl$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",
     main="Transport for London")
abline(h = mean(dat_tfl$n_ts_sm))
abline(h = mean(dat_tfl$n_ts_sm)+sd(dat_tfl$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_tfl$n_ts_sm)-sd(dat_tfl$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_tfl.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_tfl$week, dat_tfl$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="Transport for London")
abline(h = mean(dat_tfl$n_ts_sm))
abline(h = mean(dat_tfl$n_ts_sm)+sd(dat_tfl$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_tfl$n_ts_sm)-sd(dat_tfl$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_tfl$peak <- 0
dat_tfl$peak[dat_tfl$n_ts_sm>mean(dat_tfl$n_ts_sm)+sd(dat_tfl$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_tfl$entity_id <- 73

#Remove certain columns from tfl-data as there are columns in the other dataframe called like this
dat_tfl$n <- NULL
dat_tfl$n_w <- NULL


#Verband Region Stuttgart
#------------------------
dat_vrs <- dat_base[dat_base$entity_id==84,]
dat_vrs <- ddply(dat_vrs, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_vrs <- dat_vrs[order(dat_vrs$years, dat_vrs$weeks),]

# remove the missing values
dat_vrs <- dat_vrs[!is.na(dat_vrs$weeks),]

adf.test(dat_vrs$n)
adf.test(dat_vrs$n_w)
#stationary time-series

#Build a time series
dat_vrs$n_ts <- ts(dat_vrs$n)
dat_vrs$n_w_ts <- ts(dat_vrs$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_vrs$n_ts_sm <- sma(dat_vrs$n_ts, order = 4)$fitted
dat_vrs$n_w_ts_sm <- sma(dat_vrs$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_vrs$week <- as.Date(paste(dat_vrs$years, dat_vrs$weeks, 1, sep="-"), "%Y-%U-%u")
dat_vrs <- dat_vrs[order(dat_vrs$week),]
plot(dat_vrs$week, dat_vrs$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",
     main="Verband Region Stuttgart")
plot(dat_vrs$week, dat_vrs$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",
     main="Verband Region Stuttgart")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_vrs.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_vrs$week, dat_vrs$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",
     main="Verband Region Stuttgart")
abline(h = mean(dat_vrs$n_ts_sm))
abline(h = mean(dat_vrs$n_ts_sm)+sd(dat_vrs$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_vrs$n_ts_sm)-sd(dat_vrs$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_vrs.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_vrs$week, dat_vrs$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="Verband Region Stuttgart")
abline(h = mean(dat_vrs$n_ts_sm))
abline(h = mean(dat_vrs$n_ts_sm)+sd(dat_vrs$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_vrs$n_ts_sm)-sd(dat_vrs$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_vrs$peak <- 0
dat_vrs$peak[dat_vrs$n_ts_sm>mean(dat_vrs$n_ts_sm)+sd(dat_vrs$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_vrs$entity_id <- 84

#Remove certain columns from vrs-data as there are columns in the other dataframe called like this
dat_vrs$n <- NULL
dat_vrs$n_w <- NULL


#Verkehrs- und Tarifverbund Stuttgart
#------------------------------------
dat_vvs <- dat_base[dat_base$entity_id==85,]
dat_vvs <- ddply(dat_vvs, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_vvs <- dat_vvs[order(dat_vvs$years, dat_vvs$weeks),]

# remove the missing values
dat_vvs <- dat_vvs[!is.na(dat_vvs$weeks),]

adf.test(dat_vvs$n)
adf.test(dat_vvs$n_w)
#stationary time-series

#Build a time series
dat_vvs$n_ts <- ts(dat_vvs$n)
dat_vvs$n_w_ts <- ts(dat_vvs$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_vvs$n_ts_sm <- sma(dat_vvs$n_ts, order = 4)$fitted
dat_vvs$n_w_ts_sm <- sma(dat_vvs$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_vvs$week <- as.Date(paste(dat_vvs$years, dat_vvs$weeks, 1, sep="-"), "%Y-%U-%u")
dat_vvs <- dat_vvs[order(dat_vvs$week),]
plot(dat_vvs$week, dat_vvs$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",
     main="Verkehrs- und Tarifverbund Stuttgart \n (VVS)")
plot(dat_vvs$week, dat_vvs$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",
     main="Verkehrs- und Tarifverbund Stuttgart \n (VVS)")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_vvs.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_vvs$week, dat_vvs$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",
     main="Verkehrs- und Tarifverbund Stuttgart \n (VVS)")
abline(h = mean(dat_vvs$n_ts_sm))
abline(h = mean(dat_vvs$n_ts_sm)+sd(dat_vvs$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_vvs$n_ts_sm)-sd(dat_vvs$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_vvs.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_vvs$week, dat_vvs$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="Verkehrs- und Tarifverbund Stuttgart \n (VVS)")
abline(h = mean(dat_vvs$n_ts_sm))
abline(h = mean(dat_vvs$n_ts_sm)+sd(dat_vvs$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_vvs$n_ts_sm)-sd(dat_vvs$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_vvs$peak <- 0
dat_vvs$peak[dat_vvs$n_ts_sm>mean(dat_vvs$n_ts_sm)+sd(dat_vvs$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_vvs$entity_id <- 85

#Remove certain columns from vvs-data as there are columns in the other dataframe called like this
dat_vvs$n <- NULL
dat_vvs$n_w <- NULL


#Verkehrsverbund Berlin-Brandenburg
#----------------------------------
dat_vbb <- dat_base[dat_base$entity_id==86,]
dat_vbb <- ddply(dat_vbb, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_vbb <- dat_vbb[order(dat_vbb$years, dat_vbb$weeks),]

# remove the missing values
dat_vbb <- dat_vbb[!is.na(dat_vbb$weeks),]

adf.test(dat_vbb$n)
adf.test(dat_vbb$n_w)
#stationary time-series

#Build a time series
dat_vbb$n_ts <- ts(dat_vbb$n)
dat_vbb$n_w_ts <- ts(dat_vbb$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_vbb$n_ts_sm <- sma(dat_vbb$n_ts, order = 4)$fitted
dat_vbb$n_w_ts_sm <- sma(dat_vbb$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_vbb$week <- as.Date(paste(dat_vbb$years, dat_vbb$weeks, 1, sep="-"), "%Y-%U-%u")
dat_vbb <- dat_vbb[order(dat_vbb$week),]
plot(dat_vbb$week, dat_vbb$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",
     main="Verkehrsverbund Berlin-Brandenburg \n (VBB)")
plot(dat_vbb$week, dat_vbb$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",
     main="Verkehrsverbund Berlin-Brandenburg \n (VBB)")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_vbb.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_vbb$week, dat_vbb$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",
     main="Verkehrsverbund Berlin-Brandenburg \n (VBB)")
abline(h = mean(dat_vbb$n_ts_sm))
abline(h = mean(dat_vbb$n_ts_sm)+sd(dat_vbb$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_vbb$n_ts_sm)-sd(dat_vbb$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_vbb.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_vbb$week, dat_vbb$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="Verkehrsverbund Berlin-Brandenburg \n (VBB)")
abline(h = mean(dat_vbb$n_ts_sm))
abline(h = mean(dat_vbb$n_ts_sm)+sd(dat_vbb$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_vbb$n_ts_sm)-sd(dat_vbb$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_vbb$peak <- 0
dat_vbb$peak[dat_vbb$n_ts_sm>mean(dat_vbb$n_ts_sm)+sd(dat_vbb$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_vbb$entity_id <- 86

#Remove certain columns from vbb-data as there are columns in the other dataframe called like this
dat_vbb$n <- NULL
dat_vbb$n_w <- NULL

#West Midlands Integrated Transport Authority
#--------------------------------------------
dat_wmita <- dat_base[dat_base$entity_id==87,]
dat_wmita <- ddply(dat_wmita, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_wmita <- dat_wmita[order(dat_wmita$years, dat_wmita$weeks),]

# remove the missing values
dat_wmita <- dat_wmita[!is.na(dat_wmita$weeks),]

adf.test(dat_wmita$n)
adf.test(dat_wmita$n_w)
#stationary time-series?! --> but the graph suggests otherwise

#Build a time series
dat_wmita$n_ts <- ts(dat_wmita$n)
dat_wmita$n_w_ts <- ts(dat_wmita$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_wmita$n_ts_sm <- sma(dat_wmita$n_ts, order = 4)$fitted
dat_wmita$n_w_ts_sm <- sma(dat_wmita$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_wmita$week <- as.Date(paste(dat_wmita$years, dat_wmita$weeks, 1, sep="-"), "%Y-%U-%u")
dat_wmita <- dat_wmita[order(dat_wmita$week),]
plot(dat_wmita$week, dat_wmita$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",
     main="West Midlands Integrated Transport Authority \n (WMITA)")
plot(dat_wmita$week, dat_wmita$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",
     main="West Midlands Integrated Transport Authority \n (WMITA)")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_wmita.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_wmita$week, dat_wmita$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",
     main="West Midlands Integrated Transport Authority \n (WMITA)")
abline(h = mean(dat_wmita$n_ts_sm))
abline(h = mean(dat_wmita$n_ts_sm)+sd(dat_wmita$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_wmita$n_ts_sm)-sd(dat_wmita$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_wmita.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_wmita$week, dat_wmita$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="West Midlands Integrated Transport Authority \n (WMITA)")
abline(h = mean(dat_wmita$n_ts_sm))
abline(h = mean(dat_wmita$n_ts_sm)+sd(dat_wmita$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_wmita$n_ts_sm)-sd(dat_wmita$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_wmita$peak <- 0
dat_wmita$peak[dat_wmita$n_ts_sm>mean(dat_wmita$n_ts_sm)+sd(dat_wmita$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_wmita$entity_id <- 87

#Remove certain columns from wmita-data as there are columns in the other dataframe called like this
dat_wmita$n <- NULL
dat_wmita$n_w <- NULL

#ZVV
#---
dat_zvv <- dat_base[dat_base$entity_id==93,]
dat_zvv <- ddply(dat_zvv, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w))})
dat_zvv <- dat_zvv[order(dat_zvv$years, dat_zvv$weeks),]

# remove the missing values
dat_zvv <- dat_zvv[!is.na(dat_zvv$weeks),]
dat_zvv <- dat_zvv[dat_zvv$weeks!=53,]

adf.test(dat_zvv$n)
adf.test(dat_zvv$n_w)
#stationary time-series

#Build a time series
dat_zvv$n_ts <- ts(dat_zvv$n)
dat_zvv$n_w_ts <- ts(dat_zvv$n_w)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
dat_zvv$n_ts_sm <- sma(dat_zvv$n_ts, order = 4)$fitted
dat_zvv$n_w_ts_sm <- sma(dat_zvv$n_w_ts, order = 4)$fitted

# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat_zvv$week <- as.Date(paste(dat_zvv$years, dat_zvv$weeks, 1, sep="-"), "%Y-%U-%u")
dat_zvv <- dat_zvv[order(dat_zvv$week),]
plot(dat_zvv$week, dat_zvv$n_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles",
     main="Zürcher Verkehrsverbund \n (ZVV)")
plot(dat_zvv$week, dat_zvv$n_w_ts_sm, type = "l", 
     xlab="Weeks",ylab="Number of Articles (weighted)",
     main="Zürcher Verkehrsverbund \n (ZVV)")

# illustrate peaks and slumps
pdf(file=paste("./Results/Graphs/timeline_zvv.pdf", sep=""), paper="special", width=9, height=6)
plot(dat_zvv$week, dat_zvv$n_ts_sm, type = "l",
     xlab="Weeks",ylab="Number of Articles",
     main="Zürcher Verkehrsverbund \n (ZVV)")
abline(h = mean(dat_zvv$n_ts_sm))
abline(h = mean(dat_zvv$n_ts_sm)+sd(dat_zvv$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_zvv$n_ts_sm)-sd(dat_zvv$n_ts_sm), lty = "dashed")
dev.off()

png(filename=paste("./Results/Graphs/timeline_zvv.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_zvv$week, dat_zvv$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="Zürcher Verkehrsverbund \n (ZVV)")
abline(h = mean(dat_zvv$n_ts_sm))
abline(h = mean(dat_zvv$n_ts_sm)+sd(dat_zvv$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_zvv$n_ts_sm)-sd(dat_zvv$n_ts_sm), lty = "dashed")
dev.off()

#Dummy Variable for peak times
dat_zvv$peak <- 0
dat_zvv$peak[dat_zvv$n_ts_sm>mean(dat_zvv$n_ts_sm)+sd(dat_zvv$n_ts_sm)] <- 1

#Entity-Identifier for merging
dat_zvv$entity_id <- 93


#Remove certain columns from ZVV-data as there are columns in the other dataframe called like this
dat_zvv$n <- NULL
dat_zvv$n_w <- NULL

}

#Bind all the individual metro organizations datasets together and (i.e. staple them)

#Since rbind cannot handle time-series operators, we have to transform the datasets

  #create a list of datasets over which we can loop ('' is used to be able to assign list to them later)
dat_df=list('dat_gly','dat_gla','dat_libero','dat_stif','dat_sytral','dat_tfl','dat_vrs','dat_vvs',
            'dat_vbb','dat_wmita','dat_zvv')

  #apply the same function to all 11 dfs, create a list
d_list <- lapply(dat_df, function(df) {
  df <- get(df)
  df$week <- NULL
  df$n_ts <- as.numeric(df$n_ts)
  df$n_w_ts <- as.numeric(df$n_w_ts)
  df$n_ts_sm <- as.numeric(df$n_ts_sm)
  df$n_w_ts_sm <- as.numeric(df$n_w_ts_sm)
  return(df)})

  #use the list just created to assign the transformations to the 11 datasets
for (i in 1:length(dat_df)) {
  assign(dat_df[[i]], d_list[[i]])
}

dat_org <- rbind(dat_gly,dat_gla,dat_libero,dat_stif,dat_sytral,dat_tfl,dat_vrs,dat_vvs,dat_vbb,dat_wmita,dat_zvv)

#-> merge with the full dataset
#Merge (many-to-one) Organization dataset (dat_org) with full Corpus (dat_base)
dat_peak <- join(dat_base, dat_org, by = c("weeks","years","entity_id"), 
                    type="left",match="all")

#Analyze if certain governor characteristics/media types are clearly under-/overrepresented in peak/non-peak times
#------------------------

#Tables by Governor Type for peak/non-peak times
unique(dat_peak$actor.type)
medtyp_peak <- table(dat_peak$peak,dat_peak$actor.type)
png(filename=paste("./Results/Graphs/ActorType_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Actor Type \n and Salience", 
        xlab="Actor Type",ylab="Number of Articles", beside=T,col=c("dimgray","gray"),
        names.arg=c("hybrid","public (elected)","public (non-elected)")) 
legend("topleft",c("Normal Time","Peak Time"),fill=c("dimgray","gray"),bty="n") #()
dev.off()

#Shares of articles in peak and non-peak times by Actor type
require(reshape2)
dat_peak2 <- dat_peak[dat_peak$peak!=NA,]
dat_peak2 <- dat_peak[!is.na(dat_peak$actor.type),]
unique(dat_peak2$peak)

medtyp_share <- dcast(dat_peak2, actor.type ~ peak)
medtyp_share$sum <- medtyp_share[,3]+medtyp_share[,2]
medtyp_share$Perc <- (medtyp_share[,3]/medtyp_share[,5])*100
medtyp_share$sum <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Results/Graphs/ActorType_Share.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$Perc,main="Share of Articles Appearing in Peak Times \n (by Actor Type)", xlab="Actor Type",
        names.arg=c("hybrid","public (elected)","public (non-elected)"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()

#Tables by Policy Scope for peak/non-peak times
unique(dat_peak$policy.scope)
medtyp_peak <- table(dat_peak$peak,dat_peak$policy.scope)
png(filename=paste("./Results/Graphs/PolicyScope_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Policy Scope \n and Salience", 
        xlab="Policy Scope",ylab="Number of Articles", beside=T,col=c("dimgray","gray"),
        names.arg=c("narrow","wide")) 
legend("topright",c("Normal Time","Peak Time"),fill=c("dimgray","gray"),bty="n") #()
dev.off()

#Shares of articles in peak and non-peak times by Actor type
require(reshape2)
dat_peak2 <- dat_peak[dat_peak$peak!=NA,]
dat_peak2 <- dat_peak[!is.na(dat_peak$policy.scope),]
unique(dat_peak2$peak)

medtyp_share <- dcast(dat_peak2, policy.scope ~ peak)
medtyp_share$sum <- medtyp_share[,3]+medtyp_share[,2]
medtyp_share$Perc <- (medtyp_share[,3]/medtyp_share[,5])*100
medtyp_share$sum <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Results/Graphs/PolicyScope_Share.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$Perc,main="Share of Articles Appearing in Peak Times \n (by Policy Scope)", xlab="Policy Scope",
        names.arg=c("narrow","wide"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()

#Tables by Functional Scope for peak/non-peak times
unique(dat_peak$functional.scope..old.)
medtyp_peak <- table(dat_peak$peak,dat_peak$functional.scope..old.)
png(filename=paste("./Results/Graphs/FunctionalScope_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Functional Scope \n and Salience", 
        xlab="Functional Scope",ylab="Number of Articles", beside=T,col=c("dimgray","gray"),
        names.arg=c("implementation","decision-making")) 
legend("topright",c("Normal Time","Peak Time"),fill=c("dimgray","gray"),bty="n") #()
dev.off()

#Shares of articles in peak and non-peak times by Actor type
require(reshape2)
dat_peak2 <- dat_peak[dat_peak$peak!=NA,]
dat_peak2 <- dat_peak[!is.na(dat_peak$functional.scope..old.),]
unique(dat_peak2$peak)

medtyp_share <- dcast(dat_peak2, functional.scope..old. ~ peak)
medtyp_share$sum <- medtyp_share[,3]+medtyp_share[,2]
medtyp_share$Perc <- (medtyp_share[,3]/medtyp_share[,5])*100
medtyp_share$sum <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Results/Graphs/FunctionalScope_Share.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$Perc,main="Share of Articles Appearing in Peak Times \n (by Functional Scope)", xlab="Functional Scope",
        names.arg=c("implementation","decision-making"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()

#Tables by Newspaper type for peak/non-peak times
head(dat_peak)
medtyp_peak <- table(dat_peak$peak,dat_peak$media_type)
png(filename=paste("./Results/Graphs/MediaType_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Media Type \n and Salience", 
        xlab="Media Type",ylab="Number of Articles", beside=T,col=c("dimgray","gray"),
        names.arg=c("magazines","online","quality","regional","tabloid/free")) 
legend("topright",c("Normal Time","Peak Time"),fill=c("dimgray","gray"),bty="n") #()
dev.off()

#Shares of articles in peak and non-peak times by Media type
require(reshape2)
dat_peak2 <- dat_peak[dat_peak$peak!=NA,]
dat_peak2 <- dat_peak[!is.na(dat_peak$media_type),]
unique(dat_peak2$peak)

medtyp_share <- dcast(dat_peak2, media_type ~ peak)
medtyp_share$sum <- medtyp_share[,3]+medtyp_share[,2]
medtyp_share$Perc <- (medtyp_share[,3]/medtyp_share[,5])*100
medtyp_share$sum <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Results/Graphs/MediaType_Share.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$Perc,main="Share of Articles Appearing in Peak Times \n (by Media Type)", xlab="Media Type",
        names.arg=c("magazines","online","quality","regional","tabloid/free"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()

### TIME-SERIES REGRESSIONS ###
#------------------------------

#### time-series regressions
#reaggregate data with independent vars

#Preparation
{
dat <- merge(corpus, days, by.x = "article_date", by.y = "days", all = T)

dat$n[is.na(dat$n)] <- 0
dat$n_w[is.na(dat$n_w)] <- 0
dat$n_w[is.na(dat$tonality_verbalized_negative)] <- 0
dat$n_w[is.na(dat$tonality_verbalized_positive)] <- 0
dat$n_w[is.na(dat$tonality_verbalized_ambivalent)] <- 0
dat$n_w[is.na(dat$actor.type_hybrid)] <- 0
dat$n_w[is.na(dat$`actor.type_public (elected)`)] <- 0
dat$n_w[is.na(dat$`actor.type_public (non-elected)`)] <- 0
dat$n_w[is.na(dat$policy.scope_narrow)] <- 0
dat$n_w[is.na(dat$policy.scope_wide)] <- 0
#dat$n_w[is.na(dat$territorial.scope_global)] <- 0
#dat$n_w[is.na(dat$territorial.scope_regional)] <- 0
dat$n_w[is.na(dat$functional_scope_informative)] <- 0
dat$n_w[is.na(dat$functional_scope_implementing)] <- 0
dat$n_w[is.na(dat$functional_scope_decisive)] <- 0
dat$n_w[is.na(dat$media_type_magazines)] <- 0
#dat$n_w[is.na(dat$media_type_newswire)] <- 0
dat$n_w[is.na(dat$media_type_online)] <- 0
dat$n_w[is.na(dat$media_type_quality)] <- 0
dat$n_w[is.na(dat$media_type_regional)] <- 0
dat$n_w[is.na(dat$media_type_tabloid_or_free)] <- 0
dat$n_w[is.na(dat$media_country_ch)] <- 0
dat$n_w[is.na(dat$media_country_de)] <- 0
dat$n_w[is.na(dat$media_country_fr)] <- 0
#dat$n_w[is.na(dat$media_country_international)] <- 0
dat$n_w[is.na(dat$media_country_uk)] <- 0


#QUESTION: So here you're basically summing up the variables by weeks; does that make sense?
#So does it make sense to count in how many articles in a week a public elected actor appeared and
#how this correlates with the number of articles (or the tonality or the like)?

require(plyr)
#-> when adding "media_country" to the "weeks, years" in brackets, one gets country-weeks
dat <- ddply(dat, .(weeks, years,media_country), function (x) {
  data.frame(n = sum(x$n, na.rm = T),
             n_w = sum(x$n_w, na.rm = T),
             ton_neg = sum(x$tonality_verbalized_negative, na.rm = T),
             ton_pos = sum(x$tonality_verbalized_positive, na.rm = T),
             ton_amb = sum(x$tonality_verbalized_ambivalent, na.rm = T),
             hybrid = sum(x$actor.type_hybrid, na.rm = T),
             publ_elect = sum(x$`actor.type_public (elected)`, na.rm = T),
             publ_nonelect = sum(x$`actor.type_public (non-elected)`, na.rm = T),
             scope_narrow = sum(x$policy.scope_narrow, na.rm = T),
             scope_wide = sum(x$policy.scope_wide, na.rm = T),
#            scope_global = sum(x$territorial.scope_global, na.rm = T),
#            scope_regional = sum(x$territorial.scope_regional, na.rm = T),
             scope_informative = sum(x$functional_scope_informative, na.rm = T),
             scope_implementing = sum(x$functional_scope_implementing, na.rm = T),
             scope_decisive = sum(x$functional_scope_decisive, na.rm = T),
             media_type_mags = sum(x$media_type_magazines, na.rm = T),
#            media_type_wires = sum(x$media_type_newswire, na.rm = T),
             media_type_online = sum(x$media_type_online, na.rm = T),
             media_type_quality = sum(x$media_type_quality, na.rm = T),
             media_type_regional = sum(x$media_type_regional, na.rm = T),
             media_type_tabloid_or_free = sum(x$media_type_tabloid_or_free, na.rm = T),
             country_ch = sum(x$media_country_ch, na.rm = T),
             country_de = sum(x$media_country_de, na.rm = T),
             country_fr = sum(x$media_country_fr, na.rm = T),
#            country_international = sum(x$media_country_international, na.rm = T),
             country_uk = sum(x$media_country_uk, na.rm = T)
             )})


#generate shares instead of absolute values for the variables in question
dat$ton_neg_sh <- dat$ton_neg / dat$n
dat$ton_pos_sh <- dat$ton_pos / dat$n
dat$ton_amb_sh <- dat$ton_amb / dat$n
dat$ton_neg_sh <- dat$ton_neg / dat$n
dat$hybrid_sh <- dat$hybrid / dat$n
dat$publ_elect_sh <- dat$publ_elect / dat$n
dat$publ_nonelect_sh <- dat$publ_nonelect / dat$n
dat$scope_narrow_sh <- dat$scope_narrow / dat$n
dat$scope_wide_sh <- dat$scope_wide / dat$n
dat$scope_informative_sh <- dat$scope_informative / dat$n
dat$scope_implementing_sh <- dat$scope_implementing / dat$n
dat$scope_decisive_sh <- dat$scope_decisive / dat$n
dat$media_type_mags_sh <- dat$media_type_mags / dat$n
dat$media_type_online_sh <- dat$media_type_online / dat$n
dat$media_type_quality_sh <- dat$media_type_quality / dat$n
dat$media_type_regional_sh <- dat$media_type_regional / dat$n
dat$media_type_tabloid_or_free_sh <- dat$media_type_tabloid_or_free / dat$n

dat <- dat[order(dat$year, dat$weeks),]

# there is no article after week 35 of 2015, so we drop the observations after this week
dat <- dat[!(dat$year == 2015 & dat$weeks > 35),]
# week 53 is odd as well, so lets get rid of it as well
dat <- dat[dat$weeks != 53,]
dat <- dat[!is.na(dat$weeks),]

# add weeks and lag of independent variable
dat$week <- factor(as.Date(paste(dat$years, dat$weeks, 1, sep="-"), "%Y-%U-%u"))
dat$l_n <- c(NA, dat$n[1:(length(dat$n)-1)])
dat$l_n_w <- c(NA, dat$n_w[1:(length(dat$n_w)-1)])
dat$l_ton_neg <- c(NA, dat$ton_neg[1:(length(dat$ton_neg)-1)])
dat$l_ton_pos <- c(NA, dat$ton_pos[1:(length(dat$ton_pos)-1)])
dat$l_ton_amb <- c(NA, dat$ton_amb[1:(length(dat$ton_amb)-1)])
dat$l_ton_neg_sh <- c(NA, dat$ton_neg_sh[1:(length(dat$ton_neg_sh)-1)])
dat$l_ton_pos_sh <- c(NA, dat$ton_pos_sh[1:(length(dat$ton_pos_sh)-1)])
dat$l_ton_amb_sh <- c(NA, dat$ton_amb_sh[1:(length(dat$ton_amb_sh)-1)])
}

# I AM STILL HIGHLY INSECURE ABOUT THE COUNT REGRESSION MODELLING !!!!
# it is maybe good start but we have to work intensely on this

# pressing thoughts/questions:
# 1. There are counts on the right side of the equation (not dummies), so I do NOT
#    not know whether we can leave all levels of factors in there.
      #QUESTION: Why do we use counts on right side and not shares (e.g. shares of all articles in a week that
      #contain a public elected actor)
# 2. The model used in the example is very sensitive to overparameterization (i.e. when the data does not contain
#    information to estimate the parameters). For the example, I just dropped independent variables until the model converged.
#    In the actual applicationn, we have to work the type and kind of independent variables (e.g. to use logged variables for
#    some indicators sind their distribution is highly skewed) to avoid this.
# 3. There are specialized models for count data (e.g. tscount or pests = http://www.utdallas.edu/~pbrandt/code--software.html)
#    that indicate that you cannot run simple mixed effects count regressions on our data as I use to do. But I
#    actually have NO clue whether tscount or pests are feasible options. Does anyone know more?
# 4. How should we aggregate the data? weeks (how it is done at the moment,
#    Country-weeks (my favorite at the moment), Entity-weeks?
# 5. We should empirically test for over-dispersion (i.e. whether to use a poisson or
#    negative binomial model) as well as for zero-inflation. I have not done this yet.
# 6. We can run the same kind of analyses for tonality categories as dependent variables.

#install.packages('TMB', type = 'source')
library(glmmTMB) # see https://cran.r-project.org/web/packages/glmmTMB/glmmTMB.pdf
dat2 <- na.omit(dat)

#Regressions
#
{
#Salience Model
  #N
sal <- glmmTMB(n ~ l_n + hybrid +
         publ_nonelect + scope_wide + media_type_quality + media_type_regional + 
           media_type_tabloid_or_free + (1 | week),
         data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
         dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
         verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(sal)

cor_vars <- c("hybrid","publ_nonelect","scope_wide","media_type_quality","media_type_regional","media_type_tabloid_or_free")
dat_cor <- dat2[cor_vars]
cor(dat_cor) 
#high correlations between hybrid/scope_wide (r=.55), hybrid/media_type_regional (r=.59), 
#scope_wide/media_type_regional (r=.62)

  #N (relative measure for independent variables)
sal_sh <- glmmTMB(n ~ l_n + hybrid_sh +
                 publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                 media_type_tabloid_or_free_sh + (1 | week),
               data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
               dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
               verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(sal_sh)
#-> makes a strong difference to the one with independent variables as count variables:

  #N Weighted
sal_w <- glmmTMB(n_w ~ l_n_w + hybrid +
                 publ_nonelect + scope_wide + media_type_quality + media_type_regional + 
                 media_type_tabloid_or_free + (1 | week),
               data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
               dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
               verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(sal_w)
#-> makes no difference to count model

#Tonality Model
  #Negative Articles
ton_n <- glmmTMB(ton_neg ~ l_ton_neg + hybrid +
                 publ_nonelect + scope_wide + media_type_quality + media_type_regional + 
                 media_type_tabloid_or_free + (1 | week),
               data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
               dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
               verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(ton_n)

  #Negative Articles (relative measures for Independent variables)
ton_nsh <- glmmTMB(ton_neg ~ l_ton_neg + hybrid_sh +
                   publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
                 dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
                 verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(ton_nsh)

  #Share of Negative Articles in a week
library(lme4)
ton_nssh <- lmer(ton_neg_sh ~ l_ton_neg_sh + hybrid_sh +
                     publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                     media_type_tabloid_or_free_sh + (1 | week),
                   data = dat2)
summary(ton_nssh)

  #Positive Articles
ton_p <- glmmTMB(ton_pos ~ l_ton_pos + hybrid +
                   publ_nonelect + scope_wide + media_type_quality + media_type_regional + 
                   media_type_tabloid_or_free + (1 | week),
                 data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
                 dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
                 verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(ton_p)

  #Positive Articles (relative measures for Independent variables)
ton_psh <- glmmTMB(ton_pos ~ l_ton_pos + hybrid_sh +
                     publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                     media_type_tabloid_or_free_sh + (1 | week),
                   data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
                   dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
                   verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(ton_psh)

#Share of Positive Articles in a week
ton_pssh <- lmer(ton_pos_sh ~ l_ton_pos_sh + hybrid_sh +
                   publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat2)
summary(ton_pssh)

#Ambivalent Articles
ton_a <- glmmTMB(ton_amb ~ l_ton_amb + hybrid +
                   publ_nonelect + scope_wide + media_type_quality + media_type_regional + 
                   media_type_tabloid_or_free + (1 | week),
                 data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
                 dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
                 verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(ton_a)

#Ambivalent Articles (relative measures for Independent variables)
ton_ash <- glmmTMB(ton_amb ~ l_ton_amb + hybrid_sh +
                     publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                     media_type_tabloid_or_free_sh + (1 | week),
                   data = dat2, family = nbinom2(link = "log"), ziformula = ~0,
                   dispformula = ~1, weights = NULL, offset = NULL, se = TRUE,
                   verbose = TRUE, doFit = TRUE, control = glmmTMBControl(profile = T))
summary(ton_ash)

#Share of Ambivalent Articles in a week
ton_assh <- lmer(ton_amb_sh ~ l_ton_amb_sh + hybrid_sh +
                   publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat2)
summary(ton_assh)

library(stargazer)
stargazer(ton_nssh,ton_pssh,ton_assh,type="latex",title="Salience",out=paste("./Results/Tables/Tonality_sh.tex"))
stargazer(ton_nssh,ton_pssh,ton_assh,type="text",title="Salience",out=paste("./Results/Tables/Tonality_sh.txt"))
#-> stargazer doesn't recognize the count regression models, but the mixed linear models work

}

# TIME-SERIES REGRESSION WITH PEAK INTERACTION

#Preparation
{
dat_peak$n[is.na(dat_peak$n)] <- 0
dat_peak$n_w[is.na(dat_peak$n_w)] <- 0
dat_peak$n_w[is.na(dat_peak$tonality_verbalized_negative)] <- 0
dat_peak$n_w[is.na(dat_peak$tonality_verbalized_positive)] <- 0
dat_peak$n_w[is.na(dat_peak$tonality_verbalized_ambivalent)] <- 0
dat_peak$n_w[is.na(dat_peak$actor.type_hybrid)] <- 0
dat_peak$n_w[is.na(dat_peak$`actor.type_public (elected)`)] <- 0
dat_peak$n_w[is.na(dat_peak$`actor.type_public (non-elected)`)] <- 0
dat_peak$n_w[is.na(dat_peak$policy.scope_narrow)] <- 0
dat_peak$n_w[is.na(dat_peak$policy.scope_wide)] <- 0
#dat_peak$n_w[is.na(dat_peak$territorial.scope_global)] <- 0
#dat_peak$n_w[is.na(dat_peak$territorial.scope_regional)] <- 0
dat_peak$n_w[is.na(dat_peak$functional_scope_informative)] <- 0
dat_peak$n_w[is.na(dat_peak$functional_scope_implementing)] <- 0
dat_peak$n_w[is.na(dat_peak$functional_scope_decisive)] <- 0
dat_peak$n_w[is.na(dat_peak$media_type_magazines)] <- 0
#dat_peak$n_w[is.na(dat_peak$media_type_newswire)] <- 0
dat_peak$n_w[is.na(dat_peak$media_type_online)] <- 0
dat_peak$n_w[is.na(dat_peak$media_type_quality)] <- 0
dat_peak$n_w[is.na(dat_peak$media_type_regional)] <- 0
dat_peak$n_w[is.na(dat_peak$media_type_tabloid_or_free)] <- 0
dat_peak$n_w[is.na(dat_peak$media_country_ch)] <- 0
dat_peak$n_w[is.na(dat_peak$media_country_de)] <- 0
dat_peak$n_w[is.na(dat_peak$media_country_fr)] <- 0
#dat_peak$n_w[is.na(dat_peak$media_country_international)] <- 0
dat_peak$n_w[is.na(dat_peak$media_country_uk)] <- 0


#QUESTION: So here you're basically summing up the variables by weeks; does that make sense?
#So does it make sense to count in how many articles in a week a public elected actor appeared and
#how this correlates with the number of articles (or the tonality or the like)?

require(plyr)
#-> when adding "media_country" to the "weeks, years" in brackets, one gets country-weeks
dat_peak <- ddply(dat_peak, .(weeks, years,media_country), function (x) {
  data.frame(n = sum(x$n, na.rm = T),
             n_w = sum(x$n_w, na.rm = T),
             ton_neg = sum(x$tonality_verbalized_negative, na.rm = T),
             ton_pos = sum(x$tonality_verbalized_positive, na.rm = T),
             ton_amb = sum(x$tonality_verbalized_ambivalent, na.rm = T),
             hybrid = sum(x$actor.type_hybrid, na.rm = T),
             publ_elect = sum(x$`actor.type_public (elected)`, na.rm = T),
             publ_nonelect = sum(x$`actor.type_public (non-elected)`, na.rm = T),
             scope_narrow = sum(x$policy.scope_narrow, na.rm = T),
             scope_wide = sum(x$policy.scope_wide, na.rm = T),
             #            scope_global = sum(x$territorial.scope_global, na.rm = T),
             #            scope_regional = sum(x$territorial.scope_regional, na.rm = T),
             scope_informative = sum(x$functional_scope_informative, na.rm = T),
             scope_implementing = sum(x$functional_scope_implementing, na.rm = T),
             scope_decisive = sum(x$functional_scope_decisive, na.rm = T),
             media_type_mags = sum(x$media_type_magazines, na.rm = T),
             #            media_type_wires = sum(x$media_type_newswire, na.rm = T),
             media_type_online = sum(x$media_type_online, na.rm = T),
             media_type_quality = sum(x$media_type_quality, na.rm = T),
             media_type_regional = sum(x$media_type_regional, na.rm = T),
             media_type_tabloid_or_free = sum(x$media_type_tabloid_or_free, na.rm = T),
             country_ch = sum(x$media_country_ch, na.rm = T),
             country_de = sum(x$media_country_de, na.rm = T),
             country_fr = sum(x$media_country_fr, na.rm = T),
             #            country_international = sum(x$media_country_international, na.rm = T),
             country_uk = sum(x$media_country_uk, na.rm = T),
             peak_sum = sum(x$peak, na.rm = T)
             )})


#generate shares instead of absolute values for the variables in question
dat_peak$ton_neg_sh <- dat_peak$ton_neg / dat_peak$n
dat_peak$ton_pos_sh <- dat_peak$ton_pos / dat_peak$n
dat_peak$ton_amb_sh <- dat_peak$ton_amb / dat_peak$n
dat_peak$ton_neg_sh <- dat_peak$ton_neg / dat_peak$n
dat_peak$hybrid_sh <- dat_peak$hybrid / dat_peak$n
dat_peak$publ_elect_sh <- dat_peak$publ_elect / dat_peak$n
dat_peak$publ_nonelect_sh <- dat_peak$publ_nonelect / dat_peak$n
dat_peak$scope_narrow_sh <- dat_peak$scope_narrow / dat_peak$n
dat_peak$scope_wide_sh <- dat_peak$scope_wide / dat_peak$n
dat_peak$scope_informative_sh <- dat_peak$scope_informative / dat_peak$n
dat_peak$scope_implementing_sh <- dat_peak$scope_implementing / dat_peak$n
dat_peak$scope_decisive_sh <- dat_peak$scope_decisive / dat_peak$n
dat_peak$media_type_mags_sh <- dat_peak$media_type_mags / dat_peak$n
dat_peak$media_type_online_sh <- dat_peak$media_type_online / dat_peak$n
dat_peak$media_type_quality_sh <- dat_peak$media_type_quality / dat_peak$n
dat_peak$media_type_regional_sh <- dat_peak$media_type_regional / dat_peak$n
dat_peak$media_type_tabloid_or_free_sh <- dat_peak$media_type_tabloid_or_free / dat_peak$n
dat_peak$peak_sum_sh <- dat_peak$peak_sum / dat_peak$n

dat_peak <- dat_peak[order(dat_peak$year, dat_peak$weeks),]

hist(dat_peak$peak_sum_sh)
dat_peak$peak_sum_pc <- dat_peak$peak_sum_sh*100
hist(dat_peak$peak_sum_pc)
#somehow the logarithmic transformation makes weird things
dat_peak$peaklog <- log(dat_peak$peak_sum_pc)
hist(dat_peak$peaklog)

# there is no article after week 35 of 2015, so we drop the observations after this week
dat_peak <- dat_peak[!(dat_peak$year == 2015 & dat_peak$weeks > 35),]
# week 53 is odd as well, so lets get rid of it as well
dat_peak <- dat_peak[dat_peak$weeks != 53,]
dat_peak <- dat_peak[!is.na(dat_peak$weeks),]

# add weeks and lag of independent variable
dat_peak$week <- factor(as.Date(paste(dat_peak$years, dat_peak$weeks, 1, sep="-"), "%Y-%U-%u"))
dat_peak$l_n <- c(NA, dat_peak$n[1:(length(dat_peak$n)-1)])
dat_peak$l_n_w <- c(NA, dat_peak$n_w[1:(length(dat_peak$n_w)-1)])
dat_peak$l_ton_neg <- c(NA, dat_peak$ton_neg[1:(length(dat_peak$ton_neg)-1)])
dat_peak$l_ton_pos <- c(NA, dat_peak$ton_pos[1:(length(dat_peak$ton_pos)-1)])
dat_peak$l_ton_amb <- c(NA, dat_peak$ton_amb[1:(length(dat_peak$ton_amb)-1)])
dat_peak$l_ton_neg_sh <- c(NA, dat_peak$ton_neg_sh[1:(length(dat_peak$ton_neg_sh)-1)])
dat_peak$l_ton_pos_sh <- c(NA, dat_peak$ton_pos_sh[1:(length(dat_peak$ton_pos_sh)-1)])
dat_peak$l_ton_amb_sh <- c(NA, dat_peak$ton_amb_sh[1:(length(dat_peak$ton_amb_sh)-1)])

dat_peak2 <- na.omit(dat_peak)
}

#Regression

{
library(lme4)
#Share of Negative Articles in a week
  #Hybrid*Peak
ton_nhy <- lmer(ton_neg_sh ~ l_ton_neg_sh + hybrid_sh*peak_sum_sh +
                   publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_nhy)
  #Public*Peak
ton_npu <- lmer(ton_neg_sh ~ l_ton_neg_sh + hybrid_sh +
                   publ_nonelect_sh*peak_sum_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_npu)
  #Scope*Peak
ton_nsc <- lmer(ton_neg_sh ~ l_ton_neg_sh + hybrid_sh +
                   publ_nonelect_sh + scope_wide_sh*peak_sum_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_nsc)

#Share of Positive Articles in a week
  #Hybrid*Peak
ton_phy <- lmer(ton_pos_sh ~ l_ton_pos_sh + hybrid_sh*peak_sum_sh +
                   publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_phy)
  #Public*Peak
ton_ppu <- lmer(ton_pos_sh ~ l_ton_pos_sh + hybrid_sh +
                   publ_nonelect_sh*peak_sum_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_ppu)
  #Scope*Peak
ton_psc <- lmer(ton_pos_sh ~ l_ton_pos_sh + hybrid_sh +
                   publ_nonelect_sh + scope_wide_sh*peak_sum_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_psc)


#Share of Ambivalent Articles in a week
ton_ahy <- lmer(ton_amb_sh ~ l_ton_amb_sh + hybrid_sh*peak_sum_sh +
                   publ_nonelect_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_ahy)
#Public*Peak
ton_apu <- lmer(ton_amb_sh ~ l_ton_amb_sh + hybrid_sh +
                   publ_nonelect_sh*peak_sum_sh + scope_wide_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_apu)
#Scope*Peak
ton_asc <- lmer(ton_amb_sh ~ l_ton_amb_sh + hybrid_sh +
                   publ_nonelect_sh + scope_wide_sh*peak_sum_sh + media_type_quality_sh + media_type_regional_sh + 
                   media_type_tabloid_or_free_sh + (1 | week),
                 data = dat_peak2)
summary(ton_asc)

stargazer(ton_nhy,ton_npu,ton_nsc,ton_phy,ton_ppu,ton_psc,ton_ahy,ton_apu,ton_asc,
          type="latex",title="Salience",out=paste("./Results/Tables/Tonality_Int.tex"))
stargazer(ton_nhy,ton_npu,ton_nsc,ton_phy,ton_ppu,ton_psc,ton_ahy,ton_apu,ton_asc,
          type="text",title="Salience",out=paste("./Results/Tables/Tonality_Int.txt"))
}


####### Entity analysis (mediated vs. mediatized)
# list named entities of organizations recognized in the Named Entity Recognition

corpus$entities <- tolower(corpus$ner_organizations)
corpus$entities <- gsub(":::", "\n", corpus$entities, fixed = T)
corpus$entities <- gsub("[[:punct:]]", "", corpus$entities)
corpus$entities <- gsub("[[:digit:]]", "", corpus$entities)
corpus$entities <- gsub("\n", ":::", corpus$entities, fixed = T)

entities <- unlist(strsplit(corpus$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
entities[1:20,]
#entities Freq
#2752                      euro 6753
#8889                       vvs 4233
#8670  verband region stuttgart 2339
#6521                      ratp 2015
#8877                       vrs 1902
#7454                       ssb 1707
#1409                       cdu 1343
#6978                       sbb 1321
#7240                      sncf 1305
#7411                       spd 1183
#9421                       zvv  994
#6362                        ps  969
#8625                       vbz  862
#8436                       ump  759
#2912                       fdp  725
#1232                       bvg  557
#7911    tarifverbund stuttgart  534
#4447                    labour  487
#7851                       svp  453
#8756 verkehrsverbund stuttgart  440

#here are the entities included in the analysis
unique(corpus$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
corpus$accountor_spd <- grepl(":::spd:::", corpus$entities, fixed = T)
corpus$accountor_ump <- grepl(":::ump:::", corpus$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- corpus$article_text[corpus$accountor_spd == T]
sampl <- sample(sampl1, 25)
View(data.frame(u=sampl))
write.table(sampl)

# general notes:
# 1. This analysis is best done for single entities, since then it is much easier
#    to decide on which organizations potentially
# 2. Do we need some rules guiding on which and how many named entities we check?
# 3. Since we don't have a machine learning on which articles mention our entites as accountee,
#    we may use this analysis also in this way

#NER ANALYSIS FOR INDIVIDUAL GOVERNORS

{
#Entity-IDs
#33=Grand Lyon
#34=Greater London Authority
#59=Libero
#71=STIF
#72=SYTRAL
#73=Transport for London
#84=Verband Region Stuttgart
#85=Verband und Tarifverbund Stuttgart
#86=Verkehrsverbund Berlin Brandenburg
#87=West Midlands Integrated Transport Authority
#93=ZVV

#Grand Lyon
#----------

#Restrict sample to Grand Lyon
gly <- corpus[corpus$entity_id==33,]

#Extract the Named Entities from the string variable and separate them
gly$entities <- tolower(gly$ner_organizations)
gly$entities <- gsub(":::", "\n", gly$entities, fixed = T)
gly$entities <- gsub("[[:punct:]]", "", gly$entities)
gly$entities <- gsub("[[:digit:]]", "", gly$entities)
gly$entities <- gsub("\n", ":::", gly$entities, fixed = T)

entities <- unlist(strsplit(gly$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
entities[1:20,]
#entities Freq
#275             ps  111
#346            ump   94
#313           sncf   25
#355          verts   17
#327            ter   16
#237          modem   13
#183      jc decaux   11
#333            top   10
#343            udf   10
#201      les verts    9
#50             cgt    8
#52            cice    8
#96             dig    8
#220   météo france    6
#288 renault trucks    6
#311          sénat    6
#47            cfdt    5
#140         figaro    5
#298            rff    5
#302            rpr    5

#cdft=confédération démocratique du travail
#dig=?
#cice=crédit d'impôt pour la competitivité et l'emploi

#here are the entities included in the analysis
unique(gly$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
gly$accountor_ps <- grepl(":::ps:::", gly$entities, fixed = T)
gly$accountor_ump <- grepl(":::ump:::", gly$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- gly$article_text[gly$accountor_ps == T]
sampl <- sample(sampl1, 25)
write.table(sampl, file=paste("./NER_Sample-Texts/GLY_PS.txt")) #Sample of Articles containing PS

#Greater London Authority
#------------------------

#Restrict sample
gla <- corpus[corpus$entity_id==34,]

#Extract the Named Entities from the string variable and separate them
gla$entities <- tolower(gla$ner_organizations)
gla$entities <- gsub(":::", "\n", gla$entities, fixed = T)
gla$entities <- gsub("[[:punct:]]", "", gla$entities)
gla$entities <- gsub("[[:digit:]]", "", gla$entities)
gla$entities <- gsub("\n", ":::", gla$entities, fixed = T)

entities <- unlist(strsplit(gla$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
entities[1:20,]
#330  greater london authority  182
#450                    labour   85
#243               earls court   32
#907                 whitehall   27
#829                 tottenham   25
#315                       gla   21
#802                       tfl   21
#875                  vauxhall   21
#140                   chelsea   18
#121                     capco   17
#849                  treasury   17
#423                       ioc   15
#465                       lda   15
#115              canary wharf   14
#727                   savills   14
#504 london development agency   13
#203                 crossrail   12
#688                    rogers   12
#771                   st paul   12
#28                   assembly   11

#rogers=architectural firm
#savills=real estate agents
#st paul=St Paul's cathedral?

#here are the entities included in the analysis
unique(gla$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
gla$accountor_labour <- grepl(":::labour:::", gla$entities, fixed = T)
gla$accountor_earls <- grepl(":::earls court:::", gla$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- gla$article_text[gla$accountor_labour == T]
sampl <- sample(sampl1, 25)
write.table(sampl, file=paste("./NER_Sample-Texts/GLA_Labour.txt")) #Sample of Articles containing PS
#-> pasting the file into a .txt-file somehow leads R to think it's still operating in that .txt environment
#-> closeAllConnections() will restore it

#Libero
#------

#Restrict sample
lib <- corpus[corpus$entity_id==59,]

#Extract the Named Entities from the string variable and separate them
lib$entities <- tolower(lib$ner_organizations)
lib$entities <- gsub(":::", "\n", lib$entities, fixed = T)
lib$entities <- gsub("[[:punct:]]", "", lib$entities)
lib$entities <- gsub("[[:digit:]]", "", lib$entities)
lib$entities <- gsub("\n", ":::", lib$entities, fixed = T)

rm(entities)

entities <- unlist(strsplit(lib$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                      entities Freq
#132                        sbb  125
#29                         bls   99
#71                          ga   47
#123                        rbs   29
#135                        scb   24
#80                          gp   20
#167                 swiss pass   18
#153                         sp   15
#46                         cvp   12
#64                         fdp   12
#128                        rvk    8
#165                        svp    8
#23                         bgu    7
#27                         bkw    7
#140                 scl tigers    7
#19  berner tarifverbund libero    5
#170        tarifverbund libero    5
#191                        vöv    5
#11                         bea    4
#12                     bell ag    4

#rbs=regionalverkehr bern-solothurn
#rvk=Regional Verkehrskonferenz
#bgu=busbetrieb grenchen und umgebung


#here are the entities included in the analysis
unique(lib$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
lib$accountor_sbb <- grepl(":::sbb:::", lib$entities, fixed = T)
lib$accountor_bls <- grepl(":::bls:::", lib$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- lib$article_text[lib$accountor_sbb == T]
#sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/Libero_SBB.txt")) #Sample of Articles containing PS

#STIF
#----

#Restrict sample
stif <- corpus[corpus$entity_id==71,]

#Extract the Named Entities from the string variable and separate them
stif$entities <- tolower(stif$ner_organizations)
stif$entities <- gsub(":::", "\n", stif$entities, fixed = T)
stif$entities <- gsub("[[:punct:]]", "", stif$entities)
stif$entities <- gsub("[[:digit:]]", "", stif$entities)
stif$entities <- gsub("\n", ":::", stif$entities, fixed = T)

entities <- unlist(strsplit(stif$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                               entities Freq
#591                                 ratp 2005
#673                                 sncf 1258
#572                                   ps  829
#791                                  ump  653
#642                                  rff  309
#717                                 stif  286
#732 syndicat des transports diledefrance  230
#632               réseau ferré de france  222
#729                             syndicat  202
#491                                 ndlr  176
#357                  grand paris express  127
#817                                verts  114
#34                           arc express  111
#603                                régie  111
#700               société du grand paris  111
#116                                  cgt   89
#737                                    t   85
#549                                  pcf   81
#766                           transports   78
#804                               veolia   62

#t/t+=name of ticket for the Paris metro system
#ndlr=nom/note de la rédaction
#régie=Entreprise gérée par les fonctionnaires d'une collectivité publique
#cgt=confédération générale du travail

#here are the entities included in the analysis
unique(stif$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
stif$accountor_ratp <- grepl(":::ratp:::", stif$entities, fixed = T)
stif$accountor_sncf <- grepl(":::sncf:::", stif$entities, fixed = T)
stif$accountor_t <- grepl(":::t:::", stif$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- stif$article_text[stif$accountor_ratp == T]
sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/STIF_RATP.txt")) #Sample of Articles containing PS
sampl1 <- stif$article_text[stif$accountor_t == T]


#SYTRAL
#------

#Restrict sample
sytral <- corpus[corpus$entity_id==72,]

#Extract the Named Entities from the string variable and separate them
sytral$entities <- tolower(sytral$ner_organizations)
sytral$entities <- gsub(":::", "\n", sytral$entities, fixed = T)
sytral$entities <- gsub("[[:punct:]]", "", sytral$entities)
sytral$entities <- gsub("[[:digit:]]", "", sytral$entities)
sytral$entities <- gsub("\n", ":::", sytral$entities, fixed = T)

entities <- unlist(strsplit(sytral$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                                entities Freq
#62                            ps   29
#75                          sncf   14
#95                           ump   12
#98                         verts    9
#88                           ter    8
#87                           tcl    7
#85                        sytral    6
#90                           top    6
#92 transports en commun lyonnais    6
#38                        keolis    5
#46                         modem    5
#34                           ier    4
#40                     les verts    4
#49                         notre    4
#7                            cgt    3
#13                conseil d'etat    3
#57                           plu    3
#12                       conseil    2
#21                          etat    2
#23               europe ecologie    2

#here are the entities included in the analysis
unique(sytral$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
sytral$accountor_ps <- grepl(":::ps:::", sytral$entities, fixed = T)
sytral$accountor_sncf <- grepl(":::sncf:::", sytral$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- sytral$article_text[sytral$accountor_ps == T]
write.table(sampl1, file=paste("./NER_Sample-Texts/SYTRAL_PS.txt")) #Sample of Articles containing PS

#Transport for London
#--------------------

#Restrict sample
tfl <- corpus[corpus$entity_id==73,]

#Extract the Named Entities from the string variable and separate them
tfl$entities <- tolower(tfl$ner_organizations)
tfl$entities <- gsub(":::", "\n", tfl$entities, fixed = T)
tfl$entities <- gsub("[[:punct:]]", "", tfl$entities)
tfl$entities <- gsub("[[:digit:]]", "", tfl$entities)
tfl$entities <- gsub("\n", ":::", tfl$entities, fixed = T)

entities <- unlist(strsplit(tfl$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                  entities Freq
#2777                  tfl  345
#1470               labour  316
#2882 transport for london  222
#2875            transport  212
#2399                  rmt  145
#827           earls court  111
#415                 capco  105
#1600      london assembly   97
#1825             metronet   88
#187                   bbc   79
#2028                  nhs   78
#99               assembly   77
#2919             treasury   77
#1999         network rail   74
#446                  cctv   71
#404          canary wharf   66
#3162            whitehall   63
#1438          kings cross   57
#164              barclays   56
#2253                  ppp   55

#rmt=union for transport workers
#earls court=city district in chelsea and kensington borough
#capco=international finance consulting company
#ppp=public private partnership

#here are the entities included in the analysis
unique(tfl$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
tfl$accountor_labour <- grepl(":::labour:::", tfl$entities, fixed = T)
tfl$accountor_rmt <- grepl(":::rmt:::", tfl$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- tfl$article_text[tfl$accountor_labour == T]
sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/TFL_Labour.txt")) #Sample of Articles containing PS

#VRS
#---

#Restrict sample
vrs <- corpus[corpus$entity_id==84,]

#Extract the Named Entities from the string variable and separate them
vrs$entities <- tolower(vrs$ner_organizations)
vrs$entities <- gsub(":::", "\n", vrs$entities, fixed = T)
vrs$entities <- gsub("[[:punct:]]", "", vrs$entities)
vrs$entities <- gsub("[[:digit:]]", "", vrs$entities)
vrs$entities <- gsub("\n", ":::", vrs$entities, fixed = T)

entities <- unlist(strsplit(vrs$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                  entities Freq
#244                       euro 3017
#912   verband region stuttgart 1958
#962                        vrs 1558
#130                        cdu  891
#968                        vvs  755
#731                        spd  694
#740                        ssb  495
#289                        fdp  277
#1012                       wrs  157
#921  verbands region stuttgart  128
#166                   db regio  120
#394                        ihk  118
#241                         eu  112
#906                        vcd   99
#598                       öpnv   96
#833     tarifverbund stuttgart   94
#681                         rp   89
#82                       bosch   88
#6                           ag   86
#943  verkehrsverbund stuttgart   81

#vcd=Verkehrsclub Deutschland
#rp=Regierungspräsidium
#ag=Amtsgericht?
#rmt=union for transport workers
#earls court=city district in chelsea and kensington borough
#capco=international finance consulting company
#ppp=public private partnership

#here are the entities included in the analysis
unique(vrs$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
vrs$accountor_vvs <- grepl(":::vvs:::", vrs$entities, fixed = T)
vrs$accountor_cdu <- grepl(":::cdu:::", vrs$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- vrs$article_text[vrs$accountor_cdu == T]
sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/VRS_CDU.txt")) #Sample of Articles containing PS

#VVS
#---

#Restrict sample
vvs <- corpus[corpus$entity_id==85,]

#Extract the Named Entities from the string variable and separate them
vvs$entities <- tolower(vvs$ner_organizations)
vvs$entities <- gsub(":::", "\n", vvs$entities, fixed = T)
vvs$entities <- gsub("[[:punct:]]", "", vvs$entities)
vvs$entities <- gsub("[[:digit:]]", "", vvs$entities)
vvs$entities <- gsub("\n", ":::", vvs$entities, fixed = T)

entities <- unlist(strsplit(vvs$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                  entities Freq
#1065                          vvs 3465
#270                          euro 3213
#778                           ssb 1212
#911        tarifverbund stuttgart  438
#143                           cdu  410
#987      verband region stuttgart  381
#1028    verkehrsverbund stuttgart  358
#1064                          vrs  344
#769                           spd  326
#867  stuttgarter straßenbahnen ag  132
#309                           fdp  122
#917       tarifverbunds stuttgart  120
#636                          öpnv  103
#975                           vcd   91
#183                      db regio   85
#175                            db   84
#13                             ag   74
#897                           sve   70
#415                           ihk   68
#548                           lvl   63

#lvl=Ludwigsburger Verkehrslinien
#sve=Stadtverkehr Esslingen

#vcd=Verkehrsclub Deutschland
#rp=Regierungspräsidium
#ag=Amtsgericht?
#rmt=union for transport workers
#earls court=city district in chelsea and kensington borough
#capco=international finance consulting company
#ppp=public private partnership

#here are the entities included in the analysis
unique(vvs$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vvs. 

# match 'accountor entity candidates' back to the corpus
vvs$accountor_ssb <- grepl(":::ssb:::", vvs$entities, fixed = T)
vvs$accountor_cdu <- grepl(":::cdu:::", vvs$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- vvs$article_text[vvs$accountor_ssb == T]
sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/VVS_SSB.txt")) #Sample of Articles containing PS

#VBB
#---

#Restrict sample
vbb <- corpus[corpus$entity_id==86,]

#Extract the Named Entities from the string variable and separate them
vbb$entities <- tolower(vbb$ner_organizations)
vbb$entities <- gsub(":::", "\n", vbb$entities, fixed = T)
vbb$entities <- gsub("[[:punct:]]", "", vbb$entities)
vbb$entities <- gsub("[[:digit:]]", "", vbb$entities)
vbb$entities <- gsub("\n", ":::", vbb$entities, fixed = T)

entities <- unlist(strsplit(vbb$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                  entities Freq
#100                               bvg  555
#157                              euro  419
#362                               vbb  328
#388 verkehrsverbund berlinbrandenburg  251
#330                               spd  163
#119                                db  111
#64          berliner verkehrsbetriebe   69
#111                               cdu   41
#207                              igeb   31
#126                          db regio   25
#143                               dpa   24
#179                               gdl   21
#33                                bbi   17
#68                   berliner zeitung   15
#404                               vvs   13
#170                               fdp   12
#124                           db netz   10
#258                  national express   10
#304                               re    10
#250                               mtr    9

#bvg=Berliner Verkehrsbetriebe
#igeb=Berliner Fahrgastverband
#dpa=Deutsche Presse-Agentur
#gdl=Gewerkschaft Deutscher Lokomotivführer
#bbi=vormals BER
#mtr=Nahverkehranbieter aus Hongkong

#vcd=Verkehrsclub Deutschland
#rp=Regierungspräsidium
#ag=Amtsgericht?
#rmt=union for transport workers
#earls court=city district in chelsea and kensington borough
#capco=international finance consulting company
#ppp=public private partnership

#here are the entities included in the analysis
unique(vbb$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, vbb. 

# match 'accountor entity candidates' back to the corpus
vbb$accountor_bvg <- grepl(":::bvg:::", vbb$entities, fixed = T)
vbb$accountor_spd <- grepl(":::spd:::", vbb$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- vbb$article_text[vbb$accountor_bvg == T]
sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/VBB_BVG.txt")) #Sample of Articles containing PS


#WMITA
#-----

#Restrict sample
wmita <- corpus[corpus$entity_id==87,]

#Extract the Named Entities from the string variable and separate them
wmita$entities <- tolower(wmita$ner_organizations)
wmita$entities <- gsub(":::", "\n", wmita$entities, fixed = T)
wmita$entities <- gsub("[[:punct:]]", "", wmita$entities)
wmita$entities <- gsub("[[:digit:]]", "", wmita$entities)
wmita$entities <- gsub("\n", ":::", wmita$entities, fixed = T)

entities <- unlist(strsplit(wmita$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                  entities Freq
#                         birmingham city council  285
#1944                                     walsall  184
#1272                               midland metro  151
#643                     department for transport  149
#1362                                network rail  105
#1342                            national express   94
#428                                       centro   87
#1099                                      labour   86
#1252                                       metro   85
#576                                     coventry   84
#2034 west midlands passenger transport authority   77
#1492                                         pta   70
#416                               central trains   62
#248                              birmingham mail   57
#255                              birmingham post   57
#2083                               wolverhampton   57
#1357                                         nec   56
#405                                         cctv   55
#1827                                   transport   48
#281                                black country   46

#nec=national exhibition centre
#pta=parent-teacher association
#walsall=city

#here are the entities included in the analysis
unique(wmita$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, wmita. 

# match 'accountor entity candidates' back to the corpus
wmita$accountor_bcc <- grepl(":::birmingham city council:::", wmita$entities, fixed = T)
wmita$accountor_walsall <- grepl(":::walsall:::", wmita$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- wmita$article_text[wmita$accountor_bcc == T]
sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/WMITA_CityCouncil.txt")) #Sample of Articles containing PS


#ZVV
#---

#Restrict sample
zvv <- corpus[corpus$entity_id==93,]

#Extract the Named Entities from the string variable and separate them
zvv$entities <- tolower(zvv$ner_organizations)
zvv$entities <- gsub(":::", "\n", zvv$entities, fixed = T)
zvv$entities <- gsub("[[:punct:]]", "", zvv$entities)
zvv$entities <- gsub("[[:digit:]]", "", zvv$entities)
zvv$entities <- gsub("\n", ":::", zvv$entities, fixed = T)

entities <- unlist(strsplit(zvv$entities, ":::"))
entities <- as.data.frame(table(entities))
entities <- entities[order(entities$Freq, decreasing = T),]

# check the first 20 entries (we probably have to do much more in the actual analysis!!!)
closeAllConnections()
entities[1:20,]
#                  entities Freq
#474                       sbb 1196
#724                       zvv  992
#629                       vbz  862
#568                       svp  445
#715   zürcher verkehrsverbund  379
#195                       fdp  302
#530                        sp  248
#642           verkehrsverbund  177
#118                       cvp  167
#585                        ta  110
#177                      euro  102
#631                       vcs   99
#184                       evp   95
#624                       vbg   94
#583                       szu   89
#398                       nzz   84
#250                        hb   80
#211                        ga   71
#696                       zsg   63
#718 zürcher verkehrsverbundes   56

#vbg=Verkehrsbetriebe Glattal


#here are the entities included in the analysis
unique(zvv$entity_name)

# hence, we have named entities that correspond to our observed entities
# but we also find named entities that may be accountors such as the "spd", "fdp", "ump"
# or potential accountees, like ssb, wrs, zvv. 

# match 'accountor entity candidates' back to the corpus
zvv$accountor_sbb <- grepl(":::sbb:::", zvv$entities, fixed = T)
zvv$accountor_vbz <- grepl(":::vbz:::", zvv$entities, fixed = T)

# check the contents of a sample of texts in order to corroborate hypothesis that the
# 'accountor entity candidates' actually are accountors
sampl1 <- zvv$article_text[zvv$accountor_sbb == T]
sampl <- sample(sampl1, 25)
write.table(sampl1, file=paste("./NER_Sample-Texts/ZVV_SBB.txt")) #Sample of Articles containing PS
}

######## Topic Models

#PEAK TIMES, GERMAN
{
library(tm)
library(stm)

#free some workspace
rm(list=c("dat_high","dat_base","dat_peak2","tmp_vars","input","dat","dat_gly","dat_gla","dat_libero","dat_stif","dat_sytral",
          "dat_tfl","dat_vrs","dat_vvs","dat_vbb","dat_wmita","dat_zvv","medtyp_share","i","dat_df","sampl","pol_scop",
          "fun_scop","medtyp_peak","vars","var"))

#first, we have to prepare our texts for the analysis

# topic models should not be run across languages (we should discuss machine translating everything!),
# so we only take German docs for this exercise
  
#take only peak time articles
tmpeak_de <- dat_peak[dat_peak$peak==1,]
tmpeak_de <- tmpeak_de[tmpeak_de$media_language=="DE",]

tmpeak_de <- tmpeak_de[order(tmpeak_de$article_date, decreasing = T),]
tmpeak_de$weeks <- week(tmpeak_de$article_date)
tmpeak_de <- tmpeak_de[tmpeak_de$weeks != 53,]
tmpeak_de$years <- year(tmpeak_de$article_date)
tmpeak_de$week <- as.numeric(as.factor(as.Date(paste(tmpeak_de$years,
                     tmpeak_de$weeks, 1, sep="-"), "%Y-%U-%u")))

# preprocess texts (rather rough but quite standard). Denny and Spirling
# (https://www.nyu.edu/projects/spirling/documents/preprocessing.pdf) argue that we should evaluate this, too (?).
myCorpus <- Corpus(VectorSource(tmpeak_de$article_text))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, stripWhitespace)
for (i in 1:length(tmpeak_de$article_text)) {
  tmpeak_de$article_text_p[i] <- gsub("\\s", " ", myCorpus[[i]])
}
tmpeak_de$article_text_p <- gsub("\\s+", " ", tmpeak_de$article_text_p)
tmpeak_de$article_text_p <- gsub("^\\s+|\\s+$", "", tmpeak_de$article_text_p)

#write out corpus for the w2v evaluation (see below)
write.csv(tmpeak_de$article_text_p, "corpus_w2v.csv", row.names = F, fileEncoding = "utf-8")

# Generate a corpus for the stm 
corpus_stm <- textProcessor(tmpeak_de[,c("article_text_p")], metadata=tmpeak_de, stem=T,
                        language="german", removestopwords=T, lowercase=F,
                        removenumbers=F, removepunctuation=F)
corpus_stm <- prepDocuments(corpus_stm$documents, corpus_stm$vocab, corpus_stm$meta, lower.thresh = 1)


# Since we will use a deterministic (spectral) initialization of our topic model, many paramters of the topic model will 
# be fixed. We only need an evaluation of the optimal number of topics, which we achive in three steps

## 1. run a topic model for every granularity in the range of interest

# the stm likes factors - m.n. in the postestimation -, so we encode our indicators accordingly
corpus_stm$meta$actor.type <- as.factor(corpus_stm$meta$actor.type)
corpus_stm$meta$media_source <- as.factor(as.character(corpus_stm$meta$media_source))
corpus_stm$meta$media_country <- as.factor(as.character(corpus_stm$meta$media_country))
corpus_stm$meta$entity_name <- as.factor(corpus_stm$meta$entity_name)
corpus_stm$meta$media_type <- as.factor(corpus_stm$meta$media_type)
#corpus_stm$meta$tonality_verbalized <- as.factor(as.character(corpus_stm$meta$tonality_verbalized))
#corpus_stm$meta$territorial.scope <- as.factor(corpus_stm$meta$territorial.scope)
corpus_stm$meta$policy.scope <- as.factor(corpus_stm$meta$policy.scope)

# define range of topic numbers for evaluation
ks <- 3:6 #for thesting, we only evaluate 4 models, a more meaningful range is 3:50 or even 3:100

# define the prevalence formula (basically a regression of indicators on the topic prevalences generated by the model)
# for clarity
formula_prev <- "prevalence =~ actor.type + policy.scope + media_type + media_source + media_country + article_word_count + s(week)"

# evaluate on the 100 most probable words per topic
n_words <- 100

# loop over ks and write out most probable words per topic for each model
for (k in ks) {
 STM <- stm(corpus_stm$documents,corpus_stm$vocab, K=k, 
            as.formula(formula_prev), data = corpus_stm$meta,
            init.type="Spectral", verbose=T, 
            control = list(nits = 100, burnin = 25))
 words <- labelTopics(STM, n = n_words)
 words <- as.data.frame(words[[1]])
 outname <- paste0("./Results/words_", k, "_", ".txt")
 write.table(words, outname, col.names = F, row.names = F, sep = "\t", quote = F)
} 
#this command seems to take too much working space and cannot be conducted; what to do?

## 2. build a w2v model. I will not go into the details, but w2v is a word embedding model
#     that allows to estimate a semantic space for each word (i.e. space of dimensions of related words)
#     w2v is implemented in gensim in python, which is why we run an external script here
command = "C:/Users/noerber/AppData/Local/Programs/Python/Python37" # type "which python" in your terminal to determine the path to your python dist
path2script = "train_w2v_model.py"
corpus_path = "corpus_w2v.csv"
model_path = "w2v_model.txt"

allArgs = c(path2script, "english", corpus_path, model_path)# Build up args in a vector
system2(command, args=allArgs) #run the python script

# 3. Now we calculate coherence (how similar are words within a topic) and discrimination (how
#    dissimilar are word of a topic compared to the words of all other topics)
#    for more, see: https://www.fabriziogilardi.org/resources/papers/policy-diffusion-issue-definition.pdf

#run w2v evaluation
command = "C:/Users/noerber/AppData/Local/Programs/Python/Python37"
path2script = "coherence.py"
model_path = "w2v_model.txt"
allArgs = c(path2script, model_path)
system2(command, args=allArgs)

# let's see which k is best...
w2v <- read.csv("stm-evaluation.txt", header = F)
colnames(w2v) <- c("coherence", "coherence.discrimination", "k")
w2v[w2v$coherence.discrimination == max(w2v$coherence.discrimination),]
# ...the topic model with k = 6

#plot the evaluation (smoothed line not neat with only 4 models)
library(ggplot2)
plott <- ggplot(w2v, aes(x=k, y=coherence.discrimination)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "loess", colour = "black", span = 0.5)
ggsave(plot=plott, paste("./Results/Graphs/w2v_evaluation.pdf"), height=5, width=8.09)


## ok, let's run the model for the optimal number of topics
k <- 6
STM <- stm(corpus_stm$documents, corpus_stm$vocab, K=k, as.formula(formula_prev),
           data = corpus_stm$meta, init.type="Spectral", verbose = T,
           control = list(nits = 100, burnin = 25))# Fit stm

#save model output
saveRDS(STM, "./Results/STM.rds")

# Save 100 most probable words per topic
words <- labelTopics(STM, n = 100)
words <- as.data.frame(t(words[[1]]))
write.table(words, file = "./Results/words_6.txt", col.names = T, row.names = F, sep = "\t", quote = F)

#save 5 most relevant documents per topic
thoughts <- findThoughts(STM, texts = corpus_stm$meta$article_text, n = 5, topics = 1:k)
thoughtsDocs <- data.frame(matrix(unlist(thoughts$docs), nrow=k, byrow=T))
thoughtsIndices <- data.frame(matrix(unlist(thoughts$index), nrow=k, byrow=T))
thoughts <- cbind(thoughtsDocs, thoughtsIndices)
write.csv(thoughts, paste0("./Results/thoughts_", k, ".csv"),
          fileEncoding = "utf-8")

# Write out probability, exclusivity and frequency of each word per topic
require(matrixStats)

#A helper: a James-Stein Estimator Shrinking to a Uniform Distribution
#This draws from Hausser and Strimmer (2009) JMLR.
js.estimate <- function(prob, ct) {
  if(ct<=1) {
    #basically if we only observe a count of 1
    #the variance goes to infinity and we get the uniform distribution.
    return(rep(1/length(prob), length(prob)))
  }
  # MLE of prob estimate
  mlvar <- prob*(1-prob)/(ct-1)
  unif <- rep(1/length(prob), length(prob)) 
  
  # Deviation from uniform
  deviation <- sum((prob-unif)^2)
  
  #take care of special case,if no difference it doesn't matter
  if(deviation==0) return(prob)
  
  lambda <- sum(mlvar)/deviation
  #if despite  our best efforts we ended up with an NaN number-just return the uniform distribution.
  if(is.nan(lambda)) return(unif)
  
  #truncate
  if(lambda>1) lambda <- 1
  if(lambda<0) lambda <- 0
  
  #Construct shrinkage estimator as convex combination of the two
  lambda*unif + (1 - lambda)*prob
}
safelog <- function(x) {
  out <- log(x)
  out[which(out< -1000)] <- -1000
  out
}

logbeta <- STM$beta$logbeta[[1]]
topics <- 1:nrow(logbeta) 
K <- STM$settings$dim$K
wordcounts <- STM$settings$dim$wcounts$x
excl <- t(t(logbeta) - colLogSumExps(logbeta))
excl <- safelog(sapply(1:ncol(excl), function(x) js.estimate(exp(excl[,x]), wordcounts[x])))
freqscore <- apply(logbeta,1,rank)/ncol(logbeta)
freqscore <- as.data.frame(freqscore)
colnames(freqscore) <- paste("frequency_", 1:K, sep = "")
exclscore <- apply(excl,1,rank)/ncol(logbeta)
exclscore <- as.data.frame(exclscore)
colnames(exclscore) <- paste("exclusivity_", 1:K, sep = "")
beta <- lapply(STM$beta$logbeta, exp)
probscore <- as.data.frame(t(beta[[1]]))
colnames(probscore) <- paste("probability_", 1:K, sep = "")
scores <- cbind(probscore, exclscore, freqscore)
scores$vocab <- STM$vocab

outname <- paste0("./Results/scores_6.txt")
write.table(scores, outname, col.names = T, row.names = F, sep = "\t", quote = F)


# produce word-plots

# Read data and check its structure
words <- read.delim2("./Results/words_6.txt")
scores <- read.delim2("./Results/scores_6.txt")
scores <- data.frame(scores)
is.data.frame(scores)
names(scores)
head(scores)
sapply(scores, mode)

# render relevant variables numeric
co <- ncol(scores) - 1
scores[, c(1:co)] <- sapply(scores[, c(1:co)], as.numeric)
head(scores)

# Identify the Columns associated with probabilities, exclusivities and frequencies
pr <- seq(1,co/3)
ex <- seq(max(pr) + 1, max(pr) + co/3)
fr <- seq(max(ex) + 1, max(ex) + co/3)

# Plot and Save Top 50 (n) Words for each Topic (ncol(words)) by Prob, Excl and Freq
n <- 50 
for(i in 1:ncol(words)){
  topic <- data.frame(c(scores[, c(pr[i], ex[i], fr[i], co + 1)], i))
  colnames(topic) <- c("Probability", "Exclusivity", "Frequency", "Words", "Topic")
  topic <- topic[ topic[,4] %in% words[1:n,i] ,]
  pdf(file = paste("./Results/Graphs/Plain_TopicPeakDE_", i, ".pdf", sep = ""), paper = "special", width=7, height=5.5)
  print(ggplot(data = topic, aes(x = Exclusivity, y = Frequency, label = Words)) + geom_text(aes(size = Probability)) + theme(legend.position = "none") + ggtitle(paste("Topic ", i, sep = "")))
  dev.off()
}

# correlations with covariates from the prevalence formula
# weeks
prep <- estimateEffect(1:k ~ s(week), STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "week")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "WeeksPassed")
outname <- paste0("./Results/effectsPeakDE_", "WeeksPassed.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate actor type
prep <- estimateEffect(1:k ~ actor.type, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "actor.type", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "actor.type")
outname <- paste0("./Results/effectsPeakDE_", "ActorType_estimate.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

# difference actor type
x <- plot(prep, covariate = "actor.type", model = STM, method = "difference", cov.value1="public (non-elected)", 
          cov.value2="public (elected)")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1], x$cis[[i]][2])
  df <- cbind(df, i, x$labels[[i]])
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("diff_nonelec_elec", "lb", "ub", "topic", "actor.type")
outname <- paste0("./Results/difference_", "ActorType_diff.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)


#####################
# correlation plots 

# correlation of topic prevalence with daily trend
d <- data.frame(read.delim("./Results/effects_WeeksPassed.txt"))

d <- d[order(d$topic, d$WeeksPassed),]
length(d$WeeksPassed)
max(d$WeeksPassed)
mo <- seq(as.Date("2005-01-01"), by = "week", length.out = max(d$WeeksPassed))

topic_labels <- 1:6 # can be used later to define substantial topics
d$topicLabels <- NA
d$date <- rep(mo, length(topic_labels))

colnames(d)[5] <- c("var")

d$pointEstimateAverage <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]
  d$pointEstimateAverage[d$topic == j] <- mean(d$pointEstimate[d$topic == j])
}

d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$pointEstimateAverage, decreasing = TRUE)]))

pdf(file="./Results/Graphs/correlations-weekspassed.pdf", paper="special", width=12, height=9)
print(ggplot(data = d, aes(x = date, y = pointEstimate)) +
        geom_line() +
        geom_hline(aes(yintercept = pointEstimateAverage), linetype = 2) +
        facet_wrap(~ topicLabels, ncol = 2) +
        labs(x = "weeks", y = "Topic prevalence", title = "") +
        theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm")) +
        scale_alpha(guide = "none") +
        geom_ribbon(aes(ymin = lb, ymax= ub, alpha = 0.5))) 
dev.off()

# correlation of topic prevalence with Actor Type
file_name <- "./Results/effects_ActorType_estimate.txt"
var_label_long <- "Actor types in media reports"

d <- data.frame(read.delim(file_name))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

d$topicLabels <- NA
d$pointEstimateAverage <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]
  d$pointEstimateAverage[d$topic == j] <- mean(d$pointEstimate[d$topic == j])
}

d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$pointEstimateAverage, decreasing = TRUE)]))
#d$var <- as.factor(d$var)

pdf(file=paste("./Results/Graphs/correlations-actortype.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()

# difference of correlation of topic prevalence with tonality = positive and tonality = negative
file_name <- "./Results/difference_ActorType_diff.txt"
var_label_long <- "Actor type in media reports"

d <- data.frame(read.delim(file_name))
var_label <- colnames(d)[5]
colnames(d)[5] <- c("var")

#add topic labels
d$topicLabels <- NA
for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]
}

d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$diff_nonelec_elec, decreasing = TRUE)]))
d$var <- as.factor(d$var)

pdf(file=paste("./Results/correlations-diff-actortype.pdf", sep=""), paper="special", width=6.5, height=5)
print(ggplot(data = d, aes(x = topicLabels, y = diff_nonelec_elec, ymin = lb, ymax = ub)) + #, color = groupLabels
        geom_pointrange() +
        coord_flip() +
        geom_hline(yintercept = 0, lty = 2, size = 0.5) +
        labs(x = "", y = "Difference in topic prevalence (non-elected - elected)", title = "") +
        theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()

}


#NORMAL TIMES, GERMAN
{
  library(tm)
  library(stm)
  
  #first, we have to prepare our texts for the analysis
  
  # topic models should not be run across languages (we should discuss machine translating everything!),
  # so we only take German docs for this exercise
  
  #take only peak time articles
  tmnorm_de <- dat_peak[dat_peak$peak==0,]
  tmnorm_de <- tmnorm_de[tmnorm_de$media_language=="DE",]
  
  tmnorm_de <- tmnorm_de[order(tmnorm_de$article_date, decreasing = T),]
  tmnorm_de$weeks <- week(tmnorm_de$article_date)
  tmnorm_de <- tmnorm_de[tmnorm_de$weeks != 53,]
  tmnorm_de$years <- year(tmnorm_de$article_date)
  tmnorm_de$week <- as.numeric(as.factor(as.Date(paste(tmnorm_de$years,
                                                       tmnorm_de$weeks, 1, sep="-"), "%Y-%U-%u")))
  
  # preprocess texts (rather rough but quite standard). Denny and Spirling
  # (https://www.nyu.edu/projects/spirling/documents/preprocessing.pdf) argue that we should evaluate this, too (?).
  myCorpus <- Corpus(VectorSource(tmnorm_de$article_text))
  myCorpus <- tm_map(myCorpus, tolower)
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  for (i in 1:length(tmnorm_de$article_text)) {
    tmnorm_de$article_text_p[i] <- gsub("\\s", " ", myCorpus[[i]])
  }
  tmnorm_de$article_text_p <- gsub("\\s+", " ", tmnorm_de$article_text_p)
  tmnorm_de$article_text_p <- gsub("^\\s+|\\s+$", "", tmnorm_de$article_text_p)
  
  #write out corpus for the w2v evaluation (see below)
  write.csv(tmnorm_de$article_text_p, "corpus_w2vnormde.csv", row.names = F, fileEncoding = "utf-8")
  
  # Generate a corpus for the stm 
  corpus_stm <- textProcessor(tmnorm_de[,c("article_text_p")], metadata=tmnorm_de, stem=T,
                              language="german", removestopwords=T, lowercase=F,
                              removenumbers=F, removepunctuation=F)
  corpus_stm <- prepDocuments(corpus_stm$documents, corpus_stm$vocab, corpus_stm$meta, lower.thresh = 1)
  
  
  # Since we will use a deterministic (spectral) initialization of our topic model, many paramters of the topic model will 
  # be fixed. We only need an evaluation of the optimal number of topics, which we achive in three steps
  
  ## 1. run a topic model for every granularity in the range of interest
  
  # the stm likes factors - m.n. in the postestimation -, so we encode our indicators accordingly
  corpus_stm$meta$actor.type <- as.factor(corpus_stm$meta$actor.type)
  corpus_stm$meta$media_source <- as.factor(as.character(corpus_stm$meta$media_source))
  corpus_stm$meta$media_country <- as.factor(as.character(corpus_stm$meta$media_country))
  corpus_stm$meta$entity_name <- as.factor(corpus_stm$meta$entity_name)
  corpus_stm$meta$media_type <- as.factor(corpus_stm$meta$media_type)
  #corpus_stm$meta$tonality_verbalized <- as.factor(as.character(corpus_stm$meta$tonality_verbalized))
  #corpus_stm$meta$territorial.scope <- as.factor(corpus_stm$meta$territorial.scope)
  corpus_stm$meta$policy.scope <- as.factor(corpus_stm$meta$policy.scope)
  
  # define range of topic numbers for evaluation
  ks <- 3:6 #for thesting, we only evaluate 4 models, a more meaningful range is 3:50 or even 3:100
  
  # define the prevalence formula (basically a regression of indicators on the topic prevalences generated by the model)
  # for clarity
  formula_prev <- "prevalence =~ actor.type + policy.scope + media_type + media_source + media_country + article_word_count + s(week)"
  
  # evaluate on the 100 most probable words per topic
  n_words <- 100
  
  # loop over ks and write out most probable words per topic for each model
  for (k in ks) {
    STM <- stm(corpus_stm$documents,corpus_stm$vocab, K=k, 
               as.formula(formula_prev), data = corpus_stm$meta,
               init.type="Spectral", verbose=T, 
               control = list(nits = 100, burnin = 25))
    words <- labelTopics(STM, n = n_words)
    words <- as.data.frame(words[[1]])
    outname <- paste0("./Results/words_", k, "_", ".txt")
    write.table(words, outname, col.names = F, row.names = F, sep = "\t", quote = F)
  } 
  #this command seems to take too much working space and cannot be conducted; what to do?
  
  ## 2. build a w2v model. I will not go into the details, but w2v is a word embedding model
  #     that allows to estimate a semantic space for each word (i.e. space of dimensions of related words)
  #     w2v is implemented in gensim in python, which is why we run an external script here
  command = "C:/Users/noerber/AppData/Local/Programs/Python/Python37" # type "which python" in your terminal to determine the path to your python dist
  path2script = "train_w2v_model.py"
  corpus_path = "corpus_w2vnormde.csv"
  model_path = "w2v_model.txt"
  
  allArgs = c(path2script, "english", corpus_path, model_path)# Build up args in a vector
  system2(command, args=allArgs) #run the python script
  
  # 3. Now we calculate coherence (how similar are words within a topic) and discrimination (how
  #    dissimilar are word of a topic compared to the words of all other topics)
  #    for more, see: https://www.fabriziogilardi.org/resources/papers/policy-diffusion-issue-definition.pdf
  
  #run w2v evaluation
  command = "C:/Users/noerber/AppData/Local/Programs/Python/Python37"
  path2script = "coherence.py"
  model_path = "w2v_model.txt"
  allArgs = c(path2script, model_path)
  system2(command, args=allArgs)
  
  # let's see which k is best...
  w2v <- read.csv("stm-evaluation.txt", header = F)
  colnames(w2v) <- c("coherence", "coherence.discrimination", "k")
  w2v[w2v$coherence.discrimination == max(w2v$coherence.discrimination),]
  # ...the topic model with k = 6
  
  #plot the evaluation (smoothed line not neat with only 4 models)
  library(ggplot2)
  plott <- ggplot(w2v, aes(x=k, y=coherence.discrimination)) +
    geom_point() +
    theme_bw() +
    geom_smooth(method = "loess", colour = "black", span = 0.5)
  ggsave(plot=plott, paste("./Results/Graphs/w2vnormde_evaluation.pdf"), height=5, width=8.09)
  
  
  ## ok, let's run the model for the optimal number of topics
  k <- 6
  STM <- stm(corpus_stm$documents, corpus_stm$vocab, K=k, as.formula(formula_prev),
             data = corpus_stm$meta, init.type="Spectral", verbose = T,
             control = list(nits = 100, burnin = 25))# Fit stm
  
  #save model output
  saveRDS(STM, "./Results/STMnormDE.rds")
  
  # Save 100 most probable words per topic
  words <- labelTopics(STM, n = 100)
  words <- as.data.frame(t(words[[1]]))
  write.table(words, file = "./Results/wordsnormde_6.txt", col.names = T, row.names = F, sep = "\t", quote = F)
  
  #save 5 most relevant documents per topic
  thoughts <- findThoughts(STM, texts = corpus_stm$meta$article_text, n = 5, topics = 1:k)
  thoughtsDocs <- data.frame(matrix(unlist(thoughts$docs), nrow=k, byrow=T))
  thoughtsIndices <- data.frame(matrix(unlist(thoughts$index), nrow=k, byrow=T))
  thoughts <- cbind(thoughtsDocs, thoughtsIndices)
  write.csv(thoughts, paste0("./Results/thoughtsnormde_", k, ".csv"),
            fileEncoding = "utf-8")
 
  #-> this document is very unhelpful and it seems that there are way more than 5 docs per topic
   
  # Write out probability, exclusivity and frequency of each word per topic
  require(matrixStats)
  
  #A helper: a James-Stein Estimator Shrinking to a Uniform Distribution
  #This draws from Hausser and Strimmer (2009) JMLR.
  js.estimate <- function(prob, ct) {
    if(ct<=1) {
      #basically if we only observe a count of 1
      #the variance goes to infinity and we get the uniform distribution.
      return(rep(1/length(prob), length(prob)))
    }
    # MLE of prob estimate
    mlvar <- prob*(1-prob)/(ct-1)
    unif <- rep(1/length(prob), length(prob)) 
    
    # Deviation from uniform
    deviation <- sum((prob-unif)^2)
    
    #take care of special case,if no difference it doesn't matter
    if(deviation==0) return(prob)
    
    lambda <- sum(mlvar)/deviation
    #if despite  our best efforts we ended up with an NaN number-just return the uniform distribution.
    if(is.nan(lambda)) return(unif)
    
    #truncate
    if(lambda>1) lambda <- 1
    if(lambda<0) lambda <- 0
    
    #Construct shrinkage estimator as convex combination of the two
    lambda*unif + (1 - lambda)*prob
  }
  safelog <- function(x) {
    out <- log(x)
    out[which(out< -1000)] <- -1000
    out
  }
  
  logbeta <- STM$beta$logbeta[[1]]
  topics <- 1:nrow(logbeta) 
  K <- STM$settings$dim$K
  wordcounts <- STM$settings$dim$wcounts$x
  excl <- t(t(logbeta) - colLogSumExps(logbeta))
  excl <- safelog(sapply(1:ncol(excl), function(x) js.estimate(exp(excl[,x]), wordcounts[x])))
  freqscore <- apply(logbeta,1,rank)/ncol(logbeta)
  freqscore <- as.data.frame(freqscore)
  colnames(freqscore) <- paste("frequency_", 1:K, sep = "")
  exclscore <- apply(excl,1,rank)/ncol(logbeta)
  exclscore <- as.data.frame(exclscore)
  colnames(exclscore) <- paste("exclusivity_", 1:K, sep = "")
  beta <- lapply(STM$beta$logbeta, exp)
  probscore <- as.data.frame(t(beta[[1]]))
  colnames(probscore) <- paste("probability_", 1:K, sep = "")
  scores <- cbind(probscore, exclscore, freqscore)
  scores$vocab <- STM$vocab
  
  outname <- paste0("./Results/scoresnormde_6.txt")
  write.table(scores, outname, col.names = T, row.names = F, sep = "\t", quote = F)
  
  
  # produce word-plots
  
  # Read data and check its structure
  words <- read.delim2("./Results/wordsnormde_6.txt")
  scores <- read.delim2("./Results/scoresnormde_6.txt")
  scores <- data.frame(scores)
  is.data.frame(scores)
  names(scores)
  head(scores)
  sapply(scores, mode)
  
  # render relevant variables numeric
  co <- ncol(scores) - 1
  scores[, c(1:co)] <- sapply(scores[, c(1:co)], as.numeric)
  head(scores)
  
  # Identify the Columns associated with probabilities, exclusivities and frequencies
  pr <- seq(1,co/3)
  ex <- seq(max(pr) + 1, max(pr) + co/3)
  fr <- seq(max(ex) + 1, max(ex) + co/3)
  
  # Plot and Save Top 50 (n) Words for each Topic (ncol(words)) by Prob, Excl and Freq
  n <- 50 
  for(i in 1:ncol(words)){
    topic <- data.frame(c(scores[, c(pr[i], ex[i], fr[i], co + 1)], i))
    colnames(topic) <- c("Probability", "Exclusivity", "Frequency", "Words", "Topic")
    topic <- topic[ topic[,4] %in% words[1:n,i] ,]
    pdf(file = paste("./Results/Graphs/Plain_TopicNormDE_", i, ".pdf", sep = ""), paper = "special", width=7, height=5.5)
    print(ggplot(data = topic, aes(x = Exclusivity, y = Frequency, label = Words)) + geom_text(aes(size = Probability)) + theme(legend.position = "none") + ggtitle(paste("Topic ", i, sep = "")))
    dev.off()
  }
  
  # correlations with covariates from the prevalence formula
  # weeks
  prep <- estimateEffect(1:k ~ s(week), STM, meta = corpus_stm$meta)
  x <- plot(prep, covariate = "week")
  dfs <- data.frame()
  for (i in 1:length(x$means)){
    df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
    df <- cbind(df, i, x$uvals)
    dfs <- rbind(dfs, df)
  }
  colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "WeeksPassed")
  outname <- paste0("./Results/effectsNormDE_", "WeeksPassed.txt")
  write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)
  
  #estimate actor type
  prep <- estimateEffect(1:k ~ actor.type, STM, meta = corpus_stm$meta)
  x <- plot(prep, covariate = "actor.type", model = STM, method="pointestimate")
  dfs <- data.frame()
  for (i in 1:length(x$means)){
    df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
    df <- cbind(df, i, x$uvals)
    dfs <- rbind(dfs, df)
  }
  colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "actor.type")
  outname <- paste0("./Results/effectsNormDE_", "ActorType_estimate.txt")
  write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)
  
  # difference actor type
  x <- plot(prep, covariate = "actor.type", model = STM, method = "difference", cov.value1="public (non-elected)", 
            cov.value2="public (elected)")
  dfs <- data.frame()
  for (i in 1:length(x$means)){
    df <- data.frame(x$means[[i]], x$cis[[i]][1], x$cis[[i]][2])
    df <- cbind(df, i, x$labels[[i]])
    dfs <- rbind(dfs, df)
  }
  colnames(dfs) <- c("diff_nonelec_elec", "lb", "ub", "topic", "actor.type")
  outname <- paste0("./Results/differenceNormDE_", "ActorType_diff.txt")
  write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)
  
  
  #####################
  # correlation plots 
  
  # correlation of topic prevalence with daily trend
  d <- data.frame(read.delim("./Results/effects_NormDEWeeksPassed.txt"))
  
  d <- d[order(d$topic, d$WeeksPassed),]
  length(d$WeeksPassed)
  max(d$WeeksPassed)
  mo <- seq(as.Date("2005-01-01"), by = "week", length.out = max(d$WeeksPassed))
  
  topic_labels <- 1:6 # can be used later to define substantial topics
  d$topicLabels <- NA
  d$date <- rep(mo, length(topic_labels))
  
  colnames(d)[5] <- c("var")
  
  d$pointEstimateAverage <- NA
  
  for(j in 1:length(topic_labels)){
    d$topicLabels[d$topic == j] <- topic_labels[j]
    d$pointEstimateAverage[d$topic == j] <- mean(d$pointEstimate[d$topic == j])
  }
  
  d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$pointEstimateAverage, decreasing = TRUE)]))
  
  pdf(file="./Results/Graphs/correlations-NormDEweekspassed.pdf", paper="special", width=12, height=9)
  print(ggplot(data = d, aes(x = date, y = pointEstimate)) +
          geom_line() +
          geom_hline(aes(yintercept = pointEstimateAverage), linetype = 2) +
          facet_wrap(~ topicLabels, ncol = 2) +
          labs(x = "weeks", y = "Topic prevalence", title = "") +
          theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm")) +
          scale_alpha(guide = "none") +
          geom_ribbon(aes(ymin = lb, ymax= ub, alpha = 0.5))) 
  dev.off()
  
  # correlation of topic prevalence with Actor Type
  file_name <- "./Results/effectsNormDE_ActorType_estimate.txt"
  var_label_long <- "Actor types in media reports"
  
  d <- data.frame(read.delim(file_name))
  var_label <- colnames(d)[5]
  colnames(d)[5] <- c("var")
  
  d$topicLabels <- NA
  d$pointEstimateAverage <- NA
  
  for(j in 1:length(topic_labels)){
    d$topicLabels[d$topic == j] <- topic_labels[j]
    d$pointEstimateAverage[d$topic == j] <- mean(d$pointEstimate[d$topic == j])
  }
  
  d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$pointEstimateAverage, decreasing = TRUE)]))
  #d$var <- as.factor(d$var)
  
  pdf(file=paste("./Results/Graphs/correlationsNormDE-actortype.pdf", sep=""), paper="special", width=7, height=9)
  print(
    ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
      geom_pointrange(position=position_dodge(width=0.5)) +
      coord_flip() +
      geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
      labs(x = "", y = "Topic prevalence", title = "") +
      theme(legend.position = "bottom", legend.title = element_blank())
  )
  dev.off()
  
  # difference of correlation of topic prevalence with tonality = positive and tonality = negative
  file_name <- "./Results/differenceNormDE_ActorType_diff.txt"
  var_label_long <- "Actor Types in media reports"
  
  d <- data.frame(read.delim(file_name))
  var_label <- colnames(d)[5]
  colnames(d)[5] <- c("var")
  
  #add topic labels
  d$topicLabels <- NA
  for(j in 1:length(topic_labels)){
    d$topicLabels[d$topic == j] <- topic_labels[j]
  }
  
  d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$diff_nonelec_elec, decreasing = TRUE)]))
  d$var <- as.factor(d$var)
  
  pdf(file=paste("./Results/Graphs/correlationsNormDE-diff-actortype.pdf", sep=""), paper="special", width=6.5, height=5)
  print(ggplot(data = d, aes(x = topicLabels, y = diff_nonelec_elec, ymin = lb, ymax = ub)) + #, color = groupLabels
          geom_pointrange() +
          coord_flip() +
          geom_hline(yintercept = 0, lty = 2, size = 0.5) +
          labs(x = "", y = "Difference in topic prevalence (non-elected - elected)", title = "") +
          theme(legend.position = "bottom", legend.title = element_blank())
  )
  dev.off()
  
}
