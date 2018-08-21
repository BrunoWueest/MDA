##### Media-driven accountability
# UK-PROTOTYPE
# bw, 11.4.2018, ms 17.08.2018

# clear workspace
rm(list = ls())

# set working directory
setwd("/Users/noerber/Desktop/PhD/MDA/Data/Media-Data/")
getwd()

### PREPARATION ###
#------------------

#load corpus
input <- readRDS("MDA_corpus-100418.RData")
unique(input$Media_Language) #what values does language have?
unique(input$Media_Country) #what values does country have?


corpus <- input[input$Media_Language=="EN",] #take only English articles
unique(corpus$Entity_Name) #what entities are present in the English sample?
corpus <- corpus[!is.na(corpus$Entity_Name),] #remove articles that are "NA" for entity name (~6)
unique(corpus$Tonality_Verbalized)
corpus <- corpus[!is.na(corpus$Tonality_Verbalized),] #remove articles that are "NA" for Tonality (~3), otherwise the following 
  #loop doesn't work (i.e. the cbind command at the end)

#recode several independent variables (basically: generate dummies from character vectors of interest)
vars <- c("Entity_Name",
          "Media_Type",
          "Media_Country",
          "Media_Source",
          "Tonality_Verbalized",
          "actor.type",
          "policy.scope",
          "territorial.scope",
          "policy.output"
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
unique(corpus[c("entity_id","entity_name")])

#entity_id                                                                        entity_name
#5202           1                                                 bank for international settlements
#4260           2                                             basel committee on banking supervision
#5953           3                                                                     bureau veritas
#7982           4                                                              cee bankwatch network
#190963         5                                 central commission for the navigation of the rhine
#4109           6                                                           commonwealth of nations 
#10494          7                                                                  council of europe
#14783          8                                                         ethical trading initiative
#3401           9                                                        euro free trade association
#23704         10                                          euro-mediterranean parliamentary assembly
#3592          11                                                         european banking authority
#304219        12                                                                european commission
#14806         13                                                       european competition network
#22623         14                                                             european economic area
#131957        15                                                        european environment agency
#54936         16                             european insurance and occupational pensions authority
#3338          17                                                                european parliament
#3667          18                                          european securities and markets authority
#3632          19                                                              european space agency
#65792         20 european union network for the implementation and enforcement of environmental law
#44608         21                                        extractive industry transparency initiative
#44684         22                                                             fair labor association
#25220         23                                                      financial stability institute
#39869         24                                                                      fitch ratings
#42768         25                                                         forest stewardship council
#36760         26                                                 francophone parliamentary assembly
#7330          28                                                               friends of the earth
#179993        30                                                      global food safety initiative
#37571         31                                                        global reporting initiative
#32856         32                                                                     global witness
#132002        34                                                     greater london authority (gla)
#93924         35                                                     igad inter-parliamentary union
#30246         36                                                          inter-parliamentary union
#202505        37                  intergovernmental organization for international carriage by rail
#51283         38                                 international association of insurance supervisors
#6510          39                                                  international chamber of commerce
#74892         40                                                  international competition network
#6062          41                                                       international criminal court
#63879         43                                    international diamond manufacturers association
#54045         44                          international federation of organic agriculture movements
#25242         45                                                   international labor organization
#65809         46                 international network for environmental compliance and enforcement
#29866         48                               international organization of securities commissions
#17265         49                                                   international whaling commission
#39167         50                                                        internet architecture board
#19653         51                        internet corporation for assigned names and numbers (icann)
#20848         52                                                    internet engineering task force
#14402         53                                                          internet governance forum
#14716         55                                                                   internet society
#111817        56                     interparliamentary assembly of the eurasian economic community
#104227        57         joint parliamentary assembly africa - caribbean - pacific - european union
#22067         58                                                                  kimberley process
#23090         60                                                         marine stewardship council
#3997          61                                                                            moody's
#3355          62                                organization for economic cooperation & development
#15450         63                                organization for security and cooperation in europe
#25384         64                                    parliamentary assembly of the council of europe
#35496         66                              programme for the endorsement of forest certification
#35568         67                                                                rainforest alliance
#77800         68                                             round table on sustainable development
#26174         69                                               sociÃ©tÃ© gÃ©nÃ©rale de surveillance
#3339          70                                                                  standard & poor's
#131495        73                                                               transport for london
#71236         74                                                                       tüv austria
#70625         77                                                                          tüv nord
#55017         78                                                                     tüv rheinland
#72285         79                                                                      tüv saarland
#43057         80                                                                          tüv süd
#70581         82                                                          underwriters laboratories
#131418        87                               west midlands integrated transport authority (wmita)
#23682         88                                                           worker rights consortium
#33605         89                                 world business council for sustainable development
#51290         90                                                              world diamond council
#49233         91                                                world federation of diamond bourses
#43941         92                                                          world wide web consortium


#Preparatory steps

# aggregate number of articles per week (or day, or month? and weight by number of words? if yes, how?)
min(corpus$article_date)
max(corpus$article_date)

require(lubridate)
days <- data.frame(days = seq(as.Date("2005-01-01"), as.Date("2016-01-03"), by="days")) #the period for which articles are available for the three entities
days$weeks <- week(days$days)
days$years <- year(days$days)

corpus$article_date <- as.Date(corpus$article_date)
corpus$n <- 1

# take the logged number of words for the weighted aggregation (?!)
hist(corpus$article_word_count)
corpus$n_w <- log1p(corpus$article_word_count) 
hist(corpus$n_w)
dat_base <- merge(corpus, days, by.x = "article_date", by.y = "days", all = T) #we use this dataset as a starting point for 
  #the following analyses, in which we subset the data for each individual governor to have the timeline and the peak times

#add zeros for days without coverage
dat_base$n[is.na(dat_base$n)] <- 0
dat_base$n_w[is.na(dat_base$n_w)] <- 0
dat_base$tonality_verbalized_negative[is.na(dat_base$tonality_verbalized_negative)] <- 0

### DESCRIPTIVE STATISTICS ###
#-----------------------------

#Number of Articles by Governor Characteristics
table(dat_base$entity_name)
act.typ <- table(dat_base$actor.type)
png(filename=paste("./Prototype_UK/Results/Graphs/ActorType.png", sep=""), width=9, height=6, units="in", res=600)
barplot(act.typ,main="Actor Type",ylab="Number of Articles")
dev.off()
terr_scop <- table(dat_base$territorial.scope)
png(filename=paste("./Prototype_UK/Results/Graphs/TerritorialScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(terr_scop,main="Territorial Scope",ylab="Number of Articles")
dev.off()
pol_scop <- table(dat_base$policy.scope)
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(pol_scop,main="Policy Scope",ylab="Number of Articles")
dev.off()
fun_scop <- table(dat_base$functional.scope..old.)
png(filename=paste("./Prototype_UK/Results/Graphs/FunctScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(fun_scop,main="Functional Scope",ylab="Number of Articles")
dev.off()
pol_out <- table(dat_base$policy.output)
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyOutput.png", sep=""), width=9, height=6, units="in", res=600)
barplot(pol_out,main="Policy Output",ylab="Number of Articles")
dev.off()

#Number of Articles by Media Characteristics
med.typ <- table(dat_base$media_type)
png(filename=paste("./Prototype_UK/Results/Graphs/MediaType.png", sep=""), width=9, height=6, units="in", res=600)
barplot(med.typ,main="Media Type",ylab="Number of Articles")
dev.off()


# Coverage (Salience and Tonality over all entities)
#---------

# get n of articles per week and n of articles weighted by article length (aggregated per week)
require(plyr)
dat <- ddply(dat_base, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat <- dat[order(dat$year, dat$weeks),]

# week 53 is odd, so lets get rid of it
dat <- dat[dat$week != 53,]

#do we have censored data?
hist(dat$n, breaks = 100)
hist(dat$n_w, breaks = 100)
hist(dat$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat$n)
adf.test(dat$n_w)
adf.test(dat$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat$ton_neg) # monthly seasonality (seas = 18; +/- 4.5 months); 


#Build a time series
dat$n_ts <- ts(dat$n)
dat$n_w_ts <- ts(dat$n_w)
dat$ton_neg_ts <- ts(dat$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat$n_ts_sm <- sma(dat$n_ts, order = 4)$fitted
dat$n_w_ts_sm <- sma(dat$n_w_ts, order = 4)$fitted
dat$ton_neg_ts_sm <- sma(dat$ton_neg_ts, order = 4)$fitted


# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat$week <- as.Date(paste(dat$years, dat$weeks, 1, sep="-"), "%Y-%U-%u")
plot(dat$week, dat$n_ts_sm, type = "l")
plot(dat$week, dat$n_w_ts_sm, type = "l")
plot(dat$week, dat$ton_neg_ts_sm, type = "l")


# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="Governors in English Coverage")
abline(h = mean(dat$n_ts_sm))
abline(h = mean(dat$n_ts_sm)+sd(dat$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat$n_ts_sm)-sd(dat$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",main="Governors in English Coverage")
abline(h = mean(dat$ton_neg_ts_sm))
abline(h = mean(dat$ton_neg_ts_sm)+sd(dat$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat$ton_neg_ts_sm)-sd(dat$ton_neg_ts_sm), lty = "dashed")
dev.off()

### SALIENCE ANd TONALITY GRAPHS BY INDIVIDUAL GOVERNOR ###
{
#bank for international settlements
#----------
dat_1 <- dat_base[dat_base$entity_id==1,]
require(plyr)
dat_1 <- ddply(dat_1, .(weeks, years), function (x) {
    data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_1 <- dat_1[order(dat_1$year, dat_1$weeks),]
  
# week 53 is odd as well, so lets get rid of it as well
dat_1 <- dat_1[dat_1$week != 53,]
dat_1 <- dat_1[!is.na(dat_1$week),]


#do we have censored data?
hist(dat_1$n, breaks = 100)
hist(dat_1$n_w, breaks = 100)
hist(dat_1$ton_neg, breaks=100)
  
# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_1$n)
adf.test(dat_1$n_w)
adf.test(dat_1$ton_neg)
# p-value is smaller than 0.01 => stationary time series
  
# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_1$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_1$n_w) # monthly seasonality (seas = 4; monthly); 
findfrequency(dat_1$ton_neg) # monthly seasonality (seas = 8; bi-monthly); 
  
#Build a time series
dat_1$n_ts <- ts(dat_1$n)
dat_1$n_w_ts <- ts(dat_1$n_w)
dat_1$ton_neg_ts <- ts(dat_1$ton_neg)
  
#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_1$n_ts_sm <- sma(dat_1$n_ts, order = 4)$fitted
dat_1$n_w_ts_sm <- sma(dat_1$n_w_ts, order = 4)$fitted
dat_1$ton_neg_ts_sm <- sma(dat_1$ton_neg_ts, order = 4)$fitted
  
# create starting date of every week
dat_1$week <- as.Date(paste(dat_1$years, dat_1$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
  #Salience
  png(filename=paste("./Prototype_UK/Results/Graphs/timeline_1.png", sep=""), width=9, height=6, units="in", res=600)
  plot(dat_1$week, dat_1$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="bank for international settlements")
  abline(h = mean(dat_1$n_ts_sm))
  abline(h = mean(dat_1$n_ts_sm)+sd(dat_1$n_ts_sm), lty = "dashed") #Standard Deviation lines
  abline(h = mean(dat_1$n_ts_sm)-sd(dat_1$n_ts_sm), lty = "dashed")
  dev.off()
  
  #Tonality
  png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_1.png", sep=""), width=9, height=6, units="in", res=600)
  plot(dat_1$week, dat_1$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
       main="bank for international settlements")
  abline(h = mean(dat_1$ton_neg_ts_sm))
  abline(h = mean(dat_1$ton_neg_ts_sm)+sd(dat_1$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
  abline(h = mean(dat_1$ton_neg_ts_sm)-sd(dat_1$ton_neg_ts_sm), lty = "dashed")
  dev.off()

#Variable for peak and low times
  #Salience
dat_1$peak <- 0
dat_1$peak[dat_1$n_ts_sm>mean(dat_1$n_ts_sm)+sd(dat_1$n_ts_sm)] <- 1
dat_1$peak[dat_1$n_ts_sm<mean(dat_1$n_ts_sm)-sd(dat_1$n_ts_sm)] <- -1
  #Tonality
dat_1$peak_neg <- 0
dat_1$peak_neg[dat_1$ton_neg_ts_sm>mean(dat_1$ton_neg_ts_sm)+sd(dat_1$ton_neg_ts_sm)] <- 1
dat_1$peak_neg[dat_1$ton_neg_ts_sm<mean(dat_1$ton_neg_ts_sm)-sd(dat_1$ton_neg_ts_sm)] <- -1

#QUESTION: Should we use the smoothed indicators for identfiying the peaks, or the initial ones?

#Entity-Identifier for merging
dat_1$entity_id <- 1

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_1$n <- NULL
dat_1$n_w <- NULL

#basel committee on banking supervision
#----------
dat_2 <- dat_base[dat_base$entity_id==2,]
require(plyr)
dat_2 <- ddply(dat_2, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_2 <- dat_2[order(dat_2$year, dat_2$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_2 <- dat_2[dat_2$week != 53,]
dat_2 <- dat_2[!is.na(dat_2$week),]


#do we have censored data?
hist(dat_2$n, breaks = 100)
hist(dat_2$n_w, breaks = 100)
hist(dat_2$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_2$n)
adf.test(dat_2$n_w)
adf.test(dat_2$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_2$n) # monthly seasonality (seas = 2; bi-weekly); 
findfrequency(dat_2$n_w) # monthly seasonality (seas = 2; bi-weekly); 
findfrequency(dat_2$ton_neg) # monthly seasonality (seas = 4; monthly); 

#Build a time series
dat_2$n_ts <- ts(dat_2$n)
dat_2$n_w_ts <- ts(dat_2$n_w)
dat_2$ton_neg_ts <- ts(dat_2$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_2$n_ts_sm <- sma(dat_2$n_ts, order = 4)$fitted
dat_2$n_w_ts_sm <- sma(dat_2$n_w_ts, order = 4)$fitted
dat_2$ton_neg_ts_sm <- sma(dat_2$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_2$week <- as.Date(paste(dat_2$years, dat_2$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_2.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_2$week, dat_2$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="basel committee on banking supervision")
abline(h = mean(dat_2$n_ts_sm))
abline(h = mean(dat_2$n_ts_sm)+sd(dat_2$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_2$n_ts_sm)-sd(dat_2$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_2.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_2$week, dat_2$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="basel committee on banking supervision")
abline(h = mean(dat_2$ton_neg_ts_sm))
abline(h = mean(dat_2$ton_neg_ts_sm)+sd(dat_2$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_2$ton_neg_ts_sm)-sd(dat_2$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_2$peak <- 0
dat_2$peak[dat_2$n_ts_sm>mean(dat_2$n_ts_sm)+sd(dat_2$n_ts_sm)] <- 1
dat_2$peak[dat_2$n_ts_sm<mean(dat_2$n_ts_sm)-sd(dat_2$n_ts_sm)] <- -1
#Tonality
dat_2$peak_neg <- 0
dat_2$peak_neg[dat_2$ton_neg_ts_sm>mean(dat_2$ton_neg_ts_sm)+sd(dat_2$ton_neg_ts_sm)] <- 1
dat_2$peak_neg[dat_2$ton_neg_ts_sm<mean(dat_2$ton_neg_ts_sm)-sd(dat_2$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_2$entity_id <- 2

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_2$n <- NULL
dat_2$n_w <- NULL


#bureau veritas
#----------
dat_3 <- dat_base[dat_base$entity_id==3,]
require(plyr)
dat_3 <- ddply(dat_3, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_3 <- dat_3[order(dat_3$year, dat_3$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_3 <- dat_3[dat_3$week != 53,]
dat_3 <- dat_3[!is.na(dat_3$week),]


#do we have censored data?
hist(dat_3$n, breaks = 100)
hist(dat_3$n_w, breaks = 100)
hist(dat_3$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_3$n)
adf.test(dat_3$n_w)
adf.test(dat_3$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_3$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_3$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_3$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_3$n_ts <- ts(dat_3$n)
dat_3$n_w_ts <- ts(dat_3$n_w)
dat_3$ton_neg_ts <- ts(dat_3$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_3$n_ts_sm <- sma(dat_3$n_ts, order = 4)$fitted
dat_3$n_w_ts_sm <- sma(dat_3$n_w_ts, order = 4)$fitted
dat_3$ton_neg_ts_sm <- sma(dat_3$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_3$week <- as.Date(paste(dat_3$years, dat_3$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_3.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_3$week, dat_3$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="bureau veritas")
abline(h = mean(dat_3$n_ts_sm))
abline(h = mean(dat_3$n_ts_sm)+sd(dat_3$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_3$n_ts_sm)-sd(dat_3$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_3.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_3$week, dat_3$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="bureau veritas")
abline(h = mean(dat_3$ton_neg_ts_sm))
abline(h = mean(dat_3$ton_neg_ts_sm)+sd(dat_3$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_3$ton_neg_ts_sm)-sd(dat_3$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_3$peak <- 0
dat_3$peak[dat_3$n_ts_sm>mean(dat_3$n_ts_sm)+sd(dat_3$n_ts_sm)] <- 1
dat_3$peak[dat_3$n_ts_sm<mean(dat_3$n_ts_sm)-sd(dat_3$n_ts_sm)] <- -1
#Tonality
dat_3$peak_neg <- 0
dat_3$peak_neg[dat_3$ton_neg_ts_sm>mean(dat_3$ton_neg_ts_sm)+sd(dat_3$ton_neg_ts_sm)] <- 1
dat_3$peak_neg[dat_3$ton_neg_ts_sm<mean(dat_3$ton_neg_ts_sm)-sd(dat_3$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_3$entity_id <- 3

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_3$n <- NULL
dat_3$n_w <- NULL

#cee bankwatch network
#----------
dat_4 <- dat_base[dat_base$entity_id==4,]
require(plyr)
dat_4 <- ddply(dat_4, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_4 <- dat_4[order(dat_4$year, dat_4$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_4 <- dat_4[dat_4$week != 53,]
dat_4 <- dat_4[!is.na(dat_4$week),]


#do we have censored data?
hist(dat_4$n, breaks = 100)
hist(dat_4$n_w, breaks = 100)
hist(dat_4$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_4$n) #not calculated
adf.test(dat_4$n_w)
adf.test(dat_4$ton_neg) #tonality is non-stationary!
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_4$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_4$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_4$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_4$n_ts <- ts(dat_4$n)
dat_4$n_w_ts <- ts(dat_4$n_w)
dat_4$ton_neg_ts <- ts(dat_4$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_4$n_ts_sm <- sma(dat_4$n_ts, order = 4)$fitted
dat_4$n_w_ts_sm <- sma(dat_4$n_w_ts, order = 4)$fitted
dat_4$ton_neg_ts_sm <- sma(dat_4$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_4$week <- as.Date(paste(dat_4$years, dat_4$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_4.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_4$week, dat_4$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="cee bankwatch network")
abline(h = mean(dat_4$n_ts_sm))
abline(h = mean(dat_4$n_ts_sm)+sd(dat_4$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_4$n_ts_sm)-sd(dat_4$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_4.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_4$week, dat_4$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="cee bankwatch network")
abline(h = mean(dat_4$ton_neg_ts_sm))
abline(h = mean(dat_4$ton_neg_ts_sm)+sd(dat_4$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_4$ton_neg_ts_sm)-sd(dat_4$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_4$peak <- 0
dat_4$peak[dat_4$n_ts_sm>mean(dat_4$n_ts_sm)+sd(dat_4$n_ts_sm)] <- 1
dat_4$peak[dat_4$n_ts_sm<mean(dat_4$n_ts_sm)-sd(dat_4$n_ts_sm)] <- -1
#Tonality
dat_4$peak_neg <- 0
dat_4$peak_neg[dat_4$ton_neg_ts_sm>mean(dat_4$ton_neg_ts_sm)+sd(dat_4$ton_neg_ts_sm)] <- 1
dat_4$peak_neg[dat_4$ton_neg_ts_sm<mean(dat_4$ton_neg_ts_sm)-sd(dat_4$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_4$entity_id <- 4

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_4$n <- NULL
dat_4$n_w <- NULL


#central commission for the navigation of the rhine: only 2 articles for this governor; cannot calculate ts-indicators,
#----------                                          duplicate normal variables for later dataset-binding to work
dat_5 <- dat_base[dat_base$entity_id==5,]
require(plyr)
dat_5 <- ddply(dat_5, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_5 <- dat_5[order(dat_5$year, dat_5$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_5 <- dat_5[dat_5$week != 53,]
dat_5 <- dat_5[!is.na(dat_5$week),]


#do we have censored data?
hist(dat_5$n, breaks = 100)
hist(dat_5$n_w, breaks = 100)
hist(dat_5$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_5$n_ts <- dat_5$n
dat_5$n_w_ts <- dat_5$n_w
dat_5$ton_neg_ts <- dat_5$ton_neg

#duplicate variable with different name
dat_5$n_ts_sm <- dat_5$n_ts
dat_5$n_w_ts_sm <- dat_5$n_w_ts
dat_5$ton_neg_ts_sm <- dat_5$ton_neg_ts

# create starting date of every week
dat_5$week <- as.Date(paste(dat_5$years, dat_5$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_5.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_5$week, dat_5$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="central commission for the navigation of the rhine")
abline(h = mean(dat_5$n_ts_sm))
abline(h = mean(dat_5$n_ts_sm)+sd(dat_5$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_5$n_ts_sm)-sd(dat_5$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_5.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_5$week, dat_5$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="central commission for the navigation of the rhine")
abline(h = mean(dat_5$ton_neg_ts_sm))
abline(h = mean(dat_5$ton_neg_ts_sm)+sd(dat_5$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_5$ton_neg_ts_sm)-sd(dat_5$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_5$peak <- 0
dat_5$peak[dat_5$n_ts_sm>mean(dat_5$n_ts_sm)+sd(dat_5$n_ts_sm)] <- 1
dat_5$peak[dat_5$n_ts_sm<mean(dat_5$n_ts_sm)-sd(dat_5$n_ts_sm)] <- -1
#Tonality
dat_5$peak_neg <- 0
dat_5$peak_neg[dat_5$ton_neg_ts_sm>mean(dat_5$ton_neg_ts_sm)+sd(dat_5$ton_neg_ts_sm)] <- 1
dat_5$peak_neg[dat_5$ton_neg_ts_sm<mean(dat_5$ton_neg_ts_sm)-sd(dat_5$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_5$entity_id <- 5

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_5$n <- NULL
dat_5$n_w <- NULL


#commonwealth of nations
#----------
dat_6 <- dat_base[dat_base$entity_id==6,]
require(plyr)
dat_6 <- ddply(dat_6, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_6 <- dat_6[order(dat_6$year, dat_6$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_6 <- dat_6[dat_6$week != 53,]
dat_6 <- dat_6[!is.na(dat_6$week),]


#do we have censored data?
hist(dat_6$n, breaks = 100)
hist(dat_6$n_w, breaks = 100)
hist(dat_6$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_6$n) #non-stationary
adf.test(dat_6$n_w) #non-stationary
adf.test(dat_6$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_6$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_6$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_6$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_6$n_ts <- ts(dat_6$n)
dat_6$n_w_ts <- ts(dat_6$n_w)
dat_6$ton_neg_ts <- ts(dat_6$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_6$n_ts_sm <- sma(dat_6$n_ts, order = 4)$fitted
dat_6$n_w_ts_sm <- sma(dat_6$n_w_ts, order = 4)$fitted
dat_6$ton_neg_ts_sm <- sma(dat_6$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_6$week <- as.Date(paste(dat_6$years, dat_6$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_6.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_6$week, dat_6$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="commonwealth of nations")
abline(h = mean(dat_6$n_ts_sm))
abline(h = mean(dat_6$n_ts_sm)+sd(dat_6$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_6$n_ts_sm)-sd(dat_6$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_6.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_6$week, dat_6$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="commonwealth of nations")
abline(h = mean(dat_6$ton_neg_ts_sm))
abline(h = mean(dat_6$ton_neg_ts_sm)+sd(dat_6$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_6$ton_neg_ts_sm)-sd(dat_6$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_6$peak <- 0
dat_6$peak[dat_6$n_ts_sm>mean(dat_6$n_ts_sm)+sd(dat_6$n_ts_sm)] <- 1
dat_6$peak[dat_6$n_ts_sm<mean(dat_6$n_ts_sm)-sd(dat_6$n_ts_sm)] <- -1
#Tonality
dat_6$peak_neg <- 0
dat_6$peak_neg[dat_6$ton_neg_ts_sm>mean(dat_6$ton_neg_ts_sm)+sd(dat_6$ton_neg_ts_sm)] <- 1
dat_6$peak_neg[dat_6$ton_neg_ts_sm<mean(dat_6$ton_neg_ts_sm)-sd(dat_6$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_6$entity_id <- 6

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_6$n <- NULL
dat_6$n_w <- NULL


#council of europe
#----------
dat_7 <- dat_base[dat_base$entity_id==7,]
require(plyr)
dat_7 <- ddply(dat_7, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_7 <- dat_7[order(dat_7$year, dat_7$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_7 <- dat_7[dat_7$week != 53,]
dat_7 <- dat_7[!is.na(dat_7$week),]


#do we have censored data?
hist(dat_7$n, breaks = 100)
hist(dat_7$n_w, breaks = 100)
hist(dat_7$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_7$n)
adf.test(dat_7$n_w)
adf.test(dat_7$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_7$n) # monthly seasonality (seas = 21; 5 months); 
findfrequency(dat_7$n_w) # monthly seasonality (seas = 20; 5 months); 
findfrequency(dat_7$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_7$n_ts <- ts(dat_7$n)
dat_7$n_w_ts <- ts(dat_7$n_w)
dat_7$ton_neg_ts <- ts(dat_7$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_7$n_ts_sm <- sma(dat_7$n_ts, order = 4)$fitted
dat_7$n_w_ts_sm <- sma(dat_7$n_w_ts, order = 4)$fitted
dat_7$ton_neg_ts_sm <- sma(dat_7$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_7$week <- as.Date(paste(dat_7$years, dat_7$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_7.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_7$week, dat_7$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="council of europe")
abline(h = mean(dat_7$n_ts_sm))
abline(h = mean(dat_7$n_ts_sm)+sd(dat_7$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_7$n_ts_sm)-sd(dat_7$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_7.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_7$week, dat_7$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="council of europe")
abline(h = mean(dat_7$ton_neg_ts_sm))
abline(h = mean(dat_7$ton_neg_ts_sm)+sd(dat_7$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_7$ton_neg_ts_sm)-sd(dat_7$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_7$peak <- 0
dat_7$peak[dat_7$n_ts_sm>mean(dat_7$n_ts_sm)+sd(dat_7$n_ts_sm)] <- 1
dat_7$peak[dat_7$n_ts_sm<mean(dat_7$n_ts_sm)-sd(dat_7$n_ts_sm)] <- -1
#Tonality
dat_7$peak_neg <- 0
dat_7$peak_neg[dat_7$ton_neg_ts_sm>mean(dat_7$ton_neg_ts_sm)+sd(dat_7$ton_neg_ts_sm)] <- 1
dat_7$peak_neg[dat_7$ton_neg_ts_sm<mean(dat_7$ton_neg_ts_sm)-sd(dat_7$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_7$entity_id <- 7

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_7$n <- NULL
dat_7$n_w <- NULL

#ethical trading initiative
#----------
dat_8 <- dat_base[dat_base$entity_id==8,]
require(plyr)
dat_8 <- ddply(dat_8, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_8 <- dat_8[order(dat_8$year, dat_8$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_8 <- dat_8[dat_8$week != 53,]
dat_8 <- dat_8[!is.na(dat_8$week),]


#do we have censored data?
hist(dat_8$n, breaks = 100)
hist(dat_8$n_w, breaks = 100)
hist(dat_8$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_8$n)
adf.test(dat_8$n_w)
adf.test(dat_8$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_8$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_8$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_8$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_8$n_ts <- ts(dat_8$n)
dat_8$n_w_ts <- ts(dat_8$n_w)
dat_8$ton_neg_ts <- ts(dat_8$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_8$n_ts_sm <- sma(dat_8$n_ts, order = 4)$fitted
dat_8$n_w_ts_sm <- sma(dat_8$n_w_ts, order = 4)$fitted
dat_8$ton_neg_ts_sm <- sma(dat_8$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_8$week <- as.Date(paste(dat_8$years, dat_8$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_8.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_8$week, dat_8$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="ethical trading initiative")
abline(h = mean(dat_8$n_ts_sm))
abline(h = mean(dat_8$n_ts_sm)+sd(dat_8$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_8$n_ts_sm)-sd(dat_8$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_8.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_8$week, dat_8$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="ethical trading initiative")
abline(h = mean(dat_8$ton_neg_ts_sm))
abline(h = mean(dat_8$ton_neg_ts_sm)+sd(dat_8$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_8$ton_neg_ts_sm)-sd(dat_8$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_8$peak <- 0
dat_8$peak[dat_8$n_ts_sm>mean(dat_8$n_ts_sm)+sd(dat_8$n_ts_sm)] <- 1
dat_8$peak[dat_8$n_ts_sm<mean(dat_8$n_ts_sm)-sd(dat_8$n_ts_sm)] <- -1
#Tonality
dat_8$peak_neg <- 0
dat_8$peak_neg[dat_8$ton_neg_ts_sm>mean(dat_8$ton_neg_ts_sm)+sd(dat_8$ton_neg_ts_sm)] <- 1
dat_8$peak_neg[dat_8$ton_neg_ts_sm<mean(dat_8$ton_neg_ts_sm)-sd(dat_8$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_8$entity_id <- 8

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_8$n <- NULL
dat_8$n_w <- NULL

#euro free trade association
#----------
dat_9 <- dat_base[dat_base$entity_id==9,]
require(plyr)
dat_9 <- ddply(dat_9, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_9 <- dat_9[order(dat_9$year, dat_9$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_9 <- dat_9[dat_9$week != 53,]
dat_9 <- dat_9[!is.na(dat_9$week),]


#do we have censored data?
hist(dat_9$n, breaks = 100)
hist(dat_9$n_w, breaks = 100)
hist(dat_9$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_9$n)
adf.test(dat_9$n_w)
adf.test(dat_9$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_9$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_9$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_9$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_9$n_ts <- ts(dat_9$n)
dat_9$n_w_ts <- ts(dat_9$n_w)
dat_9$ton_neg_ts <- ts(dat_9$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_9$n_ts_sm <- sma(dat_9$n_ts, order = 4)$fitted
dat_9$n_w_ts_sm <- sma(dat_9$n_w_ts, order = 4)$fitted
dat_9$ton_neg_ts_sm <- sma(dat_9$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_9$week <- as.Date(paste(dat_9$years, dat_9$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_9.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_9$week, dat_9$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="euro free trade association")
abline(h = mean(dat_9$n_ts_sm))
abline(h = mean(dat_9$n_ts_sm)+sd(dat_9$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_9$n_ts_sm)-sd(dat_9$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_9.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_9$week, dat_9$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="euro free trade association")
abline(h = mean(dat_9$ton_neg_ts_sm))
abline(h = mean(dat_9$ton_neg_ts_sm)+sd(dat_9$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_9$ton_neg_ts_sm)-sd(dat_9$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_9$peak <- 0
dat_9$peak[dat_9$n_ts_sm>mean(dat_9$n_ts_sm)+sd(dat_9$n_ts_sm)] <- 1
dat_9$peak[dat_9$n_ts_sm<mean(dat_9$n_ts_sm)-sd(dat_9$n_ts_sm)] <- -1
#Tonality
dat_9$peak_neg <- 0
dat_9$peak_neg[dat_9$ton_neg_ts_sm>mean(dat_9$ton_neg_ts_sm)+sd(dat_9$ton_neg_ts_sm)] <- 1
dat_9$peak_neg[dat_9$ton_neg_ts_sm<mean(dat_9$ton_neg_ts_sm)-sd(dat_9$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_9$entity_id <- 9

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_9$n <- NULL
dat_9$n_w <- NULL

#euro-mediterranean parliamentary assembly
#----------
dat_10 <- dat_base[dat_base$entity_id==10,]
require(plyr)
dat_10 <- ddply(dat_10, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_10 <- dat_10[order(dat_10$year, dat_10$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_10 <- dat_10[dat_10$week != 53,]
dat_10 <- dat_10[!is.na(dat_10$week),]


#do we have censored data?
hist(dat_10$n, breaks = 100)
hist(dat_10$n_w, breaks = 100)
hist(dat_10$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_10$n) #non-stationary
adf.test(dat_10$n_w) #non-stationary
adf.test(dat_10$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_10$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_10$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_10$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_10$n_ts <- ts(dat_10$n)
dat_10$n_w_ts <- ts(dat_10$n_w)
dat_10$ton_neg_ts <- ts(dat_10$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_10$n_ts_sm <- sma(dat_10$n_ts, order = 4)$fitted
dat_10$n_w_ts_sm <- sma(dat_10$n_w_ts, order = 4)$fitted
dat_10$ton_neg_ts_sm <- sma(dat_10$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_10$week <- as.Date(paste(dat_10$years, dat_10$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_10.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_10$week, dat_10$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="euro-mediterranean parliamentary assembly")
abline(h = mean(dat_10$n_ts_sm))
abline(h = mean(dat_10$n_ts_sm)+sd(dat_10$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_10$n_ts_sm)-sd(dat_10$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_10.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_10$week, dat_10$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="euro-mediterranean parliamentary assembly")
abline(h = mean(dat_10$ton_neg_ts_sm))
abline(h = mean(dat_10$ton_neg_ts_sm)+sd(dat_10$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_10$ton_neg_ts_sm)-sd(dat_10$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_10$peak <- 0
dat_10$peak[dat_10$n_ts_sm>mean(dat_10$n_ts_sm)+sd(dat_10$n_ts_sm)] <- 1
dat_10$peak[dat_10$n_ts_sm<mean(dat_10$n_ts_sm)-sd(dat_10$n_ts_sm)] <- -1
#Tonality
dat_10$peak_neg <- 0
dat_10$peak_neg[dat_10$ton_neg_ts_sm>mean(dat_10$ton_neg_ts_sm)+sd(dat_10$ton_neg_ts_sm)] <- 1
dat_10$peak_neg[dat_10$ton_neg_ts_sm<mean(dat_10$ton_neg_ts_sm)-sd(dat_10$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_10$entity_id <- 10

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_10$n <- NULL
dat_10$n_w <- NULL

#european banking authority
#----------
dat_11 <- dat_base[dat_base$entity_id==11,]
require(plyr)
dat_11 <- ddply(dat_11, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_11 <- dat_11[order(dat_11$year, dat_11$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_11 <- dat_11[dat_11$week != 53,]
dat_11 <- dat_11[!is.na(dat_11$week),]

#do we have censored data?
hist(dat_11$n, breaks = 100)
hist(dat_11$n_w, breaks = 100)
hist(dat_11$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_11$n)
adf.test(dat_11$n_w)
adf.test(dat_11$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_11$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_11$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_11$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_11$n_ts <- ts(dat_11$n)
dat_11$n_w_ts <- ts(dat_11$n_w)
dat_11$ton_neg_ts <- ts(dat_11$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_11$n_ts_sm <- sma(dat_11$n_ts, order = 4)$fitted
dat_11$n_w_ts_sm <- sma(dat_11$n_w_ts, order = 4)$fitted
dat_11$ton_neg_ts_sm <- sma(dat_11$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_11$week <- as.Date(paste(dat_11$years, dat_11$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_11.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_11$week, dat_11$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european banking authority")
abline(h = mean(dat_11$n_ts_sm))
abline(h = mean(dat_11$n_ts_sm)+sd(dat_11$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_11$n_ts_sm)-sd(dat_11$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_11.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_11$week, dat_11$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european banking authority")
abline(h = mean(dat_11$ton_neg_ts_sm))
abline(h = mean(dat_11$ton_neg_ts_sm)+sd(dat_11$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_11$ton_neg_ts_sm)-sd(dat_11$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_11$peak <- 0
dat_11$peak[dat_11$n_ts_sm>mean(dat_11$n_ts_sm)+sd(dat_11$n_ts_sm)] <- 1
dat_11$peak[dat_11$n_ts_sm<mean(dat_11$n_ts_sm)-sd(dat_11$n_ts_sm)] <- -1
#Tonality
dat_11$peak_neg <- 0
dat_11$peak_neg[dat_11$ton_neg_ts_sm>mean(dat_11$ton_neg_ts_sm)+sd(dat_11$ton_neg_ts_sm)] <- 1
dat_11$peak_neg[dat_11$ton_neg_ts_sm<mean(dat_11$ton_neg_ts_sm)-sd(dat_11$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_11$entity_id <- 11

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_11$n <- NULL
dat_11$n_w <- NULL

#european commission
#----------
dat_12 <- dat_base[dat_base$entity_id==12,]
require(plyr)
dat_12 <- ddply(dat_12, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_12 <- dat_12[order(dat_12$year, dat_12$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_12 <- dat_12[dat_12$week != 53,]
dat_12 <- dat_12[!is.na(dat_12$week),]

#do we have censored data?
hist(dat_12$n, breaks = 100)
hist(dat_12$n_w, breaks = 100)
hist(dat_12$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_12$n) #non-stationary
adf.test(dat_12$n_w) #non-stationary
adf.test(dat_12$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_12$n) # monthly seasonality (seas = 3; 3-weekly); 
findfrequency(dat_12$n_w) # monthly seasonality (seas = 3; 3-weekly); 
findfrequency(dat_12$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_12$n_ts <- ts(dat_12$n)
dat_12$n_w_ts <- ts(dat_12$n_w)
dat_12$ton_neg_ts <- ts(dat_12$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_12$n_ts_sm <- sma(dat_12$n_ts, order = 4)$fitted
dat_12$n_w_ts_sm <- sma(dat_12$n_w_ts, order = 4)$fitted
dat_12$ton_neg_ts_sm <- sma(dat_12$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_12$week <- as.Date(paste(dat_12$years, dat_12$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_12.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_12$week, dat_12$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european commission")
abline(h = mean(dat_12$n_ts_sm))
abline(h = mean(dat_12$n_ts_sm)+sd(dat_12$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_12$n_ts_sm)-sd(dat_12$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_12.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_12$week, dat_12$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european commission")
abline(h = mean(dat_12$ton_neg_ts_sm))
abline(h = mean(dat_12$ton_neg_ts_sm)+sd(dat_12$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_12$ton_neg_ts_sm)-sd(dat_12$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_12$peak <- 0
dat_12$peak[dat_12$n_ts_sm>mean(dat_12$n_ts_sm)+sd(dat_12$n_ts_sm)] <- 1
dat_12$peak[dat_12$n_ts_sm<mean(dat_12$n_ts_sm)-sd(dat_12$n_ts_sm)] <- -1
#Tonality
dat_12$peak_neg <- 0
dat_12$peak_neg[dat_12$ton_neg_ts_sm>mean(dat_12$ton_neg_ts_sm)+sd(dat_12$ton_neg_ts_sm)] <- 1
dat_12$peak_neg[dat_12$ton_neg_ts_sm<mean(dat_12$ton_neg_ts_sm)-sd(dat_12$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_12$entity_id <- 12

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_12$n <- NULL
dat_12$n_w <- NULL

#european competition network
#----------
dat_13 <- dat_base[dat_base$entity_id==13,]
require(plyr)
dat_13 <- ddply(dat_13, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_13 <- dat_13[order(dat_13$year, dat_13$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_13 <- dat_13[dat_13$week != 53,]
dat_13 <- dat_13[!is.na(dat_13$week),]

#do we have censored data?
hist(dat_13$n, breaks = 100)
hist(dat_13$n_w, breaks = 100)
hist(dat_13$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_13$n)
adf.test(dat_13$n_w)
adf.test(dat_13$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_13$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_13$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_13$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_13$n_ts <- ts(dat_13$n)
dat_13$n_w_ts <- ts(dat_13$n_w)
dat_13$ton_neg_ts <- ts(dat_13$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_13$n_ts_sm <- sma(dat_13$n_ts, order = 4)$fitted
dat_13$n_w_ts_sm <- sma(dat_13$n_w_ts, order = 4)$fitted
dat_13$ton_neg_ts_sm <- sma(dat_13$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_13$week <- as.Date(paste(dat_13$years, dat_13$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_13.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_13$week, dat_13$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european competition network")
abline(h = mean(dat_13$n_ts_sm))
abline(h = mean(dat_13$n_ts_sm)+sd(dat_13$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_13$n_ts_sm)-sd(dat_13$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_13.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_13$week, dat_13$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european competition network")
abline(h = mean(dat_13$ton_neg_ts_sm))
abline(h = mean(dat_13$ton_neg_ts_sm)+sd(dat_13$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_13$ton_neg_ts_sm)-sd(dat_13$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_13$peak <- 0
dat_13$peak[dat_13$n_ts_sm>mean(dat_13$n_ts_sm)+sd(dat_13$n_ts_sm)] <- 1
dat_13$peak[dat_13$n_ts_sm<mean(dat_13$n_ts_sm)-sd(dat_13$n_ts_sm)] <- -1
#Tonality
dat_13$peak_neg <- 0
dat_13$peak_neg[dat_13$ton_neg_ts_sm>mean(dat_13$ton_neg_ts_sm)+sd(dat_13$ton_neg_ts_sm)] <- 1
dat_13$peak_neg[dat_13$ton_neg_ts_sm<mean(dat_13$ton_neg_ts_sm)-sd(dat_13$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_13$entity_id <- 13

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_13$n <- NULL
dat_13$n_w <- NULL


#european economic area; QUESTION: is this really a political actor/governor?
#----------
dat_14 <- dat_base[dat_base$entity_id==14,]
require(plyr)
dat_14 <- ddply(dat_14, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_14 <- dat_14[order(dat_14$year, dat_14$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_14 <- dat_14[dat_14$week != 53,]
dat_14 <- dat_14[!is.na(dat_14$week),]

#do we have censored data?
hist(dat_14$n, breaks = 100)
hist(dat_14$n_w, breaks = 100)
hist(dat_14$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_14$n)
adf.test(dat_14$n_w)
adf.test(dat_14$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_14$n) # monthly seasonality (seas = 2; bi-weekly); 
findfrequency(dat_14$n_w) # monthly seasonality (seas = 2; bi-weekly); 
findfrequency(dat_14$ton_neg) # monthly seasonality (seas = 1 weekly); 

#Build a time series
dat_14$n_ts <- ts(dat_14$n)
dat_14$n_w_ts <- ts(dat_14$n_w)
dat_14$ton_neg_ts <- ts(dat_14$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_14$n_ts_sm <- sma(dat_14$n_ts, order = 4)$fitted
dat_14$n_w_ts_sm <- sma(dat_14$n_w_ts, order = 4)$fitted
dat_14$ton_neg_ts_sm <- sma(dat_14$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_14$week <- as.Date(paste(dat_14$years, dat_14$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_14.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_14$week, dat_14$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european economic area")
abline(h = mean(dat_14$n_ts_sm))
abline(h = mean(dat_14$n_ts_sm)+sd(dat_14$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_14$n_ts_sm)-sd(dat_14$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_14.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_14$week, dat_14$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european economic area")
abline(h = mean(dat_14$ton_neg_ts_sm))
abline(h = mean(dat_14$ton_neg_ts_sm)+sd(dat_14$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_14$ton_neg_ts_sm)-sd(dat_14$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_14$peak <- 0
dat_14$peak[dat_14$n_ts_sm>mean(dat_14$n_ts_sm)+sd(dat_14$n_ts_sm)] <- 1
dat_14$peak[dat_14$n_ts_sm<mean(dat_14$n_ts_sm)-sd(dat_14$n_ts_sm)] <- -1
#Tonality
dat_14$peak_neg <- 0
dat_14$peak_neg[dat_14$ton_neg_ts_sm>mean(dat_14$ton_neg_ts_sm)+sd(dat_14$ton_neg_ts_sm)] <- 1
dat_14$peak_neg[dat_14$ton_neg_ts_sm<mean(dat_14$ton_neg_ts_sm)-sd(dat_14$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_14$entity_id <- 14

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_14$n <- NULL
dat_14$n_w <- NULL

#european environment agency: only 2 articles in total, certain indicators (_ts) cannot be calculated; 
#----------                   still rename them to match other datasets
dat_15 <- dat_base[dat_base$entity_id==15,]
require(plyr)
dat_15 <- ddply(dat_15, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_15 <- dat_15[order(dat_15$year, dat_15$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_15 <- dat_15[dat_15$week != 53,]
dat_15 <- dat_15[!is.na(dat_15$week),]


#do we have censored data?
hist(dat_15$n, breaks = 100)
hist(dat_15$n_w, breaks = 100)
hist(dat_15$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_15$n_ts <- dat_15$n
dat_15$n_w_ts <- dat_15$n_w
dat_15$ton_neg_ts <- dat_15$ton_neg

#duplicate variable with different name
dat_15$n_ts_sm <- dat_15$n_ts
dat_15$n_w_ts_sm <- dat_15$n_w_ts
dat_15$ton_neg_ts_sm <- dat_15$ton_neg_ts

# create starting date of every week
dat_15$week <- as.Date(paste(dat_15$years, dat_15$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_15.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_15$week, dat_15$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european environment agency")
abline(h = mean(dat_15$n_ts_sm))
abline(h = mean(dat_15$n_ts_sm)+sd(dat_15$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_15$n_ts_sm)-sd(dat_15$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_15.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_15$week, dat_15$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european environment agency")
abline(h = mean(dat_15$ton_neg_ts_sm))
abline(h = mean(dat_15$ton_neg_ts_sm)+sd(dat_15$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_15$ton_neg_ts_sm)-sd(dat_15$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_15$peak <- 0
dat_15$peak[dat_15$n_ts_sm>mean(dat_15$n_ts_sm)+sd(dat_15$n_ts_sm)] <- 1
dat_15$peak[dat_15$n_ts_sm<mean(dat_15$n_ts_sm)-sd(dat_15$n_ts_sm)] <- -1
#Tonality
dat_15$peak_neg <- 0
dat_15$peak_neg[dat_15$ton_neg_ts_sm>mean(dat_15$ton_neg_ts_sm)+sd(dat_15$ton_neg_ts_sm)] <- 1
dat_15$peak_neg[dat_15$ton_neg_ts_sm<mean(dat_15$ton_neg_ts_sm)-sd(dat_15$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_15$entity_id <- 15

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_15$n <- NULL
dat_15$n_w <- NULL

#european insurance and occupational pensions authority
#----------
dat_16 <- dat_base[dat_base$entity_id==16,]
require(plyr)
dat_16 <- ddply(dat_16, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_16 <- dat_16[order(dat_16$year, dat_16$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_16 <- dat_16[dat_16$week != 53,]
dat_16 <- dat_16[!is.na(dat_16$week),]


#do we have censored data?
hist(dat_16$n, breaks = 100)
hist(dat_16$n_w, breaks = 100)
hist(dat_16$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_16$n) #non-stationary
adf.test(dat_16$n_w) #non-stationary
adf.test(dat_16$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_16$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_16$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_16$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_16$n_ts <- ts(dat_16$n)
dat_16$n_w_ts <- ts(dat_16$n_w)
dat_16$ton_neg_ts <- ts(dat_16$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_16$n_ts_sm <- sma(dat_16$n_ts, order = 4)$fitted
dat_16$n_w_ts_sm <- sma(dat_16$n_w_ts, order = 4)$fitted
dat_16$ton_neg_ts_sm <- sma(dat_16$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_16$week <- as.Date(paste(dat_16$years, dat_16$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_16.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_16$week, dat_16$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european insurance and occupational pensions authority")
abline(h = mean(dat_16$n_ts_sm))
abline(h = mean(dat_16$n_ts_sm)+sd(dat_16$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_16$n_ts_sm)-sd(dat_16$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_16.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_16$week, dat_16$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european insurance and occupational pensions authority")
abline(h = mean(dat_16$ton_neg_ts_sm))
abline(h = mean(dat_16$ton_neg_ts_sm)+sd(dat_16$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_16$ton_neg_ts_sm)-sd(dat_16$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_16$peak <- 0
dat_16$peak[dat_16$n_ts_sm>mean(dat_16$n_ts_sm)+sd(dat_16$n_ts_sm)] <- 1
dat_16$peak[dat_16$n_ts_sm<mean(dat_16$n_ts_sm)-sd(dat_16$n_ts_sm)] <- -1
#Tonality
dat_16$peak_neg <- 0
dat_16$peak_neg[dat_16$ton_neg_ts_sm>mean(dat_16$ton_neg_ts_sm)+sd(dat_16$ton_neg_ts_sm)] <- 1
dat_16$peak_neg[dat_16$ton_neg_ts_sm<mean(dat_16$ton_neg_ts_sm)-sd(dat_16$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_16$entity_id <- 16

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_16$n <- NULL
dat_16$n_w <- NULL

#european parliament
#----------
dat_17 <- dat_base[dat_base$entity_id==17,]
require(plyr)
dat_17 <- ddply(dat_17, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_17 <- dat_17[order(dat_17$year, dat_17$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_17 <- dat_17[dat_17$week != 53,]
dat_17 <- dat_17[!is.na(dat_17$week),]


#do we have censored data?
hist(dat_17$n, breaks = 100)
hist(dat_17$n_w, breaks = 100)
hist(dat_17$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_17$n)
adf.test(dat_17$n_w)
adf.test(dat_17$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_17$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_17$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_17$ton_neg) # monthly seasonality (seas = 5; +/- monthly); 

#Build a time series
dat_17$n_ts <- ts(dat_17$n)
dat_17$n_w_ts <- ts(dat_17$n_w)
dat_17$ton_neg_ts <- ts(dat_17$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_17$n_ts_sm <- sma(dat_17$n_ts, order = 4)$fitted
dat_17$n_w_ts_sm <- sma(dat_17$n_w_ts, order = 4)$fitted
dat_17$ton_neg_ts_sm <- sma(dat_17$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_17$week <- as.Date(paste(dat_17$years, dat_17$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_17.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_17$week, dat_17$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european parliament")
abline(h = mean(dat_17$n_ts_sm))
abline(h = mean(dat_17$n_ts_sm)+sd(dat_17$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_17$n_ts_sm)-sd(dat_17$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_17.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_17$week, dat_17$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european parliament")
abline(h = mean(dat_17$ton_neg_ts_sm))
abline(h = mean(dat_17$ton_neg_ts_sm)+sd(dat_17$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_17$ton_neg_ts_sm)-sd(dat_17$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_17$peak <- 0
dat_17$peak[dat_17$n_ts_sm>mean(dat_17$n_ts_sm)+sd(dat_17$n_ts_sm)] <- 1
dat_17$peak[dat_17$n_ts_sm<mean(dat_17$n_ts_sm)-sd(dat_17$n_ts_sm)] <- -1
#Tonality
dat_17$peak_neg <- 0
dat_17$peak_neg[dat_17$ton_neg_ts_sm>mean(dat_17$ton_neg_ts_sm)+sd(dat_17$ton_neg_ts_sm)] <- 1
dat_17$peak_neg[dat_17$ton_neg_ts_sm<mean(dat_17$ton_neg_ts_sm)-sd(dat_17$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_17$entity_id <- 17

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_17$n <- NULL
dat_17$n_w <- NULL


#european securities and markets authority
#----------
dat_18 <- dat_base[dat_base$entity_id==18,]
require(plyr)
dat_18 <- ddply(dat_base, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_18 <- dat_18[order(dat_18$year, dat_18$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_18 <- dat_18[dat_18$week != 53,]
dat_18 <- dat_18[!is.na(dat_18$week),]

#do we have censored data?
hist(dat_18$n, breaks = 100)
hist(dat_18$n_w, breaks = 100)
hist(dat_18$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_18$n) #non-stationary
adf.test(dat_18$n_w) #non-stationary
adf.test(dat_18$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_18$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_18$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_18$ton_neg) # monthly seasonality (seas = 6; 1.5 months); 

#Build a time series
dat_18$n_ts <- ts(dat_18$n)
dat_18$n_w_ts <- ts(dat_18$n_w)
dat_18$ton_neg_ts <- ts(dat_18$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_18$n_ts_sm <- sma(dat_18$n_ts, order = 4)$fitted
dat_18$n_w_ts_sm <- sma(dat_18$n_w_ts, order = 4)$fitted
dat_18$ton_neg_ts_sm <- sma(dat_18$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_18$week <- as.Date(paste(dat_18$years, dat_18$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_18.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_18$week, dat_18$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european securities and markets authority")
abline(h = mean(dat_18$n_ts_sm))
abline(h = mean(dat_18$n_ts_sm)+sd(dat_18$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_18$n_ts_sm)-sd(dat_18$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_18.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_18$week, dat_18$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european securities and markets authority")
abline(h = mean(dat_18$ton_neg_ts_sm))
abline(h = mean(dat_18$ton_neg_ts_sm)+sd(dat_18$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_18$ton_neg_ts_sm)-sd(dat_18$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_18$peak <- 0
dat_18$peak[dat_18$n_ts_sm>mean(dat_18$n_ts_sm)+sd(dat_18$n_ts_sm)] <- 1
dat_18$peak[dat_18$n_ts_sm<mean(dat_18$n_ts_sm)-sd(dat_18$n_ts_sm)] <- -1
#Tonality
dat_18$peak_neg <- 0
dat_18$peak_neg[dat_18$ton_neg_ts_sm>mean(dat_18$ton_neg_ts_sm)+sd(dat_18$ton_neg_ts_sm)] <- 1
dat_18$peak_neg[dat_18$ton_neg_ts_sm<mean(dat_18$ton_neg_ts_sm)-sd(dat_18$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_18$entity_id <- 18

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_18$n <- NULL
dat_18$n_w <- NULL

#european space agency
#----------
dat_19 <- dat_base[dat_base$entity_id==19,]
require(plyr)
dat_19 <- ddply(dat_19, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_19 <- dat_19[order(dat_19$year, dat_19$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_19 <- dat_19[dat_19$week != 53,]
dat_19 <- dat_19[!is.na(dat_19$week),]


#do we have censored data?
hist(dat_19$n, breaks = 100)
hist(dat_19$n_w, breaks = 100)
hist(dat_19$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_19$n)
adf.test(dat_19$n_w)
adf.test(dat_19$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_19$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_19$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_19$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_19$n_ts <- ts(dat_19$n)
dat_19$n_w_ts <- ts(dat_19$n_w)
dat_19$ton_neg_ts <- ts(dat_19$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_19$n_ts_sm <- sma(dat_19$n_ts, order = 4)$fitted
dat_19$n_w_ts_sm <- sma(dat_19$n_w_ts, order = 4)$fitted
dat_19$ton_neg_ts_sm <- sma(dat_19$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_19$week <- as.Date(paste(dat_19$years, dat_19$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_19.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_19$week, dat_19$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european space agency")
abline(h = mean(dat_19$n_ts_sm))
abline(h = mean(dat_19$n_ts_sm)+sd(dat_19$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_19$n_ts_sm)-sd(dat_19$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_19.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_19$week, dat_19$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european space agency")
abline(h = mean(dat_19$ton_neg_ts_sm))
abline(h = mean(dat_19$ton_neg_ts_sm)+sd(dat_19$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_19$ton_neg_ts_sm)-sd(dat_19$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_19$peak <- 0
dat_19$peak[dat_19$n_ts_sm>mean(dat_19$n_ts_sm)+sd(dat_19$n_ts_sm)] <- 1
dat_19$peak[dat_19$n_ts_sm<mean(dat_19$n_ts_sm)-sd(dat_19$n_ts_sm)] <- -1
#Tonality
dat_19$peak_neg <- 0
dat_19$peak_neg[dat_19$ton_neg_ts_sm>mean(dat_19$ton_neg_ts_sm)+sd(dat_19$ton_neg_ts_sm)] <- 1
dat_19$peak_neg[dat_19$ton_neg_ts_sm<mean(dat_19$ton_neg_ts_sm)-sd(dat_19$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_19$entity_id <- 19

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_19$n <- NULL
dat_19$n_w <- NULL


#european union network for the implementation and enforcement of environmental law: only 9 weeks of coverage (~30 articles)
#----------
dat_20 <- dat_base[dat_base$entity_id==20,]
require(plyr)
dat_20 <- ddply(dat_20, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_20 <- dat_20[order(dat_20$year, dat_20$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_20 <- dat_20[dat_20$week != 53,]
dat_20 <- dat_20[!is.na(dat_20$week),]


#do we have censored data?
hist(dat_20$n, breaks = 100)
hist(dat_20$n_w, breaks = 100)
hist(dat_20$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_20$n)
adf.test(dat_20$n_w) 
adf.test(dat_20$ton_neg) #not calculated, no article with negative tonality
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_20$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_20$n_w) # monthly seasonality (seas = 1; weekly); 
#findfrequency(dat_20$ton_neg) # not calculated, no article with negative tonality; 

#Build a time series
dat_20$n_ts <- ts(dat_20$n)
dat_20$n_w_ts <- ts(dat_20$n_w)
dat_20$ton_neg_ts <- ts(dat_20$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_20$n_ts_sm <- sma(dat_20$n_ts, order = 4)$fitted
dat_20$n_w_ts_sm <- sma(dat_20$n_w_ts, order = 4)$fitted
dat_20$ton_neg_ts_sm <- sma(dat_20$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_20$week <- as.Date(paste(dat_20$years, dat_20$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_20.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_20$week, dat_20$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="european union network for the implementation and enforcement of environmental law")
abline(h = mean(dat_20$n_ts_sm))
abline(h = mean(dat_20$n_ts_sm)+sd(dat_20$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_20$n_ts_sm)-sd(dat_20$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_20.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_20$week, dat_20$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="european union network for the implementation and enforcement of environmental law")
abline(h = mean(dat_20$ton_neg_ts_sm))
abline(h = mean(dat_20$ton_neg_ts_sm)+sd(dat_20$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_20$ton_neg_ts_sm)-sd(dat_20$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_20$peak <- 0
dat_20$peak[dat_20$n_ts_sm>mean(dat_20$n_ts_sm)+sd(dat_20$n_ts_sm)] <- 1
dat_20$peak[dat_20$n_ts_sm<mean(dat_20$n_ts_sm)-sd(dat_20$n_ts_sm)] <- -1
#Tonality
dat_20$peak_neg <- 0
dat_20$peak_neg[dat_20$ton_neg_ts_sm>mean(dat_20$ton_neg_ts_sm)+sd(dat_20$ton_neg_ts_sm)] <- 1
dat_20$peak_neg[dat_20$ton_neg_ts_sm<mean(dat_20$ton_neg_ts_sm)-sd(dat_20$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_20$entity_id <- 20

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_20$n <- NULL
dat_20$n_w <- NULL


#extractive industry transparency initiative
#----------
dat_21 <- dat_base[dat_base$entity_id==21,]
require(plyr)
dat_21 <- ddply(dat_21, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_21 <- dat_21[order(dat_21$year, dat_21$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_21 <- dat_21[dat_21$week != 53,]
dat_21 <- dat_21[!is.na(dat_21$week),]


#do we have censored data?
hist(dat_21$n, breaks = 100)
hist(dat_21$n_w, breaks = 100)
hist(dat_21$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_21$n)
adf.test(dat_21$n_w)
adf.test(dat_21$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_21$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_21$n_w) # monthly seasonality (seas = 6; 1.5 monthly); 
findfrequency(dat_21$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_21$n_ts <- ts(dat_21$n)
dat_21$n_w_ts <- ts(dat_21$n_w)
dat_21$ton_neg_ts <- ts(dat_21$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_21$n_ts_sm <- sma(dat_21$n_ts, order = 4)$fitted
dat_21$n_w_ts_sm <- sma(dat_21$n_w_ts, order = 4)$fitted
dat_21$ton_neg_ts_sm <- sma(dat_21$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_21$week <- as.Date(paste(dat_21$years, dat_21$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_21.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_21$week, dat_21$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="extractive industry transparency initiative")
abline(h = mean(dat_21$n_ts_sm))
abline(h = mean(dat_21$n_ts_sm)+sd(dat_21$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_21$n_ts_sm)-sd(dat_21$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_21.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_21$week, dat_21$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="extractive industry transparency initiative")
abline(h = mean(dat_21$ton_neg_ts_sm))
abline(h = mean(dat_21$ton_neg_ts_sm)+sd(dat_21$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_21$ton_neg_ts_sm)-sd(dat_21$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_21$peak <- 0
dat_21$peak[dat_21$n_ts_sm>mean(dat_21$n_ts_sm)+sd(dat_21$n_ts_sm)] <- 1
dat_21$peak[dat_21$n_ts_sm<mean(dat_21$n_ts_sm)-sd(dat_21$n_ts_sm)] <- -1
#Tonality
dat_21$peak_neg <- 0
dat_21$peak_neg[dat_21$ton_neg_ts_sm>mean(dat_21$ton_neg_ts_sm)+sd(dat_21$ton_neg_ts_sm)] <- 1
dat_21$peak_neg[dat_21$ton_neg_ts_sm<mean(dat_21$ton_neg_ts_sm)-sd(dat_21$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_21$entity_id <- 21

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_21$n <- NULL
dat_21$n_w <- NULL


#fair labor association
#----------
dat_22 <- dat_base[dat_base$entity_id==22,]
require(plyr)
dat_22 <- ddply(dat_22, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_22 <- dat_22[order(dat_22$year, dat_22$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_22 <- dat_22[dat_22$week != 53,]
dat_22 <- dat_22[!is.na(dat_22$week),]


#do we have censored data?
hist(dat_22$n, breaks = 100)
hist(dat_22$n_w, breaks = 100)
hist(dat_22$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_22$n) #non-stationary
adf.test(dat_22$n_w) #non-stationary
adf.test(dat_22$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_22$n) # monthly seasonality (seas = 16; tri-monthly); 
findfrequency(dat_22$n_w) # monthly seasonality (seas = 16; tri-monthly); 
findfrequency(dat_22$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_22$n_ts <- ts(dat_22$n)
dat_22$n_w_ts <- ts(dat_22$n_w)
dat_22$ton_neg_ts <- ts(dat_22$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_22$n_ts_sm <- sma(dat_22$n_ts, order = 4)$fitted
dat_22$n_w_ts_sm <- sma(dat_22$n_w_ts, order = 4)$fitted
dat_22$ton_neg_ts_sm <- sma(dat_22$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_22$week <- as.Date(paste(dat_22$years, dat_22$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_22.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_22$week, dat_22$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="fair labor association")
abline(h = mean(dat_22$n_ts_sm))
abline(h = mean(dat_22$n_ts_sm)+sd(dat_22$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_22$n_ts_sm)-sd(dat_22$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_22.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_22$week, dat_22$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="fair labor association")
abline(h = mean(dat_22$ton_neg_ts_sm))
abline(h = mean(dat_22$ton_neg_ts_sm)+sd(dat_22$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_22$ton_neg_ts_sm)-sd(dat_22$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_22$peak <- 0
dat_22$peak[dat_22$n_ts_sm>mean(dat_22$n_ts_sm)+sd(dat_22$n_ts_sm)] <- 1
dat_22$peak[dat_22$n_ts_sm<mean(dat_22$n_ts_sm)-sd(dat_22$n_ts_sm)] <- -1
#Tonality
dat_22$peak_neg <- 0
dat_22$peak_neg[dat_22$ton_neg_ts_sm>mean(dat_22$ton_neg_ts_sm)+sd(dat_22$ton_neg_ts_sm)] <- 1
dat_22$peak_neg[dat_22$ton_neg_ts_sm<mean(dat_22$ton_neg_ts_sm)-sd(dat_22$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_22$entity_id <- 22

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_22$n <- NULL
dat_22$n_w <- NULL


#financial stability institute
#----------
dat_23 <- dat_base[dat_base$entity_id==23,]
require(plyr)
dat_23 <- ddply(dat_23, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_23 <- dat_23[order(dat_23$year, dat_23$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_23 <- dat_23[dat_23$week != 53,]
dat_23 <- dat_23[!is.na(dat_23$week),]


#do we have censored data?
hist(dat_23$n, breaks = 100)
hist(dat_23$n_w, breaks = 100)
hist(dat_23$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_23$n)
adf.test(dat_23$n_w)
adf.test(dat_23$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_23$n) # monthly seasonality (seas = 4; monthly); 
findfrequency(dat_23$n_w) # monthly seasonality (seas = 4; monthly); 
findfrequency(dat_23$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_23$n_ts <- ts(dat_23$n)
dat_23$n_w_ts <- ts(dat_23$n_w)
dat_23$ton_neg_ts <- ts(dat_23$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_23$n_ts_sm <- sma(dat_23$n_ts, order = 4)$fitted
dat_23$n_w_ts_sm <- sma(dat_23$n_w_ts, order = 4)$fitted
dat_23$ton_neg_ts_sm <- sma(dat_23$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_23$week <- as.Date(paste(dat_23$years, dat_23$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_23.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_23$week, dat_23$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="financial stability institute")
abline(h = mean(dat_23$n_ts_sm))
abline(h = mean(dat_23$n_ts_sm)+sd(dat_23$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_23$n_ts_sm)-sd(dat_23$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_23.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_23$week, dat_23$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="financial stability institute")
abline(h = mean(dat_23$ton_neg_ts_sm))
abline(h = mean(dat_23$ton_neg_ts_sm)+sd(dat_23$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_23$ton_neg_ts_sm)-sd(dat_23$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_23$peak <- 0
dat_23$peak[dat_23$n_ts_sm>mean(dat_23$n_ts_sm)+sd(dat_23$n_ts_sm)] <- 1
dat_23$peak[dat_23$n_ts_sm<mean(dat_23$n_ts_sm)-sd(dat_23$n_ts_sm)] <- -1
#Tonality
dat_23$peak_neg <- 0
dat_23$peak_neg[dat_23$ton_neg_ts_sm>mean(dat_23$ton_neg_ts_sm)+sd(dat_23$ton_neg_ts_sm)] <- 1
dat_23$peak_neg[dat_23$ton_neg_ts_sm<mean(dat_23$ton_neg_ts_sm)-sd(dat_23$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_23$entity_id <- 23

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_23$n <- NULL
dat_23$n_w <- NULL


#fitch ratings
#----------
dat_24 <- dat_base[dat_base$entity_id==24,]
require(plyr)
dat_24 <- ddply(dat_24, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_24 <- dat_24[order(dat_24$year, dat_24$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_24 <- dat_24[dat_24$week != 53,]
dat_24 <- dat_24[!is.na(dat_24$week),]


#do we have censored data?
hist(dat_24$n, breaks = 100)
hist(dat_24$n_w, breaks = 100)
hist(dat_24$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_24$n)
adf.test(dat_24$n_w)
adf.test(dat_24$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_24$n) # monthly seasonality (seas = 4; monthly); 
findfrequency(dat_24$n_w) # monthly seasonality (seas = 10; 2.5 monthly); 
findfrequency(dat_24$ton_neg) # monthly seasonality (seas = 10; 2.5 monthly); 

#Build a time series
dat_24$n_ts <- ts(dat_24$n)
dat_24$n_w_ts <- ts(dat_24$n_w)
dat_24$ton_neg_ts <- ts(dat_24$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_24$n_ts_sm <- sma(dat_24$n_ts, order = 4)$fitted
dat_24$n_w_ts_sm <- sma(dat_24$n_w_ts, order = 4)$fitted
dat_24$ton_neg_ts_sm <- sma(dat_24$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_24$week <- as.Date(paste(dat_24$years, dat_24$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_24.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_24$week, dat_24$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="fitch ratings")
abline(h = mean(dat_24$n_ts_sm))
abline(h = mean(dat_24$n_ts_sm)+sd(dat_24$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_24$n_ts_sm)-sd(dat_24$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_24.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_24$week, dat_24$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="fitch ratings")
abline(h = mean(dat_24$ton_neg_ts_sm))
abline(h = mean(dat_24$ton_neg_ts_sm)+sd(dat_24$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_24$ton_neg_ts_sm)-sd(dat_24$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_24$peak <- 0
dat_24$peak[dat_24$n_ts_sm>mean(dat_24$n_ts_sm)+sd(dat_24$n_ts_sm)] <- 1
dat_24$peak[dat_24$n_ts_sm<mean(dat_24$n_ts_sm)-sd(dat_24$n_ts_sm)] <- -1
#Tonality
dat_24$peak_neg <- 0
dat_24$peak_neg[dat_24$ton_neg_ts_sm>mean(dat_24$ton_neg_ts_sm)+sd(dat_24$ton_neg_ts_sm)] <- 1
dat_24$peak_neg[dat_24$ton_neg_ts_sm<mean(dat_24$ton_neg_ts_sm)-sd(dat_24$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_24$entity_id <- 24

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_24$n <- NULL
dat_24$n_w <- NULL

#forest stewardship council
#----------
dat_25 <- dat_base[dat_base$entity_id==25,]
require(plyr)
dat_25 <- ddply(dat_25, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_25 <- dat_25[order(dat_25$year, dat_25$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_25 <- dat_25[dat_25$week != 53,]
dat_25 <- dat_25[!is.na(dat_25$week),]


#do we have censored data?
hist(dat_25$n, breaks = 100)
hist(dat_25$n_w, breaks = 100)
hist(dat_25$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_25$n)
adf.test(dat_25$n_w)
adf.test(dat_25$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_25$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_25$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_25$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_25$n_ts <- ts(dat_25$n)
dat_25$n_w_ts <- ts(dat_25$n_w)
dat_25$ton_neg_ts <- ts(dat_25$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_25$n_ts_sm <- sma(dat_25$n_ts, order = 4)$fitted
dat_25$n_w_ts_sm <- sma(dat_25$n_w_ts, order = 4)$fitted
dat_25$ton_neg_ts_sm <- sma(dat_25$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_25$week <- as.Date(paste(dat_25$years, dat_25$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_25.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_25$week, dat_25$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="forest stewardship council")
abline(h = mean(dat_25$n_ts_sm))
abline(h = mean(dat_25$n_ts_sm)+sd(dat_25$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_25$n_ts_sm)-sd(dat_25$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_25.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_25$week, dat_25$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="forest stewardship council")
abline(h = mean(dat_25$ton_neg_ts_sm))
abline(h = mean(dat_25$ton_neg_ts_sm)+sd(dat_25$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_25$ton_neg_ts_sm)-sd(dat_25$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_25$peak <- 0
dat_25$peak[dat_25$n_ts_sm>mean(dat_25$n_ts_sm)+sd(dat_25$n_ts_sm)] <- 1
dat_25$peak[dat_25$n_ts_sm<mean(dat_25$n_ts_sm)-sd(dat_25$n_ts_sm)] <- -1
#Tonality
dat_25$peak_neg <- 0
dat_25$peak_neg[dat_25$ton_neg_ts_sm>mean(dat_25$ton_neg_ts_sm)+sd(dat_25$ton_neg_ts_sm)] <- 1
dat_25$peak_neg[dat_25$ton_neg_ts_sm<mean(dat_25$ton_neg_ts_sm)-sd(dat_25$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_25$entity_id <- 25

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_25$n <- NULL
dat_25$n_w <- NULL

#francophone parliamentary assembly
#----------
dat_26 <- dat_base[dat_base$entity_id==26,]
require(plyr)
dat_26 <- ddply(dat_26, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_26 <- dat_26[order(dat_26$year, dat_26$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_26 <- dat_26[dat_26$week != 53,]
dat_26 <- dat_26[!is.na(dat_26$week),]


#do we have censored data?
hist(dat_26$n, breaks = 100)
hist(dat_26$n_w, breaks = 100)
hist(dat_26$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_26$n)
adf.test(dat_26$n_w)
adf.test(dat_26$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_26$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_26$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_26$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_26$n_ts <- ts(dat_26$n)
dat_26$n_w_ts <- ts(dat_26$n_w)
dat_26$ton_neg_ts <- ts(dat_26$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_26$n_ts_sm <- sma(dat_26$n_ts, order = 4)$fitted
dat_26$n_w_ts_sm <- sma(dat_26$n_w_ts, order = 4)$fitted
dat_26$ton_neg_ts_sm <- sma(dat_26$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_26$week <- as.Date(paste(dat_26$years, dat_26$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_26.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_26$week, dat_26$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="francophone parliamentary assembly")
abline(h = mean(dat_26$n_ts_sm))
abline(h = mean(dat_26$n_ts_sm)+sd(dat_26$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_26$n_ts_sm)-sd(dat_26$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_26.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_26$week, dat_26$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="francophone parliamentary assembly")
abline(h = mean(dat_26$ton_neg_ts_sm))
abline(h = mean(dat_26$ton_neg_ts_sm)+sd(dat_26$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_26$ton_neg_ts_sm)-sd(dat_26$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_26$peak <- 0
dat_26$peak[dat_26$n_ts_sm>mean(dat_26$n_ts_sm)+sd(dat_26$n_ts_sm)] <- 1
dat_26$peak[dat_26$n_ts_sm<mean(dat_26$n_ts_sm)-sd(dat_26$n_ts_sm)] <- -1
#Tonality
dat_26$peak_neg <- 0
dat_26$peak_neg[dat_26$ton_neg_ts_sm>mean(dat_26$ton_neg_ts_sm)+sd(dat_26$ton_neg_ts_sm)] <- 1
dat_26$peak_neg[dat_26$ton_neg_ts_sm<mean(dat_26$ton_neg_ts_sm)-sd(dat_26$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_26$entity_id <- 26

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_26$n <- NULL
dat_26$n_w <- NULL

#friends of the earth
#----------
dat_28 <- dat_base[dat_base$entity_id==28,]
require(plyr)
dat_28 <- ddply(dat_28, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_28 <- dat_28[order(dat_28$year, dat_28$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_28 <- dat_28[dat_28$week != 53,]
dat_28 <- dat_28[!is.na(dat_28$week),]


#do we have censored data?
hist(dat_28$n, breaks = 100)
hist(dat_28$n_w, breaks = 100)
hist(dat_28$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_28$n)
adf.test(dat_28$n_w)
adf.test(dat_28$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_28$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_28$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_28$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_28$n_ts <- ts(dat_28$n)
dat_28$n_w_ts <- ts(dat_28$n_w)
dat_28$ton_neg_ts <- ts(dat_28$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_28$n_ts_sm <- sma(dat_28$n_ts, order = 4)$fitted
dat_28$n_w_ts_sm <- sma(dat_28$n_w_ts, order = 4)$fitted
dat_28$ton_neg_ts_sm <- sma(dat_28$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_28$week <- as.Date(paste(dat_28$years, dat_28$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_28.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_28$week, dat_28$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="friends of the earth")
abline(h = mean(dat_28$n_ts_sm))
abline(h = mean(dat_28$n_ts_sm)+sd(dat_28$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_28$n_ts_sm)-sd(dat_28$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_28.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_28$week, dat_28$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="friends of the earth")
abline(h = mean(dat_28$ton_neg_ts_sm))
abline(h = mean(dat_28$ton_neg_ts_sm)+sd(dat_28$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_28$ton_neg_ts_sm)-sd(dat_28$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_28$peak <- 0
dat_28$peak[dat_28$n_ts_sm>mean(dat_28$n_ts_sm)+sd(dat_28$n_ts_sm)] <- 1
dat_28$peak[dat_28$n_ts_sm<mean(dat_28$n_ts_sm)-sd(dat_28$n_ts_sm)] <- -1
#Tonality
dat_28$peak_neg <- 0
dat_28$peak_neg[dat_28$ton_neg_ts_sm>mean(dat_28$ton_neg_ts_sm)+sd(dat_28$ton_neg_ts_sm)] <- 1
dat_28$peak_neg[dat_28$ton_neg_ts_sm<mean(dat_28$ton_neg_ts_sm)-sd(dat_28$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_28$entity_id <- 28

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_28$n <- NULL
dat_28$n_w <- NULL

#global food safety initiative: 16 weeks with coverage, ~20 articles
#----------
dat_30 <- dat_base[dat_base$entity_id==30,]
require(plyr)
dat_30 <- ddply(dat_30, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_30 <- dat_30[order(dat_30$year, dat_30$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_30 <- dat_30[dat_30$week != 53,]
dat_30 <- dat_30[!is.na(dat_30$week),]

#do we have censored data?
hist(dat_30$n, breaks = 100)
hist(dat_30$n_w, breaks = 100)
hist(dat_30$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_30$n)
adf.test(dat_30$n_w) #non-stationary
adf.test(dat_30$ton_neg) #not calculated, no negative articles
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_30$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_30$n_w) # monthly seasonality (seas = 1; weekly); 
#findfrequency(dat_30$ton_neg) # not calculated, no negative articles 

#Build a time series
dat_30$n_ts <- ts(dat_30$n)
dat_30$n_w_ts <- ts(dat_30$n_w)
dat_30$ton_neg_ts <- ts(dat_30$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_30$n_ts_sm <- sma(dat_30$n_ts, order = 4)$fitted
dat_30$n_w_ts_sm <- sma(dat_30$n_w_ts, order = 4)$fitted
dat_30$ton_neg_ts_sm <- sma(dat_30$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_30$week <- as.Date(paste(dat_30$years, dat_30$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_30.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_30$week, dat_30$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="global food safety initiative")
abline(h = mean(dat_30$n_ts_sm))
abline(h = mean(dat_30$n_ts_sm)+sd(dat_30$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_30$n_ts_sm)-sd(dat_30$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_30.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_30$week, dat_30$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="global food safety initiative")
abline(h = mean(dat_30$ton_neg_ts_sm))
abline(h = mean(dat_30$ton_neg_ts_sm)+sd(dat_30$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_30$ton_neg_ts_sm)-sd(dat_30$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_30$peak <- 0
dat_30$peak[dat_30$n_ts_sm>mean(dat_30$n_ts_sm)+sd(dat_30$n_ts_sm)] <- 1
dat_30$peak[dat_30$n_ts_sm<mean(dat_30$n_ts_sm)-sd(dat_30$n_ts_sm)] <- -1
#Tonality
dat_30$peak_neg <- 0
dat_30$peak_neg[dat_30$ton_neg_ts_sm>mean(dat_30$ton_neg_ts_sm)+sd(dat_30$ton_neg_ts_sm)] <- 1
dat_30$peak_neg[dat_30$ton_neg_ts_sm<mean(dat_30$ton_neg_ts_sm)-sd(dat_30$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_30$entity_id <- 30

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_30$n <- NULL
dat_30$n_w <- NULL

#global reporting initiative
#----------
dat_31 <- dat_base[dat_base$entity_id==31,]
require(plyr)
dat_31 <- ddply(dat_31, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_31 <- dat_31[order(dat_31$year, dat_31$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_31 <- dat_31[dat_31$week != 53,]
dat_31 <- dat_31[!is.na(dat_31$week),]
#do we have censored data?
hist(dat_31$n, breaks = 100)
hist(dat_31$n_w, breaks = 100)
hist(dat_31$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_31$n)
adf.test(dat_31$n_w)
adf.test(dat_31$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_31$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_31$n_w) # monthly seasonality (seas = 4; monthly); 
findfrequency(dat_31$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_31$n_ts <- ts(dat_31$n)
dat_31$n_w_ts <- ts(dat_31$n_w)
dat_31$ton_neg_ts <- ts(dat_31$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_31$n_ts_sm <- sma(dat_31$n_ts, order = 4)$fitted
dat_31$n_w_ts_sm <- sma(dat_31$n_w_ts, order = 4)$fitted
dat_31$ton_neg_ts_sm <- sma(dat_31$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_31$week <- as.Date(paste(dat_31$years, dat_31$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_31.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_31$week, dat_31$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="global reporting initiative")
abline(h = mean(dat_31$n_ts_sm))
abline(h = mean(dat_31$n_ts_sm)+sd(dat_31$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_31$n_ts_sm)-sd(dat_31$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_31.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_31$week, dat_31$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="global reporting initiative")
abline(h = mean(dat_31$ton_neg_ts_sm))
abline(h = mean(dat_31$ton_neg_ts_sm)+sd(dat_31$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_31$ton_neg_ts_sm)-sd(dat_31$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_31$peak <- 0
dat_31$peak[dat_31$n_ts_sm>mean(dat_31$n_ts_sm)+sd(dat_31$n_ts_sm)] <- 1
dat_31$peak[dat_31$n_ts_sm<mean(dat_31$n_ts_sm)-sd(dat_31$n_ts_sm)] <- -1
#Tonality
dat_31$peak_neg <- 0
dat_31$peak_neg[dat_31$ton_neg_ts_sm>mean(dat_31$ton_neg_ts_sm)+sd(dat_31$ton_neg_ts_sm)] <- 1
dat_31$peak_neg[dat_31$ton_neg_ts_sm<mean(dat_31$ton_neg_ts_sm)-sd(dat_31$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_31$entity_id <- 31

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_31$n <- NULL
dat_31$n_w <- NULL

#global witness
#----------
dat_32 <- dat_base[dat_base$entity_id==32,]
require(plyr)
dat_32 <- ddply(dat_32, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_32 <- dat_32[order(dat_32$year, dat_32$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_32 <- dat_32[dat_32$week != 53,]
dat_32 <- dat_32[!is.na(dat_32$week),]

#do we have censored data?
hist(dat_32$n, breaks = 100)
hist(dat_32$n_w, breaks = 100)
hist(dat_32$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_32$n)
adf.test(dat_32$n_w)
adf.test(dat_32$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_32$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_32$n_w) # monthly seasonality (seas = 3; tri-weekly); 
findfrequency(dat_32$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_32$n_ts <- ts(dat_32$n)
dat_32$n_w_ts <- ts(dat_32$n_w)
dat_32$ton_neg_ts <- ts(dat_32$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_32$n_ts_sm <- sma(dat_32$n_ts, order = 4)$fitted
dat_32$n_w_ts_sm <- sma(dat_32$n_w_ts, order = 4)$fitted
dat_32$ton_neg_ts_sm <- sma(dat_32$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_32$week <- as.Date(paste(dat_32$years, dat_32$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_32.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_32$week, dat_32$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="global witness")
abline(h = mean(dat_32$n_ts_sm))
abline(h = mean(dat_32$n_ts_sm)+sd(dat_32$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_32$n_ts_sm)-sd(dat_32$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_32.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_32$week, dat_32$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="global witness")
abline(h = mean(dat_32$ton_neg_ts_sm))
abline(h = mean(dat_32$ton_neg_ts_sm)+sd(dat_32$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_32$ton_neg_ts_sm)-sd(dat_32$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_32$peak <- 0
dat_32$peak[dat_32$n_ts_sm>mean(dat_32$n_ts_sm)+sd(dat_32$n_ts_sm)] <- 1
dat_32$peak[dat_32$n_ts_sm<mean(dat_32$n_ts_sm)-sd(dat_32$n_ts_sm)] <- -1
#Tonality
dat_32$peak_neg <- 0
dat_32$peak_neg[dat_32$ton_neg_ts_sm>mean(dat_32$ton_neg_ts_sm)+sd(dat_32$ton_neg_ts_sm)] <- 1
dat_32$peak_neg[dat_32$ton_neg_ts_sm<mean(dat_32$ton_neg_ts_sm)-sd(dat_32$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_32$entity_id <- 32

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_32$n <- NULL
dat_32$n_w <- NULL


#greater london authority (gla)
#------------------------
dat_34 <- dat_base[dat_base$entity_id==34,]
require(plyr)
dat_34 <- ddply(dat_34, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_34 <- dat_34[order(dat_34$year, dat_34$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_34 <- dat_34[dat_34$week != 53,]
dat_34 <- dat_34[!is.na(dat_34$week),]

#do we have censored data?
hist(dat_34$n, breaks = 100)
hist(dat_34$n_w, breaks = 100)
hist(dat_34$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_34$n)
adf.test(dat_34$n_w)
adf.test(dat_34$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_34$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_34$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_34$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_34$n_ts <- ts(dat_34$n)
dat_34$n_w_ts <- ts(dat_34$n_w)
dat_34$ton_neg_ts <- ts(dat_34$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_34$n_ts_sm <- sma(dat_34$n_ts, order = 4)$fitted
dat_34$n_w_ts_sm <- sma(dat_34$n_w_ts, order = 4)$fitted
dat_34$ton_neg_ts_sm <- sma(dat_34$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_34$week <- as.Date(paste(dat_34$years, dat_34$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_34.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_34$week, dat_34$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="greater london authority (gla)")
abline(h = mean(dat_34$n_ts_sm))
abline(h = mean(dat_34$n_ts_sm)+sd(dat_34$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_34$n_ts_sm)-sd(dat_34$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_34.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_34$week, dat_34$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="greater london authority (gla)")
abline(h = mean(dat_34$ton_neg_ts_sm))
abline(h = mean(dat_34$ton_neg_ts_sm)+sd(dat_34$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_34$ton_neg_ts_sm)-sd(dat_34$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_34$peak <- 0
dat_34$peak[dat_34$n_ts_sm>mean(dat_34$n_ts_sm)+sd(dat_34$n_ts_sm)] <- 1
dat_34$peak[dat_34$n_ts_sm<mean(dat_34$n_ts_sm)-sd(dat_34$n_ts_sm)] <- -1
#Tonality
dat_34$peak_neg <- 0
dat_34$peak_neg[dat_34$ton_neg_ts_sm>mean(dat_34$ton_neg_ts_sm)+sd(dat_34$ton_neg_ts_sm)] <- 1
dat_34$peak_neg[dat_34$ton_neg_ts_sm<mean(dat_34$ton_neg_ts_sm)-sd(dat_34$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_34$entity_id <- 34

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_34$n <- NULL
dat_34$n_w <- NULL


#igad inter-parliamentary union: only 1 article
#----------
dat_35 <- dat_base[dat_base$entity_id==35,]
require(plyr)
dat_35 <- ddply(dat_35, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_35 <- dat_35[order(dat_35$year, dat_35$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_35 <- dat_35[dat_35$week != 53,]
dat_35 <- dat_35[!is.na(dat_35$week),]

#do we have censored data?
hist(dat_35$n, breaks = 100)
hist(dat_35$n_w, breaks = 100)
hist(dat_35$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_35$n_ts <- dat_35$n
dat_35$n_w_ts <- dat_35$n_w
dat_35$ton_neg_ts <- dat_35$ton_neg

#duplicate variable with different name
dat_35$n_ts_sm <- dat_35$n_ts
dat_35$n_w_ts_sm <- dat_35$n_w_ts
dat_35$ton_neg_ts_sm <- dat_35$ton_neg_ts

# create starting date of every week
dat_35$week <- as.Date(paste(dat_35$years, dat_35$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_35.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_35$week, dat_35$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="igad inter-parliamentary union")
abline(h = mean(dat_35$n_ts_sm))
abline(h = mean(dat_35$n_ts_sm)+sd(dat_35$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_35$n_ts_sm)-sd(dat_35$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_35.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_35$week, dat_35$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="igad inter-parliamentary union")
abline(h = mean(dat_35$ton_neg_ts_sm))
abline(h = mean(dat_35$ton_neg_ts_sm)+sd(dat_35$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_35$ton_neg_ts_sm)-sd(dat_35$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_35$peak <- 0
dat_35$peak[dat_35$n_ts_sm>mean(dat_35$n_ts_sm)+sd(dat_35$n_ts_sm)] <- 1
dat_35$peak[dat_35$n_ts_sm<mean(dat_35$n_ts_sm)-sd(dat_35$n_ts_sm)] <- -1
#Tonality
dat_35$peak_neg <- 0
dat_35$peak_neg[dat_35$ton_neg_ts_sm>mean(dat_35$ton_neg_ts_sm)+sd(dat_35$ton_neg_ts_sm)] <- 1
dat_35$peak_neg[dat_35$ton_neg_ts_sm<mean(dat_35$ton_neg_ts_sm)-sd(dat_35$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_35$entity_id <- 35

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_35$n <- NULL
dat_35$n_w <- NULL


#inter-parliamentary union
#----------
dat_36 <- dat_base[dat_base$entity_id==36,]
require(plyr)
dat_36 <- ddply(dat_36, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_36 <- dat_36[order(dat_36$year, dat_36$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_36 <- dat_36[dat_36$week != 53,]
dat_36 <- dat_36[!is.na(dat_36$week),]

#do we have censored data?
hist(dat_36$n, breaks = 100)
hist(dat_36$n_w, breaks = 100)
hist(dat_36$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_36$n)
adf.test(dat_36$n_w)
adf.test(dat_36$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_36$n) # monthly seasonality (seas = 6; 1.5 monthly); 
findfrequency(dat_36$n_w) # monthly seasonality (seas = 6; 1.5 monthly); 
findfrequency(dat_36$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_36$n_ts <- ts(dat_36$n)
dat_36$n_w_ts <- ts(dat_36$n_w)
dat_36$ton_neg_ts <- ts(dat_36$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_36$n_ts_sm <- sma(dat_36$n_ts, order = 4)$fitted
dat_36$n_w_ts_sm <- sma(dat_36$n_w_ts, order = 4)$fitted
dat_36$ton_neg_ts_sm <- sma(dat_36$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_36$week <- as.Date(paste(dat_36$years, dat_36$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_36.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_36$week, dat_36$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="inter-parliamentary union")
abline(h = mean(dat_36$n_ts_sm))
abline(h = mean(dat_36$n_ts_sm)+sd(dat_36$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_36$n_ts_sm)-sd(dat_36$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_36.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_36$week, dat_36$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="inter-parliamentary union")
abline(h = mean(dat_36$ton_neg_ts_sm))
abline(h = mean(dat_36$ton_neg_ts_sm)+sd(dat_36$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_36$ton_neg_ts_sm)-sd(dat_36$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_36$peak <- 0
dat_36$peak[dat_36$n_ts_sm>mean(dat_36$n_ts_sm)+sd(dat_36$n_ts_sm)] <- 1
dat_36$peak[dat_36$n_ts_sm<mean(dat_36$n_ts_sm)-sd(dat_36$n_ts_sm)] <- -1
#Tonality
dat_36$peak_neg <- 0
dat_36$peak_neg[dat_36$ton_neg_ts_sm>mean(dat_36$ton_neg_ts_sm)+sd(dat_36$ton_neg_ts_sm)] <- 1
dat_36$peak_neg[dat_36$ton_neg_ts_sm<mean(dat_36$ton_neg_ts_sm)-sd(dat_36$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_36$entity_id <- 36

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_36$n <- NULL
dat_36$n_w <- NULL


#intergovernmental organization for international carriage by rail: only 1 article
#----------
dat_37 <- dat_base[dat_base$entity_id==37,]
require(plyr)
dat_37 <- ddply(dat_37, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_37 <- dat_37[order(dat_37$year, dat_37$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_37 <- dat_37[dat_37$week != 53,]
dat_37 <- dat_37[!is.na(dat_37$week),]

#do we have censored data?
hist(dat_37$n, breaks = 100)
hist(dat_37$n_w, breaks = 100)
hist(dat_37$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_37$n_ts <- dat_37$n
dat_37$n_w_ts <- dat_37$n_w
dat_37$ton_neg_ts <- dat_37$ton_neg

#duplicate variable with different name
dat_37$n_ts_sm <- dat_37$n_ts
dat_37$n_w_ts_sm <- dat_37$n_w_ts
dat_37$ton_neg_ts_sm <- dat_37$ton_neg_ts

# create starting date of every week
dat_37$week <- as.Date(paste(dat_37$years, dat_37$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_37.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_37$week, dat_37$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="intergovernmental organization for international carriage by rail")
abline(h = mean(dat_37$n_ts_sm))
abline(h = mean(dat_37$n_ts_sm)+sd(dat_37$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_37$n_ts_sm)-sd(dat_37$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_37.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_37$week, dat_37$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="intergovernmental organization for international carriage by rail")
abline(h = mean(dat_37$ton_neg_ts_sm))
abline(h = mean(dat_37$ton_neg_ts_sm)+sd(dat_37$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_37$ton_neg_ts_sm)-sd(dat_37$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_37$peak <- 0
dat_37$peak[dat_37$n_ts_sm>mean(dat_37$n_ts_sm)+sd(dat_37$n_ts_sm)] <- 1
dat_37$peak[dat_37$n_ts_sm<mean(dat_37$n_ts_sm)-sd(dat_37$n_ts_sm)] <- -1
#Tonality
dat_37$peak_neg <- 0
dat_37$peak_neg[dat_37$ton_neg_ts_sm>mean(dat_37$ton_neg_ts_sm)+sd(dat_37$ton_neg_ts_sm)] <- 1
dat_37$peak_neg[dat_37$ton_neg_ts_sm<mean(dat_37$ton_neg_ts_sm)-sd(dat_37$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_37$entity_id <- 37

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_37$n <- NULL
dat_37$n_w <- NULL


#international association of insurance supervisors
#----------
dat_38 <- dat_base[dat_base$entity_id==38,]
require(plyr)
dat_38 <- ddply(dat_38, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_38 <- dat_38[order(dat_38$year, dat_38$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_38 <- dat_38[dat_38$week != 53,]
dat_38 <- dat_38[!is.na(dat_38$week),]

#do we have censored data?
hist(dat_38$n, breaks = 100)
hist(dat_38$n_w, breaks = 100)
hist(dat_38$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_38$n) #non-stationary
adf.test(dat_38$n_w) #non-stationary
adf.test(dat_38$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_38$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_38$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_38$ton_neg) # monthly seasonality (seas = 18; +/- 4.5 months); 

#Build a time series
dat_38$n_ts <- ts(dat_38$n)
dat_38$n_w_ts <- ts(dat_38$n_w)
dat_38$ton_neg_ts <- ts(dat_38$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_38$n_ts_sm <- sma(dat_38$n_ts, order = 4)$fitted
dat_38$n_w_ts_sm <- sma(dat_38$n_w_ts, order = 4)$fitted
dat_38$ton_neg_ts_sm <- sma(dat_38$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_38$week <- as.Date(paste(dat_38$years, dat_38$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_38.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_38$week, dat_38$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international association of insurance supervisors")
abline(h = mean(dat_38$n_ts_sm))
abline(h = mean(dat_38$n_ts_sm)+sd(dat_38$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_38$n_ts_sm)-sd(dat_38$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_38.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_38$week, dat_38$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international association of insurance supervisors")
abline(h = mean(dat_38$ton_neg_ts_sm))
abline(h = mean(dat_38$ton_neg_ts_sm)+sd(dat_38$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_38$ton_neg_ts_sm)-sd(dat_38$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_38$peak <- 0
dat_38$peak[dat_38$n_ts_sm>mean(dat_38$n_ts_sm)+sd(dat_38$n_ts_sm)] <- 1
dat_38$peak[dat_38$n_ts_sm<mean(dat_38$n_ts_sm)-sd(dat_38$n_ts_sm)] <- -1
#Tonality
dat_38$peak_neg <- 0
dat_38$peak_neg[dat_38$ton_neg_ts_sm>mean(dat_38$ton_neg_ts_sm)+sd(dat_38$ton_neg_ts_sm)] <- 1
dat_38$peak_neg[dat_38$ton_neg_ts_sm<mean(dat_38$ton_neg_ts_sm)-sd(dat_38$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_38$entity_id <- 38

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_38$n <- NULL
dat_38$n_w <- NULL

#international chamber of commerce
#----------
dat_39 <- dat_base[dat_base$entity_id==39,]
require(plyr)
dat_39 <- ddply(dat_39, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_39 <- dat_39[order(dat_39$year, dat_39$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_39 <- dat_39[dat_39$week != 53,]
dat_39 <- dat_39[!is.na(dat_39$week),]

#do we have censored data?
hist(dat_39$n, breaks = 100)
hist(dat_39$n_w, breaks = 100)
hist(dat_39$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_39$n)
adf.test(dat_39$n_w)
adf.test(dat_39$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_39$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_39$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_39$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_39$n_ts <- ts(dat_39$n)
dat_39$n_w_ts <- ts(dat_39$n_w)
dat_39$ton_neg_ts <- ts(dat_39$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_39$n_ts_sm <- sma(dat_39$n_ts, order = 4)$fitted
dat_39$n_w_ts_sm <- sma(dat_39$n_w_ts, order = 4)$fitted
dat_39$ton_neg_ts_sm <- sma(dat_39$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_39$week <- as.Date(paste(dat_39$years, dat_39$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_39.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_39$week, dat_39$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international chamber of commerces")
abline(h = mean(dat_39$n_ts_sm))
abline(h = mean(dat_39$n_ts_sm)+sd(dat_39$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_39$n_ts_sm)-sd(dat_39$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_39.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_39$week, dat_39$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international chamber of commerce")
abline(h = mean(dat_39$ton_neg_ts_sm))
abline(h = mean(dat_39$ton_neg_ts_sm)+sd(dat_39$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_39$ton_neg_ts_sm)-sd(dat_39$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_39$peak <- 0
dat_39$peak[dat_39$n_ts_sm>mean(dat_39$n_ts_sm)+sd(dat_39$n_ts_sm)] <- 1
dat_39$peak[dat_39$n_ts_sm<mean(dat_39$n_ts_sm)-sd(dat_39$n_ts_sm)] <- -1
#Tonality
dat_39$peak_neg <- 0
dat_39$peak_neg[dat_39$ton_neg_ts_sm>mean(dat_39$ton_neg_ts_sm)+sd(dat_39$ton_neg_ts_sm)] <- 1
dat_39$peak_neg[dat_39$ton_neg_ts_sm<mean(dat_39$ton_neg_ts_sm)-sd(dat_39$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_39$entity_id <- 39

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_39$n <- NULL
dat_39$n_w <- NULL

#international competition network
#----------
dat_40 <- dat_base[dat_base$entity_id==40,]
require(plyr)
dat_40 <- ddply(dat_40, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_40 <- dat_40[order(dat_40$year, dat_40$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_40 <- dat_40[dat_40$week != 53,]
dat_40 <- dat_40[!is.na(dat_40$week),]

#do we have censored data?
hist(dat_40$n, breaks = 100)
hist(dat_40$n_w, breaks = 100)
hist(dat_40$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_40$n) #non-stationary
adf.test(dat_40$n_w) #non-stationary
adf.test(dat_40$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_40$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_40$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_40$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_40$n_ts <- ts(dat_40$n)
dat_40$n_w_ts <- ts(dat_40$n_w)
dat_40$ton_neg_ts <- ts(dat_40$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_40$n_ts_sm <- sma(dat_40$n_ts, order = 4)$fitted
dat_40$n_w_ts_sm <- sma(dat_40$n_w_ts, order = 4)$fitted
dat_40$ton_neg_ts_sm <- sma(dat_40$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_40$week <- as.Date(paste(dat_40$years, dat_40$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_40.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_40$week, dat_40$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international competition network")
abline(h = mean(dat_40$n_ts_sm))
abline(h = mean(dat_40$n_ts_sm)+sd(dat_40$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_40$n_ts_sm)-sd(dat_40$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_40.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_40$week, dat_40$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international competition network")
abline(h = mean(dat_40$ton_neg_ts_sm))
abline(h = mean(dat_40$ton_neg_ts_sm)+sd(dat_40$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_40$ton_neg_ts_sm)-sd(dat_40$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_40$peak <- 0
dat_40$peak[dat_40$n_ts_sm>mean(dat_40$n_ts_sm)+sd(dat_40$n_ts_sm)] <- 1
dat_40$peak[dat_40$n_ts_sm<mean(dat_40$n_ts_sm)-sd(dat_40$n_ts_sm)] <- -1
#Tonality
dat_40$peak_neg <- 0
dat_40$peak_neg[dat_40$ton_neg_ts_sm>mean(dat_40$ton_neg_ts_sm)+sd(dat_40$ton_neg_ts_sm)] <- 1
dat_40$peak_neg[dat_40$ton_neg_ts_sm<mean(dat_40$ton_neg_ts_sm)-sd(dat_40$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_40$entity_id <- 40

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_40$n <- NULL
dat_40$n_w <- NULL

#international criminal court
#----------
dat_41 <- dat_base[dat_base$entity_id==41,]
require(plyr)
dat_41 <- ddply(dat_41, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_41 <- dat_41[order(dat_41$year, dat_41$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_41 <- dat_41[dat_41$week != 53,]
dat_41 <- dat_41[!is.na(dat_41$week),]

#do we have censored data?
hist(dat_41$n, breaks = 100)
hist(dat_41$n_w, breaks = 100)
hist(dat_41$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_41$n)
adf.test(dat_41$n_w)
adf.test(dat_41$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_41$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_41$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_41$ton_neg) # monthly seasonality (seas = 7; 1.5 monthly); 

#Build a time series
dat_41$n_ts <- ts(dat_41$n)
dat_41$n_w_ts <- ts(dat_41$n_w)
dat_41$ton_neg_ts <- ts(dat_41$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_41$n_ts_sm <- sma(dat_41$n_ts, order = 4)$fitted
dat_41$n_w_ts_sm <- sma(dat_41$n_w_ts, order = 4)$fitted
dat_41$ton_neg_ts_sm <- sma(dat_41$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_41$week <- as.Date(paste(dat_41$years, dat_41$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_41.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_41$week, dat_41$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international criminal court")
abline(h = mean(dat_41$n_ts_sm))
abline(h = mean(dat_41$n_ts_sm)+sd(dat_41$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_41$n_ts_sm)-sd(dat_41$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_41.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_41$week, dat_41$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international criminal court")
abline(h = mean(dat_41$ton_neg_ts_sm))
abline(h = mean(dat_41$ton_neg_ts_sm)+sd(dat_41$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_41$ton_neg_ts_sm)-sd(dat_41$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_41$peak <- 0
dat_41$peak[dat_41$n_ts_sm>mean(dat_41$n_ts_sm)+sd(dat_41$n_ts_sm)] <- 1
dat_41$peak[dat_41$n_ts_sm<mean(dat_41$n_ts_sm)-sd(dat_41$n_ts_sm)] <- -1
#Tonality
dat_41$peak_neg <- 0
dat_41$peak_neg[dat_41$ton_neg_ts_sm>mean(dat_41$ton_neg_ts_sm)+sd(dat_41$ton_neg_ts_sm)] <- 1
dat_41$peak_neg[dat_41$ton_neg_ts_sm<mean(dat_41$ton_neg_ts_sm)-sd(dat_41$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_41$entity_id <- 41

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_41$n <- NULL
dat_41$n_w <- NULL

#international diamond manufacturers association: 9 weeks with coverage, 9 articles
#----------
dat_43 <- dat_base[dat_base$entity_id==43,]
require(plyr)
dat_43 <- ddply(dat_43, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_43 <- dat_43[order(dat_43$year, dat_43$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_43 <- dat_43[dat_43$week != 53,]
dat_43 <- dat_43[!is.na(dat_43$week),]

#do we have censored data?
hist(dat_43$n, breaks = 100) 
hist(dat_43$n_w, breaks = 100) 
hist(dat_43$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_43$n) #non-stationary
adf.test(dat_43$n_w) #non-stationary
adf.test(dat_43$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
#findfrequency(dat_43$n) # not calculated, no variation, only 1 article per week 
findfrequency(dat_43$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_43$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_43$n_ts <- ts(dat_43$n)
dat_43$n_w_ts <- ts(dat_43$n_w)
dat_43$ton_neg_ts <- ts(dat_43$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_43$n_ts_sm <- sma(dat_43$n_ts, order = 4)$fitted
dat_43$n_w_ts_sm <- sma(dat_43$n_w_ts, order = 4)$fitted
dat_43$ton_neg_ts_sm <- sma(dat_43$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_43$week <- as.Date(paste(dat_43$years, dat_43$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_43.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_43$week, dat_43$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international diamond manufacturers association")
abline(h = mean(dat_43$n_ts_sm))
abline(h = mean(dat_43$n_ts_sm)+sd(dat_43$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_43$n_ts_sm)-sd(dat_43$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_43.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_43$week, dat_43$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international diamond manufacturers association")
abline(h = mean(dat_43$ton_neg_ts_sm))
abline(h = mean(dat_43$ton_neg_ts_sm)+sd(dat_43$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_43$ton_neg_ts_sm)-sd(dat_43$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_43$peak <- 0
dat_43$peak[dat_43$n_ts_sm>mean(dat_43$n_ts_sm)+sd(dat_43$n_ts_sm)] <- 1
dat_43$peak[dat_43$n_ts_sm<mean(dat_43$n_ts_sm)-sd(dat_43$n_ts_sm)] <- -1
#Tonality
dat_43$peak_neg <- 0
dat_43$peak_neg[dat_43$ton_neg_ts_sm>mean(dat_43$ton_neg_ts_sm)+sd(dat_43$ton_neg_ts_sm)] <- 1
dat_43$peak_neg[dat_43$ton_neg_ts_sm<mean(dat_43$ton_neg_ts_sm)-sd(dat_43$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_43$entity_id <- 43

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_43$n <- NULL
dat_43$n_w <- NULL

#international federation of organic agriculture movements: 14 weeks with coverage, 14 articles
#----------
dat_44 <- dat_base[dat_base$entity_id==44,]
require(plyr)
dat_44 <- ddply(dat_44, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_44 <- dat_44[order(dat_44$year, dat_44$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_44 <- dat_44[dat_44$week != 53,]
dat_44 <- dat_44[!is.na(dat_44$week),]

#do we have censored data?
hist(dat_44$n, breaks = 100)
hist(dat_44$n_w, breaks = 100)
hist(dat_44$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
#adf.test(dat_44$n) #not calculated, one article per week
adf.test(dat_44$n_w) #non-stationary
adf.test(dat_44$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
#findfrequency(dat_44$n) # not calculated
findfrequency(dat_44$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_44$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_44$n_ts <- ts(dat_44$n)
dat_44$n_w_ts <- ts(dat_44$n_w)
dat_44$ton_neg_ts <- ts(dat_44$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_44$n_ts_sm <- sma(dat_44$n_ts, order = 4)$fitted
dat_44$n_w_ts_sm <- sma(dat_44$n_w_ts, order = 4)$fitted
dat_44$ton_neg_ts_sm <- sma(dat_44$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_44$week <- as.Date(paste(dat_44$years, dat_44$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_44.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_44$week, dat_44$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international federation of organic agriculture movements")
abline(h = mean(dat_44$n_ts_sm))
abline(h = mean(dat_44$n_ts_sm)+sd(dat_44$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_44$n_ts_sm)-sd(dat_44$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_44.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_44$week, dat_44$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international federation of organic agriculture movements")
abline(h = mean(dat_44$ton_neg_ts_sm))
abline(h = mean(dat_44$ton_neg_ts_sm)+sd(dat_44$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_44$ton_neg_ts_sm)-sd(dat_44$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_44$peak <- 0
dat_44$peak[dat_44$n_ts_sm>mean(dat_44$n_ts_sm)+sd(dat_44$n_ts_sm)] <- 1
dat_44$peak[dat_44$n_ts_sm<mean(dat_44$n_ts_sm)-sd(dat_44$n_ts_sm)] <- -1
#Tonality
dat_44$peak_neg <- 0
dat_44$peak_neg[dat_44$ton_neg_ts_sm>mean(dat_44$ton_neg_ts_sm)+sd(dat_44$ton_neg_ts_sm)] <- 1
dat_44$peak_neg[dat_44$ton_neg_ts_sm<mean(dat_44$ton_neg_ts_sm)-sd(dat_44$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_44$entity_id <- 44

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_44$n <- NULL
dat_44$n_w <- NULL

#international labor organization
#----------
dat_45 <- dat_base[dat_base$entity_id==45,]
require(plyr)
dat_45 <- ddply(dat_45, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_45 <- dat_45[order(dat_45$year, dat_45$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_45 <- dat_45[dat_45$week != 53,]
dat_45 <- dat_45[!is.na(dat_45$week),]

#do we have censored data?
hist(dat_45$n, breaks = 100)
hist(dat_45$n_w, breaks = 100)
hist(dat_45$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_45$n)
adf.test(dat_45$n_w)
adf.test(dat_45$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_45$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_45$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_45$ton_neg) # monthly seasonality (seas = 4; monthly); 

#Build a time series
dat_45$n_ts <- ts(dat_45$n)
dat_45$n_w_ts <- ts(dat_45$n_w)
dat_45$ton_neg_ts <- ts(dat_45$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_45$n_ts_sm <- sma(dat_45$n_ts, order = 4)$fitted
dat_45$n_w_ts_sm <- sma(dat_45$n_w_ts, order = 4)$fitted
dat_45$ton_neg_ts_sm <- sma(dat_45$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_45$week <- as.Date(paste(dat_45$years, dat_45$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_45.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_45$week, dat_45$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international labor organization")
abline(h = mean(dat_45$n_ts_sm))
abline(h = mean(dat_45$n_ts_sm)+sd(dat_45$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_45$n_ts_sm)-sd(dat_45$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_45.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_45$week, dat_45$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international labor organization")
abline(h = mean(dat_45$ton_neg_ts_sm))
abline(h = mean(dat_45$ton_neg_ts_sm)+sd(dat_45$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_45$ton_neg_ts_sm)-sd(dat_45$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_45$peak <- 0
dat_45$peak[dat_45$n_ts_sm>mean(dat_45$n_ts_sm)+sd(dat_45$n_ts_sm)] <- 1
dat_45$peak[dat_45$n_ts_sm<mean(dat_45$n_ts_sm)-sd(dat_45$n_ts_sm)] <- -1
#Tonality
dat_45$peak_neg <- 0
dat_45$peak_neg[dat_45$ton_neg_ts_sm>mean(dat_45$ton_neg_ts_sm)+sd(dat_45$ton_neg_ts_sm)] <- 1
dat_45$peak_neg[dat_45$ton_neg_ts_sm<mean(dat_45$ton_neg_ts_sm)-sd(dat_45$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_45$entity_id <- 45

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_45$n <- NULL
dat_45$n_w <- NULL

#international network for environmental compliance and enforcement: 2 weeks with coverage, 2 articles
#----------
dat_46 <- dat_base[dat_base$entity_id==46,]
require(plyr)
dat_46 <- ddply(dat_46, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_46 <- dat_46[order(dat_46$year, dat_46$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_46 <- dat_46[dat_46$week != 53,]
dat_46 <- dat_46[!is.na(dat_46$week),]
#do we have censored data?
hist(dat_46$n, breaks = 100)
hist(dat_46$n_w, breaks = 100)
hist(dat_46$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_46$n_ts <- dat_46$n
dat_46$n_w_ts <- dat_46$n_w
dat_46$ton_neg_ts <- dat_46$ton_neg

#duplicate variable with different name
dat_46$n_ts_sm <- dat_46$n_ts
dat_46$n_w_ts_sm <- dat_46$n_w_ts
dat_46$ton_neg_ts_sm <- dat_46$ton_neg_ts

# create starting date of every week
dat_46$week <- as.Date(paste(dat_46$years, dat_46$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_46.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_46$week, dat_46$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international network for environmental compliance and enforcement")
abline(h = mean(dat_46$n_ts_sm))
abline(h = mean(dat_46$n_ts_sm)+sd(dat_46$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_46$n_ts_sm)-sd(dat_46$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_46.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_46$week, dat_46$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international network for environmental compliance and enforcement")
abline(h = mean(dat_46$ton_neg_ts_sm))
abline(h = mean(dat_46$ton_neg_ts_sm)+sd(dat_46$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_46$ton_neg_ts_sm)-sd(dat_46$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_46$peak <- 0
dat_46$peak[dat_46$n_ts_sm>mean(dat_46$n_ts_sm)+sd(dat_46$n_ts_sm)] <- 1
dat_46$peak[dat_46$n_ts_sm<mean(dat_46$n_ts_sm)-sd(dat_46$n_ts_sm)] <- -1
#Tonality
dat_46$peak_neg <- 0
dat_46$peak_neg[dat_46$ton_neg_ts_sm>mean(dat_46$ton_neg_ts_sm)+sd(dat_46$ton_neg_ts_sm)] <- 1
dat_46$peak_neg[dat_46$ton_neg_ts_sm<mean(dat_46$ton_neg_ts_sm)-sd(dat_46$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_46$entity_id <- 46

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_46$n <- NULL
dat_46$n_w <- NULL

#international organization of securities commissions
#----------
dat_48 <- dat_base[dat_base$entity_id==48,]
require(plyr)
dat_48 <- ddply(dat_48, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_48 <- dat_48[order(dat_48$year, dat_48$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_48 <- dat_48[dat_48$week != 53,]
dat_48 <- dat_48[!is.na(dat_48$week),]

#do we have censored data?
hist(dat_48$n, breaks = 100)
hist(dat_48$n_w, breaks = 100)
hist(dat_48$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_48$n)
adf.test(dat_48$n_w)
adf.test(dat_48$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_48$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_48$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_48$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_48$n_ts <- ts(dat_48$n)
dat_48$n_w_ts <- ts(dat_48$n_w)
dat_48$ton_neg_ts <- ts(dat_48$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_48$n_ts_sm <- sma(dat_48$n_ts, order = 4)$fitted
dat_48$n_w_ts_sm <- sma(dat_48$n_w_ts, order = 4)$fitted
dat_48$ton_neg_ts_sm <- sma(dat_48$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_48$week <- as.Date(paste(dat_48$years, dat_48$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_48.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_48$week, dat_48$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international organization of securities commissions")
abline(h = mean(dat_48$n_ts_sm))
abline(h = mean(dat_48$n_ts_sm)+sd(dat_48$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_48$n_ts_sm)-sd(dat_48$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_48.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_48$week, dat_48$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international organization of securities commissions")
abline(h = mean(dat_48$ton_neg_ts_sm))
abline(h = mean(dat_48$ton_neg_ts_sm)+sd(dat_48$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_48$ton_neg_ts_sm)-sd(dat_48$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_48$peak <- 0
dat_48$peak[dat_48$n_ts_sm>mean(dat_48$n_ts_sm)+sd(dat_48$n_ts_sm)] <- 1
dat_48$peak[dat_48$n_ts_sm<mean(dat_48$n_ts_sm)-sd(dat_48$n_ts_sm)] <- -1
#Tonality
dat_48$peak_neg <- 0
dat_48$peak_neg[dat_48$ton_neg_ts_sm>mean(dat_48$ton_neg_ts_sm)+sd(dat_48$ton_neg_ts_sm)] <- 1
dat_48$peak_neg[dat_48$ton_neg_ts_sm<mean(dat_48$ton_neg_ts_sm)-sd(dat_48$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_48$entity_id <- 48

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_48$n <- NULL
dat_48$n_w <- NULL

#international whaling commission
#----------
dat_49 <- dat_base[dat_base$entity_id==49,]
require(plyr)
dat_49 <- ddply(dat_49, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_49 <- dat_49[order(dat_49$year, dat_49$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_49 <- dat_49[dat_49$week != 53,]
dat_49 <- dat_49[!is.na(dat_49$week),]


#do we have censored data?
hist(dat_49$n, breaks = 100)
hist(dat_49$n_w, breaks = 100)
hist(dat_49$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_49$n)
adf.test(dat_49$n_w)
adf.test(dat_49$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_49$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_49$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_49$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_49$n_ts <- ts(dat_49$n)
dat_49$n_w_ts <- ts(dat_49$n_w)
dat_49$ton_neg_ts <- ts(dat_49$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_49$n_ts_sm <- sma(dat_49$n_ts, order = 4)$fitted
dat_49$n_w_ts_sm <- sma(dat_49$n_w_ts, order = 4)$fitted
dat_49$ton_neg_ts_sm <- sma(dat_49$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_49$week <- as.Date(paste(dat_49$years, dat_49$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_49.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_49$week, dat_49$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="international whaling commission")
abline(h = mean(dat_49$n_ts_sm))
abline(h = mean(dat_49$n_ts_sm)+sd(dat_49$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_49$n_ts_sm)-sd(dat_49$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_49.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_49$week, dat_49$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="international whaling commission")
abline(h = mean(dat_49$ton_neg_ts_sm))
abline(h = mean(dat_49$ton_neg_ts_sm)+sd(dat_49$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_49$ton_neg_ts_sm)-sd(dat_49$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_49$peak <- 0
dat_49$peak[dat_49$n_ts_sm>mean(dat_49$n_ts_sm)+sd(dat_49$n_ts_sm)] <- 1
dat_49$peak[dat_49$n_ts_sm<mean(dat_49$n_ts_sm)-sd(dat_49$n_ts_sm)] <- -1
#Tonality
dat_49$peak_neg <- 0
dat_49$peak_neg[dat_49$ton_neg_ts_sm>mean(dat_49$ton_neg_ts_sm)+sd(dat_49$ton_neg_ts_sm)] <- 1
dat_49$peak_neg[dat_49$ton_neg_ts_sm<mean(dat_49$ton_neg_ts_sm)-sd(dat_49$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_49$entity_id <- 49

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_49$n <- NULL
dat_49$n_w <- NULL

#internet architecture board: 6 weeks with coverage, 5 articles
#----------
dat_50 <- dat_base[dat_base$entity_id==50,]
require(plyr)
dat_50 <- ddply(dat_50, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_50 <- dat_50[order(dat_50$year, dat_50$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_50 <- dat_50[dat_50$week != 53,]
dat_50 <- dat_50[!is.na(dat_50$week),]


#do we have censored data?
hist(dat_50$n, breaks = 100)
hist(dat_50$n_w, breaks = 100)
hist(dat_50$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
#adf.test(dat_50$n) #not calculated
#adf.test(dat_50$n_w) #not calculated
#adf.test(dat_50$ton_neg) #not calculated
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_50$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_50$n_w) # monthly seasonality (seas = 2; weekly); 
findfrequency(dat_50$ton_neg) # monthly seasonality (seas = 1 weekly); 

#Build a time series
dat_50$n_ts <- ts(dat_50$n)
dat_50$n_w_ts <- ts(dat_50$n_w)
dat_50$ton_neg_ts <- ts(dat_50$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_50$n_ts_sm <- sma(dat_50$n_ts, order = 4)$fitted
dat_50$n_w_ts_sm <- sma(dat_50$n_w_ts, order = 4)$fitted
dat_50$ton_neg_ts_sm <- sma(dat_50$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_50$week <- as.Date(paste(dat_50$years, dat_50$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_50.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_50$week, dat_50$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="internet architecture board")
abline(h = mean(dat_50$n_ts_sm))
abline(h = mean(dat_50$n_ts_sm)+sd(dat_50$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_50$n_ts_sm)-sd(dat_50$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_50.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_50$week, dat_50$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="internet architecture board")
abline(h = mean(dat_50$ton_neg_ts_sm))
abline(h = mean(dat_50$ton_neg_ts_sm)+sd(dat_50$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_50$ton_neg_ts_sm)-sd(dat_50$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_50$peak <- 0
dat_50$peak[dat_50$n_ts_sm>mean(dat_50$n_ts_sm)+sd(dat_50$n_ts_sm)] <- 1
dat_50$peak[dat_50$n_ts_sm<mean(dat_50$n_ts_sm)-sd(dat_50$n_ts_sm)] <- -1
#Tonality
dat_50$peak_neg <- 0
dat_50$peak_neg[dat_50$ton_neg_ts_sm>mean(dat_50$ton_neg_ts_sm)+sd(dat_50$ton_neg_ts_sm)] <- 1
dat_50$peak_neg[dat_50$ton_neg_ts_sm<mean(dat_50$ton_neg_ts_sm)-sd(dat_50$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_50$entity_id <- 50

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_50$n <- NULL
dat_50$n_w <- NULL

#internet corporation for assigned names and numbers (icann)
#----------
dat_51 <- dat_base[dat_base$entity_id==51,]
require(plyr)
dat_51 <- ddply(dat_51, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_51 <- dat_51[order(dat_51$year, dat_51$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_51 <- dat_51[dat_51$week != 53,]
dat_51 <- dat_51[!is.na(dat_51$week),]

#do we have censored data?
hist(dat_51$n, breaks = 100)
hist(dat_51$n_w, breaks = 100)
hist(dat_51$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_51$n)
adf.test(dat_51$n_w)
adf.test(dat_51$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_51$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_51$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_51$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_51$n_ts <- ts(dat_51$n)
dat_51$n_w_ts <- ts(dat_51$n_w)
dat_51$ton_neg_ts <- ts(dat_51$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_51$n_ts_sm <- sma(dat_51$n_ts, order = 4)$fitted
dat_51$n_w_ts_sm <- sma(dat_51$n_w_ts, order = 4)$fitted
dat_51$ton_neg_ts_sm <- sma(dat_51$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_51$week <- as.Date(paste(dat_51$years, dat_51$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_51.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_51$week, dat_51$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="internet corporation for assigned names and numbers (icann)")
abline(h = mean(dat_51$n_ts_sm))
abline(h = mean(dat_51$n_ts_sm)+sd(dat_51$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_51$n_ts_sm)-sd(dat_51$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_51.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_51$week, dat_51$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="internet corporation for assigned names and numbers (icann)")
abline(h = mean(dat_51$ton_neg_ts_sm))
abline(h = mean(dat_51$ton_neg_ts_sm)+sd(dat_51$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_51$ton_neg_ts_sm)-sd(dat_51$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_51$peak <- 0
dat_51$peak[dat_51$n_ts_sm>mean(dat_51$n_ts_sm)+sd(dat_51$n_ts_sm)] <- 1
dat_51$peak[dat_51$n_ts_sm<mean(dat_51$n_ts_sm)-sd(dat_51$n_ts_sm)] <- -1
#Tonality
dat_51$peak_neg <- 0
dat_51$peak_neg[dat_51$ton_neg_ts_sm>mean(dat_51$ton_neg_ts_sm)+sd(dat_51$ton_neg_ts_sm)] <- 1
dat_51$peak_neg[dat_51$ton_neg_ts_sm<mean(dat_51$ton_neg_ts_sm)-sd(dat_51$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_51$entity_id <- 51

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_51$n <- NULL
dat_51$n_w <- NULL


#internet engineering task force
#----------
dat_52 <- dat_base[dat_base$entity_id==52,]
require(plyr)
dat_52 <- ddply(dat_52, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_52 <- dat_52[order(dat_52$year, dat_52$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_52 <- dat_52[dat_52$week != 53,]
dat_52 <- dat_52[!is.na(dat_52$week),]

#do we have censored data?
hist(dat_52$n, breaks = 100)
hist(dat_52$n_w, breaks = 100)
hist(dat_52$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_52$n)
adf.test(dat_52$n_w)
adf.test(dat_52$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_52$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_52$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_52$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_52$n_ts <- ts(dat_52$n)
dat_52$n_w_ts <- ts(dat_52$n_w)
dat_52$ton_neg_ts <- ts(dat_52$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_52$n_ts_sm <- sma(dat_52$n_ts, order = 4)$fitted
dat_52$n_w_ts_sm <- sma(dat_52$n_w_ts, order = 4)$fitted
dat_52$ton_neg_ts_sm <- sma(dat_52$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_52$week <- as.Date(paste(dat_52$years, dat_52$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_52.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_52$week, dat_52$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="internet engineering task force")
abline(h = mean(dat_52$n_ts_sm))
abline(h = mean(dat_52$n_ts_sm)+sd(dat_52$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_52$n_ts_sm)-sd(dat_52$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_52.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_52$week, dat_52$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="internet engineering task force")
abline(h = mean(dat_52$ton_neg_ts_sm))
abline(h = mean(dat_52$ton_neg_ts_sm)+sd(dat_52$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_52$ton_neg_ts_sm)-sd(dat_52$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_52$peak <- 0
dat_52$peak[dat_52$n_ts_sm>mean(dat_52$n_ts_sm)+sd(dat_52$n_ts_sm)] <- 1
dat_52$peak[dat_52$n_ts_sm<mean(dat_52$n_ts_sm)-sd(dat_52$n_ts_sm)] <- -1
#Tonality
dat_52$peak_neg <- 0
dat_52$peak_neg[dat_52$ton_neg_ts_sm>mean(dat_52$ton_neg_ts_sm)+sd(dat_52$ton_neg_ts_sm)] <- 1
dat_52$peak_neg[dat_52$ton_neg_ts_sm<mean(dat_52$ton_neg_ts_sm)-sd(dat_52$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_52$entity_id <- 52

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_52$n <- NULL
dat_52$n_w <- NULL

#internet governance forum
#----------
dat_53 <- dat_base[dat_base$entity_id==53,]
require(plyr)
dat_53 <- ddply(dat_53, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_53 <- dat_53[order(dat_53$year, dat_53$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_53 <- dat_53[dat_53$week != 53,]
dat_53 <- dat_53[!is.na(dat_53$week),]


#do we have censored data?
hist(dat_53$n, breaks = 100)
hist(dat_53$n_w, breaks = 100)
hist(dat_53$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_53$n)
adf.test(dat_53$n_w)
adf.test(dat_53$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_53$n) # monthly seasonality (seas = 5; monthly); 
findfrequency(dat_53$n_w) # monthly seasonality (seas = 5; monthly); 
findfrequency(dat_53$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_53$n_ts <- ts(dat_53$n)
dat_53$n_w_ts <- ts(dat_53$n_w)
dat_53$ton_neg_ts <- ts(dat_53$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_53$n_ts_sm <- sma(dat_53$n_ts, order = 4)$fitted
dat_53$n_w_ts_sm <- sma(dat_53$n_w_ts, order = 4)$fitted
dat_53$ton_neg_ts_sm <- sma(dat_53$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_53$week <- as.Date(paste(dat_53$years, dat_53$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_53.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_53$week, dat_53$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="internet engineering task force")
abline(h = mean(dat_53$n_ts_sm))
abline(h = mean(dat_53$n_ts_sm)+sd(dat_53$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_53$n_ts_sm)-sd(dat_53$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_53.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_53$week, dat_53$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="internet engineering task force")
abline(h = mean(dat_53$ton_neg_ts_sm))
abline(h = mean(dat_53$ton_neg_ts_sm)+sd(dat_53$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_53$ton_neg_ts_sm)-sd(dat_53$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_53$peak <- 0
dat_53$peak[dat_53$n_ts_sm>mean(dat_53$n_ts_sm)+sd(dat_53$n_ts_sm)] <- 1
dat_53$peak[dat_53$n_ts_sm<mean(dat_53$n_ts_sm)-sd(dat_53$n_ts_sm)] <- -1
#Tonality
dat_53$peak_neg <- 0
dat_53$peak_neg[dat_53$ton_neg_ts_sm>mean(dat_53$ton_neg_ts_sm)+sd(dat_53$ton_neg_ts_sm)] <- 1
dat_53$peak_neg[dat_53$ton_neg_ts_sm<mean(dat_53$ton_neg_ts_sm)-sd(dat_53$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_53$entity_id <- 53

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_53$n <- NULL
dat_53$n_w <- NULL


#internet society
#----------
dat_55 <- dat_base[dat_base$entity_id==55,]
require(plyr)
dat_55 <- ddply(dat_55, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_55 <- dat_55[order(dat_55$year, dat_55$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_55 <- dat_55[dat_55$week != 53,]
dat_55 <- dat_55[!is.na(dat_55$week),]

#do we have censored data?
hist(dat_55$n, breaks = 100)
hist(dat_55$n_w, breaks = 100)
hist(dat_55$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_55$n)
adf.test(dat_55$n_w)
adf.test(dat_55$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_55$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_55$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_55$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_55$n_ts <- ts(dat_55$n)
dat_55$n_w_ts <- ts(dat_55$n_w)
dat_55$ton_neg_ts <- ts(dat_55$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_55$n_ts_sm <- sma(dat_55$n_ts, order = 4)$fitted
dat_55$n_w_ts_sm <- sma(dat_55$n_w_ts, order = 4)$fitted
dat_55$ton_neg_ts_sm <- sma(dat_55$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_55$week <- as.Date(paste(dat_55$years, dat_55$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_55.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_55$week, dat_55$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="internet society")
abline(h = mean(dat_55$n_ts_sm))
abline(h = mean(dat_55$n_ts_sm)+sd(dat_55$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_55$n_ts_sm)-sd(dat_55$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_55.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_55$week, dat_55$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="internet society")
abline(h = mean(dat_55$ton_neg_ts_sm))
abline(h = mean(dat_55$ton_neg_ts_sm)+sd(dat_55$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_55$ton_neg_ts_sm)-sd(dat_55$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_55$peak <- 0
dat_55$peak[dat_55$n_ts_sm>mean(dat_55$n_ts_sm)+sd(dat_55$n_ts_sm)] <- 1
dat_55$peak[dat_55$n_ts_sm<mean(dat_55$n_ts_sm)-sd(dat_55$n_ts_sm)] <- -1
#Tonality
dat_55$peak_neg <- 0
dat_55$peak_neg[dat_55$ton_neg_ts_sm>mean(dat_55$ton_neg_ts_sm)+sd(dat_55$ton_neg_ts_sm)] <- 1
dat_55$peak_neg[dat_55$ton_neg_ts_sm<mean(dat_55$ton_neg_ts_sm)-sd(dat_55$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_55$entity_id <- 55

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_55$n <- NULL
dat_55$n_w <- NULL

#interparliamentary assembly of the eurasian economic community: 14 weeks with coverage, ~25 articles
#----------
dat_56 <- dat_base[dat_base$entity_id==56,]
require(plyr)
dat_56 <- ddply(dat_56, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_56 <- dat_56[order(dat_56$year, dat_56$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_56 <- dat_56[dat_56$week != 53,]
dat_56 <- dat_56[!is.na(dat_56$week),]


#do we have censored data?
hist(dat_56$n, breaks = 100)
hist(dat_56$n_w, breaks = 100)
hist(dat_56$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_56$n)
adf.test(dat_56$n_w)
#adf.test(dat_56$ton_neg) #not calculated
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_56$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_56$n_w) # monthly seasonality (seas = 1; weekly); 
#findfrequency(dat_56$ton_neg) # #not calculated 

#Build a time series
dat_56$n_ts <- ts(dat_56$n)
dat_56$n_w_ts <- ts(dat_56$n_w)
dat_56$ton_neg_ts <- ts(dat_56$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_56$n_ts_sm <- sma(dat_56$n_ts, order = 4)$fitted
dat_56$n_w_ts_sm <- sma(dat_56$n_w_ts, order = 4)$fitted
dat_56$ton_neg_ts_sm <- sma(dat_56$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_56$week <- as.Date(paste(dat_56$years, dat_56$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_56.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_56$week, dat_56$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="interparliamentary assembly of the eurasian economic community")
abline(h = mean(dat_56$n_ts_sm))
abline(h = mean(dat_56$n_ts_sm)+sd(dat_56$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_56$n_ts_sm)-sd(dat_56$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_56.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_56$week, dat_56$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="interparliamentary assembly of the eurasian economic community")
abline(h = mean(dat_56$ton_neg_ts_sm))
abline(h = mean(dat_56$ton_neg_ts_sm)+sd(dat_56$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_56$ton_neg_ts_sm)-sd(dat_56$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_56$peak <- 0
dat_56$peak[dat_56$n_ts_sm>mean(dat_56$n_ts_sm)+sd(dat_56$n_ts_sm)] <- 1
dat_56$peak[dat_56$n_ts_sm<mean(dat_56$n_ts_sm)-sd(dat_56$n_ts_sm)] <- -1
#Tonality
dat_56$peak_neg <- 0
dat_56$peak_neg[dat_56$ton_neg_ts_sm>mean(dat_56$ton_neg_ts_sm)+sd(dat_56$ton_neg_ts_sm)] <- 1
dat_56$peak_neg[dat_56$ton_neg_ts_sm<mean(dat_56$ton_neg_ts_sm)-sd(dat_56$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_56$entity_id <- 56

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_56$n <- NULL
dat_56$n_w <- NULL

#joint parliamentary assembly africa - caribbean - pacific - european union
#----------
dat_57 <- dat_base[dat_base$entity_id==57,]
require(plyr)
dat_57 <- ddply(dat_57, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_57 <- dat_57[order(dat_57$year, dat_57$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_57 <- dat_57[dat_57$week != 53,]
dat_57 <- dat_57[!is.na(dat_57$week),]


#do we have censored data?
hist(dat_57$n, breaks = 100)
hist(dat_57$n_w, breaks = 100)
hist(dat_57$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_57$n)
adf.test(dat_57$n_w)
adf.test(dat_57$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_57$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_57$n_w) # monthly seasonality (seas = 7; 1.5 monthly); 
findfrequency(dat_57$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_57$n_ts <- ts(dat_57$n)
dat_57$n_w_ts <- ts(dat_57$n_w)
dat_57$ton_neg_ts <- ts(dat_57$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_57$n_ts_sm <- sma(dat_57$n_ts, order = 4)$fitted
dat_57$n_w_ts_sm <- sma(dat_57$n_w_ts, order = 4)$fitted
dat_57$ton_neg_ts_sm <- sma(dat_57$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_57$week <- as.Date(paste(dat_57$years, dat_57$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_57.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_57$week, dat_57$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="joint parliamentary assembly africa - caribbean - pacific - european union")
abline(h = mean(dat_57$n_ts_sm))
abline(h = mean(dat_57$n_ts_sm)+sd(dat_57$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_57$n_ts_sm)-sd(dat_57$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_57.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_57$week, dat_57$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="joint parliamentary assembly africa - caribbean - pacific - european union")
abline(h = mean(dat_57$ton_neg_ts_sm))
abline(h = mean(dat_57$ton_neg_ts_sm)+sd(dat_57$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_57$ton_neg_ts_sm)-sd(dat_57$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_57$peak <- 0
dat_57$peak[dat_57$n_ts_sm>mean(dat_57$n_ts_sm)+sd(dat_57$n_ts_sm)] <- 1
dat_57$peak[dat_57$n_ts_sm<mean(dat_57$n_ts_sm)-sd(dat_57$n_ts_sm)] <- -1
#Tonality
dat_57$peak_neg <- 0
dat_57$peak_neg[dat_57$ton_neg_ts_sm>mean(dat_57$ton_neg_ts_sm)+sd(dat_57$ton_neg_ts_sm)] <- 1
dat_57$peak_neg[dat_57$ton_neg_ts_sm<mean(dat_57$ton_neg_ts_sm)-sd(dat_57$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_57$entity_id <- 57

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_57$n <- NULL
dat_57$n_w <- NULL

#kimberley process
#----------
dat_58 <- dat_base[dat_base$entity_id==58,]
require(plyr)
dat_58 <- ddply(dat_58, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_58 <- dat_58[order(dat_58$year, dat_58$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_58 <- dat_58[dat_58$week != 53,]
dat_58 <- dat_58[!is.na(dat_58$week),]


#do we have censored data?
hist(dat_58$n, breaks = 100)
hist(dat_58$n_w, breaks = 100)
hist(dat_58$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_58$n) #non-stationary
adf.test(dat_58$n_w) #non-stationary
adf.test(dat_58$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_58$n) # monthly seasonality (seas = 6; 1.5 monthly); 
findfrequency(dat_58$n_w) # monthly seasonality (seas = 6; 1.5 monthly); 
findfrequency(dat_58$ton_neg) # monthly seasonality (seas = 6; 1.5 monthly); 

#Build a time series
dat_58$n_ts <- ts(dat_58$n)
dat_58$n_w_ts <- ts(dat_58$n_w)
dat_58$ton_neg_ts <- ts(dat_58$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_58$n_ts_sm <- sma(dat_58$n_ts, order = 4)$fitted
dat_58$n_w_ts_sm <- sma(dat_58$n_w_ts, order = 4)$fitted
dat_58$ton_neg_ts_sm <- sma(dat_58$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_58$week <- as.Date(paste(dat_58$years, dat_58$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_58.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_58$week, dat_58$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="kimberley process")
abline(h = mean(dat_58$n_ts_sm))
abline(h = mean(dat_58$n_ts_sm)+sd(dat_58$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_58$n_ts_sm)-sd(dat_58$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_58.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_58$week, dat_58$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="kimberley process")
abline(h = mean(dat_58$ton_neg_ts_sm))
abline(h = mean(dat_58$ton_neg_ts_sm)+sd(dat_58$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_58$ton_neg_ts_sm)-sd(dat_58$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_58$peak <- 0
dat_58$peak[dat_58$n_ts_sm>mean(dat_58$n_ts_sm)+sd(dat_58$n_ts_sm)] <- 1
dat_58$peak[dat_58$n_ts_sm<mean(dat_58$n_ts_sm)-sd(dat_58$n_ts_sm)] <- -1
#Tonality
dat_58$peak_neg <- 0
dat_58$peak_neg[dat_58$ton_neg_ts_sm>mean(dat_58$ton_neg_ts_sm)+sd(dat_58$ton_neg_ts_sm)] <- 1
dat_58$peak_neg[dat_58$ton_neg_ts_sm<mean(dat_58$ton_neg_ts_sm)-sd(dat_58$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_58$entity_id <- 58

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_58$n <- NULL
dat_58$n_w <- NULL

#marine stewardship council
#----------
dat_60 <- dat_base[dat_base$entity_id==60,]
require(plyr)
dat_60 <- ddply(dat_60, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_60 <- dat_60[order(dat_60$year, dat_60$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_60 <- dat_60[dat_60$week != 53,]
dat_60 <- dat_60[!is.na(dat_60$week),]


#do we have censored data?
hist(dat_60$n, breaks = 100)
hist(dat_60$n_w, breaks = 100)
hist(dat_60$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_60$n)
adf.test(dat_60$n_w)
adf.test(dat_60$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_60$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_60$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_60$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_60$n_ts <- ts(dat_60$n)
dat_60$n_w_ts <- ts(dat_60$n_w)
dat_60$ton_neg_ts <- ts(dat_60$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_60$n_ts_sm <- sma(dat_60$n_ts, order = 4)$fitted
dat_60$n_w_ts_sm <- sma(dat_60$n_w_ts, order = 4)$fitted
dat_60$ton_neg_ts_sm <- sma(dat_60$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_60$week <- as.Date(paste(dat_60$years, dat_60$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_60.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_60$week, dat_60$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="marine stewardship council")
abline(h = mean(dat_60$n_ts_sm))
abline(h = mean(dat_60$n_ts_sm)+sd(dat_60$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_60$n_ts_sm)-sd(dat_60$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_60.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_60$week, dat_60$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="marine stewardship council")
abline(h = mean(dat_60$ton_neg_ts_sm))
abline(h = mean(dat_60$ton_neg_ts_sm)+sd(dat_60$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_60$ton_neg_ts_sm)-sd(dat_60$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_60$peak <- 0
dat_60$peak[dat_60$n_ts_sm>mean(dat_60$n_ts_sm)+sd(dat_60$n_ts_sm)] <- 1
dat_60$peak[dat_60$n_ts_sm<mean(dat_60$n_ts_sm)-sd(dat_60$n_ts_sm)] <- -1
#Tonality
dat_60$peak_neg <- 0
dat_60$peak_neg[dat_60$ton_neg_ts_sm>mean(dat_60$ton_neg_ts_sm)+sd(dat_60$ton_neg_ts_sm)] <- 1
dat_60$peak_neg[dat_60$ton_neg_ts_sm<mean(dat_60$ton_neg_ts_sm)-sd(dat_60$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_60$entity_id <- 60

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_60$n <- NULL
dat_60$n_w <- NULL

#moody's
#----------
dat_61 <- dat_base[dat_base$entity_id==61,]
require(plyr)
dat_61 <- ddply(dat_61, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_61 <- dat_61[order(dat_61$year, dat_61$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_61 <- dat_61[dat_61$week != 53,]
dat_61 <- dat_61[!is.na(dat_61$week),]

#do we have censored data?
hist(dat_61$n, breaks = 100)
hist(dat_61$n_w, breaks = 100)
hist(dat_61$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_61$n)
adf.test(dat_61$n_w)
adf.test(dat_61$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_61$n) # monthly seasonality (seas = 19; 5-monthly); 
findfrequency(dat_61$n_w) # monthly seasonality (seas = 19; 5-monthly); 
findfrequency(dat_61$ton_neg) # monthly seasonality (seas = 21; 5-monthly); 

#Build a time series
dat_61$n_ts <- ts(dat_61$n)
dat_61$n_w_ts <- ts(dat_61$n_w)
dat_61$ton_neg_ts <- ts(dat_61$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_61$n_ts_sm <- sma(dat_61$n_ts, order = 4)$fitted
dat_61$n_w_ts_sm <- sma(dat_61$n_w_ts, order = 4)$fitted
dat_61$ton_neg_ts_sm <- sma(dat_61$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_61$week <- as.Date(paste(dat_61$years, dat_61$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_61.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_61$week, dat_61$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="moody's")
abline(h = mean(dat_61$n_ts_sm))
abline(h = mean(dat_61$n_ts_sm)+sd(dat_61$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_61$n_ts_sm)-sd(dat_61$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_61.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_61$week, dat_61$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="moody's")
abline(h = mean(dat_61$ton_neg_ts_sm))
abline(h = mean(dat_61$ton_neg_ts_sm)+sd(dat_61$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_61$ton_neg_ts_sm)-sd(dat_61$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_61$peak <- 0
dat_61$peak[dat_61$n_ts_sm>mean(dat_61$n_ts_sm)+sd(dat_61$n_ts_sm)] <- 1
dat_61$peak[dat_61$n_ts_sm<mean(dat_61$n_ts_sm)-sd(dat_61$n_ts_sm)] <- -1
#Tonality
dat_61$peak_neg <- 0
dat_61$peak_neg[dat_61$ton_neg_ts_sm>mean(dat_61$ton_neg_ts_sm)+sd(dat_61$ton_neg_ts_sm)] <- 1
dat_61$peak_neg[dat_61$ton_neg_ts_sm<mean(dat_61$ton_neg_ts_sm)-sd(dat_61$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_61$entity_id <- 61

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_61$n <- NULL
dat_61$n_w <- NULL


#organization for economic cooperation & development
#----------
dat_62 <- dat_base[dat_base$entity_id==62,]
require(plyr)
dat_62 <- ddply(dat_62, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_62 <- dat_62[order(dat_62$year, dat_62$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_62 <- dat_62[dat_62$week != 53,]
dat_62 <- dat_62[!is.na(dat_62$week),]

#do we have censored data?
hist(dat_62$n, breaks = 100)
hist(dat_62$n_w, breaks = 100)
hist(dat_62$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_62$n)
adf.test(dat_62$n_w)
adf.test(dat_62$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_62$n) # monthly seasonality (seas = 13; 3-monthly); 
findfrequency(dat_62$n_w) # monthly seasonality (seas = 13; 3-monthly); 
findfrequency(dat_62$ton_neg) # monthly seasonality (seas = 13; 3-monthly); 

#Build a time series
dat_62$n_ts <- ts(dat_62$n)
dat_62$n_w_ts <- ts(dat_62$n_w)
dat_62$ton_neg_ts <- ts(dat_62$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_62$n_ts_sm <- sma(dat_62$n_ts, order = 4)$fitted
dat_62$n_w_ts_sm <- sma(dat_62$n_w_ts, order = 4)$fitted
dat_62$ton_neg_ts_sm <- sma(dat_62$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_62$week <- as.Date(paste(dat_62$years, dat_62$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_62.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_62$week, dat_62$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="organization for economic cooperation & development")
abline(h = mean(dat_62$n_ts_sm))
abline(h = mean(dat_62$n_ts_sm)+sd(dat_62$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_62$n_ts_sm)-sd(dat_62$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_62.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_62$week, dat_62$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="organization for economic cooperation & development")
abline(h = mean(dat_62$ton_neg_ts_sm))
abline(h = mean(dat_62$ton_neg_ts_sm)+sd(dat_62$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_62$ton_neg_ts_sm)-sd(dat_62$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_62$peak <- 0
dat_62$peak[dat_62$n_ts_sm>mean(dat_62$n_ts_sm)+sd(dat_62$n_ts_sm)] <- 1
dat_62$peak[dat_62$n_ts_sm<mean(dat_62$n_ts_sm)-sd(dat_62$n_ts_sm)] <- -1
#Tonality
dat_62$peak_neg <- 0
dat_62$peak_neg[dat_62$ton_neg_ts_sm>mean(dat_62$ton_neg_ts_sm)+sd(dat_62$ton_neg_ts_sm)] <- 1
dat_62$peak_neg[dat_62$ton_neg_ts_sm<mean(dat_62$ton_neg_ts_sm)-sd(dat_62$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_62$entity_id <- 62

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_62$n <- NULL
dat_62$n_w <- NULL


#organization for security and cooperation in europe
#----------
dat_63 <- dat_base[dat_base$entity_id==63,]
require(plyr)
dat_63 <- ddply(dat_63, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_63 <- dat_63[order(dat_63$year, dat_63$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_63 <- dat_63[dat_63$week != 53,]
dat_63 <- dat_63[!is.na(dat_63$week),]

#do we have censored data?
hist(dat_63$n, breaks = 100)
hist(dat_63$n_w, breaks = 100)
hist(dat_63$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_63$n)
adf.test(dat_63$n_w)
adf.test(dat_63$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_63$n) # monthly seasonality (seas = 9; 2-monthly); 
findfrequency(dat_63$n_w) # monthly seasonality (seas = 9; 2-monthly); 
findfrequency(dat_63$ton_neg) # monthly seasonality (seas = 13; 3-monthly); 

#Build a time series
dat_63$n_ts <- ts(dat_63$n)
dat_63$n_w_ts <- ts(dat_63$n_w)
dat_63$ton_neg_ts <- ts(dat_63$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_63$n_ts_sm <- sma(dat_63$n_ts, order = 4)$fitted
dat_63$n_w_ts_sm <- sma(dat_63$n_w_ts, order = 4)$fitted
dat_63$ton_neg_ts_sm <- sma(dat_63$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_63$week <- as.Date(paste(dat_63$years, dat_63$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_63.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_63$week, dat_63$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="organization for security and cooperation in europe")
abline(h = mean(dat_63$n_ts_sm))
abline(h = mean(dat_63$n_ts_sm)+sd(dat_63$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_63$n_ts_sm)-sd(dat_63$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_63.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_63$week, dat_63$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="organization for security and cooperation in europe")
abline(h = mean(dat_63$ton_neg_ts_sm))
abline(h = mean(dat_63$ton_neg_ts_sm)+sd(dat_63$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_63$ton_neg_ts_sm)-sd(dat_63$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_63$peak <- 0
dat_63$peak[dat_63$n_ts_sm>mean(dat_63$n_ts_sm)+sd(dat_63$n_ts_sm)] <- 1
dat_63$peak[dat_63$n_ts_sm<mean(dat_63$n_ts_sm)-sd(dat_63$n_ts_sm)] <- -1
#Tonality
dat_63$peak_neg <- 0
dat_63$peak_neg[dat_63$ton_neg_ts_sm>mean(dat_63$ton_neg_ts_sm)+sd(dat_63$ton_neg_ts_sm)] <- 1
dat_63$peak_neg[dat_63$ton_neg_ts_sm<mean(dat_63$ton_neg_ts_sm)-sd(dat_63$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_63$entity_id <- 63

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_63$n <- NULL
dat_63$n_w <- NULL


#parliamentary assembly of the council of europe
#----------
dat_64 <- dat_base[dat_base$entity_id==64,]
require(plyr)
dat_64 <- ddply(dat_64, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_64 <- dat_64[order(dat_64$year, dat_64$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_64 <- dat_64[dat_64$week != 53,]
dat_64 <- dat_64[!is.na(dat_64$week),]

#do we have censored data?
hist(dat_64$n, breaks = 100)
hist(dat_64$n_w, breaks = 100)
hist(dat_64$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_64$n)
adf.test(dat_64$n_w)
adf.test(dat_64$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_64$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_64$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_64$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_64$n_ts <- ts(dat_64$n)
dat_64$n_w_ts <- ts(dat_64$n_w)
dat_64$ton_neg_ts <- ts(dat_64$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_64$n_ts_sm <- sma(dat_64$n_ts, order = 4)$fitted
dat_64$n_w_ts_sm <- sma(dat_64$n_w_ts, order = 4)$fitted
dat_64$ton_neg_ts_sm <- sma(dat_64$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_64$week <- as.Date(paste(dat_64$years, dat_64$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_64.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_64$week, dat_64$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="parliamentary assembly of the council of europe")
abline(h = mean(dat_64$n_ts_sm))
abline(h = mean(dat_64$n_ts_sm)+sd(dat_64$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_64$n_ts_sm)-sd(dat_64$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_64.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_64$week, dat_64$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="parliamentary assembly of the council of europe")
abline(h = mean(dat_64$ton_neg_ts_sm))
abline(h = mean(dat_64$ton_neg_ts_sm)+sd(dat_64$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_64$ton_neg_ts_sm)-sd(dat_64$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_64$peak <- 0
dat_64$peak[dat_64$n_ts_sm>mean(dat_64$n_ts_sm)+sd(dat_64$n_ts_sm)] <- 1
dat_64$peak[dat_64$n_ts_sm<mean(dat_64$n_ts_sm)-sd(dat_64$n_ts_sm)] <- -1
#Tonality
dat_64$peak_neg <- 0
dat_64$peak_neg[dat_64$ton_neg_ts_sm>mean(dat_64$ton_neg_ts_sm)+sd(dat_64$ton_neg_ts_sm)] <- 1
dat_64$peak_neg[dat_64$ton_neg_ts_sm<mean(dat_64$ton_neg_ts_sm)-sd(dat_64$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_64$entity_id <- 64

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_64$n <- NULL
dat_64$n_w <- NULL


#programme for the endorsement of forest certification: 12 weeks with coverage, ~13 articles
#----------
dat_66 <- dat_base[dat_base$entity_id==66,]
require(plyr)
dat_66 <- ddply(dat_66, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_66 <- dat_66[order(dat_66$year, dat_66$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_66 <- dat_66[dat_66$week != 53,]
dat_66 <- dat_66[!is.na(dat_66$week),]

#do we have censored data?
hist(dat_66$n, breaks = 100)
hist(dat_66$n_w, breaks = 100)
hist(dat_66$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_66$n) #non-stationary
adf.test(dat_66$n_w) #non-stationary
#adf.test(dat_66$ton_neg) #not calculated
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_66$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_66$n_w) # monthly seasonality (seas = 1; weekly); 
#findfrequency(dat_66$ton_neg) # not calculated 

#Build a time series
dat_66$n_ts <- ts(dat_66$n)
dat_66$n_w_ts <- ts(dat_66$n_w)
dat_66$ton_neg_ts <- ts(dat_66$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_66$n_ts_sm <- sma(dat_66$n_ts, order = 4)$fitted
dat_66$n_w_ts_sm <- sma(dat_66$n_w_ts, order = 4)$fitted
dat_66$ton_neg_ts_sm <- sma(dat_66$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_66$week <- as.Date(paste(dat_66$years, dat_66$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_66.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_66$week, dat_66$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="programme for the endorsement of forest certification")
abline(h = mean(dat_66$n_ts_sm))
abline(h = mean(dat_66$n_ts_sm)+sd(dat_66$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_66$n_ts_sm)-sd(dat_66$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_66.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_66$week, dat_66$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="programme for the endorsement of forest certification")
abline(h = mean(dat_66$ton_neg_ts_sm))
abline(h = mean(dat_66$ton_neg_ts_sm)+sd(dat_66$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_66$ton_neg_ts_sm)-sd(dat_66$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_66$peak <- 0
dat_66$peak[dat_66$n_ts_sm>mean(dat_66$n_ts_sm)+sd(dat_66$n_ts_sm)] <- 1
dat_66$peak[dat_66$n_ts_sm<mean(dat_66$n_ts_sm)-sd(dat_66$n_ts_sm)] <- -1
#Tonality
dat_66$peak_neg <- 0
dat_66$peak_neg[dat_66$ton_neg_ts_sm>mean(dat_66$ton_neg_ts_sm)+sd(dat_66$ton_neg_ts_sm)] <- 1
dat_66$peak_neg[dat_66$ton_neg_ts_sm<mean(dat_66$ton_neg_ts_sm)-sd(dat_66$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_66$entity_id <- 66

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_66$n <- NULL
dat_66$n_w <- NULL


#rainforest alliance
#----------
dat_67 <- dat_base[dat_base$entity_id==67,]
require(plyr)
dat_67 <- ddply(dat_67, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_67 <- dat_67[order(dat_67$year, dat_67$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_67 <- dat_67[dat_67$week != 53,]
dat_67 <- dat_67[!is.na(dat_67$week),]

#do we have censored data?
hist(dat_67$n, breaks = 100)
hist(dat_67$n_w, breaks = 100)
hist(dat_67$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_67$n) #non-stationary
adf.test(dat_67$n_w) #non-stationary
adf.test(dat_67$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_67$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_67$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_67$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_67$n_ts <- ts(dat_67$n)
dat_67$n_w_ts <- ts(dat_67$n_w)
dat_67$ton_neg_ts <- ts(dat_67$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_67$n_ts_sm <- sma(dat_67$n_ts, order = 4)$fitted
dat_67$n_w_ts_sm <- sma(dat_67$n_w_ts, order = 4)$fitted
dat_67$ton_neg_ts_sm <- sma(dat_67$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_67$week <- as.Date(paste(dat_67$years, dat_67$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_67.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_67$week, dat_67$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="rainforest alliance")
abline(h = mean(dat_67$n_ts_sm))
abline(h = mean(dat_67$n_ts_sm)+sd(dat_67$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_67$n_ts_sm)-sd(dat_67$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_67.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_67$week, dat_67$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="rainforest alliance")
abline(h = mean(dat_67$ton_neg_ts_sm))
abline(h = mean(dat_67$ton_neg_ts_sm)+sd(dat_67$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_67$ton_neg_ts_sm)-sd(dat_67$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_67$peak <- 0
dat_67$peak[dat_67$n_ts_sm>mean(dat_67$n_ts_sm)+sd(dat_67$n_ts_sm)] <- 1
dat_67$peak[dat_67$n_ts_sm<mean(dat_67$n_ts_sm)-sd(dat_67$n_ts_sm)] <- -1
#Tonality
dat_67$peak_neg <- 0
dat_67$peak_neg[dat_67$ton_neg_ts_sm>mean(dat_67$ton_neg_ts_sm)+sd(dat_67$ton_neg_ts_sm)] <- 1
dat_67$peak_neg[dat_67$ton_neg_ts_sm<mean(dat_67$ton_neg_ts_sm)-sd(dat_67$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_67$entity_id <- 67

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_67$n <- NULL
dat_67$n_w <- NULL


#round table on sustainable development: 3 weeks with coverage, ~3 articles
#----------
dat_68 <- dat_base[dat_base$entity_id==68,]
require(plyr)
dat_68 <- ddply(dat_68, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_68 <- dat_68[order(dat_68$year, dat_68$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_68 <- dat_68[dat_68$week != 53,]
dat_68 <- dat_68[!is.na(dat_68$week),]

#do we have censored data?
hist(dat_68$n, breaks = 100)
hist(dat_68$n_w, breaks = 100)
hist(dat_68$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_68$n_ts <- dat_68$n
dat_68$n_w_ts <- dat_68$n_w
dat_68$ton_neg_ts <- dat_68$ton_neg

#duplicate variable with different name
dat_68$n_ts_sm <- dat_68$n_ts
dat_68$n_w_ts_sm <- dat_68$n_w_ts
dat_68$ton_neg_ts_sm <- dat_68$ton_neg_ts

# create starting date of every week
dat_68$week <- as.Date(paste(dat_68$years, dat_68$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_68.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_68$week, dat_68$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="round table on sustainable development")
abline(h = mean(dat_68$n_ts_sm))
abline(h = mean(dat_68$n_ts_sm)+sd(dat_68$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_68$n_ts_sm)-sd(dat_68$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_68.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_68$week, dat_68$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="round table on sustainable development")
abline(h = mean(dat_68$ton_neg_ts_sm))
abline(h = mean(dat_68$ton_neg_ts_sm)+sd(dat_68$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_68$ton_neg_ts_sm)-sd(dat_68$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_68$peak <- 0
dat_68$peak[dat_68$n_ts_sm>mean(dat_68$n_ts_sm)+sd(dat_68$n_ts_sm)] <- 1
dat_68$peak[dat_68$n_ts_sm<mean(dat_68$n_ts_sm)-sd(dat_68$n_ts_sm)] <- -1
#Tonality
dat_68$peak_neg <- 0
dat_68$peak_neg[dat_68$ton_neg_ts_sm>mean(dat_68$ton_neg_ts_sm)+sd(dat_68$ton_neg_ts_sm)] <- 1
dat_68$peak_neg[dat_68$ton_neg_ts_sm<mean(dat_68$ton_neg_ts_sm)-sd(dat_68$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_68$entity_id <- 68

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_68$n <- NULL
dat_68$n_w <- NULL


#société générale de surveillance
#----------
dat_69 <- dat_base[dat_base$entity_id==69,]
require(plyr)
dat_69 <- ddply(dat_69, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_69 <- dat_69[order(dat_69$year, dat_69$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_69 <- dat_69[dat_69$week != 53,]
dat_69 <- dat_69[!is.na(dat_69$week),]

#do we have censored data?
hist(dat_69$n, breaks = 100)
hist(dat_69$n_w, breaks = 100)
hist(dat_69$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_69$n) #non-stationary
adf.test(dat_69$n_w) #non-stationary
adf.test(dat_69$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_69$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_69$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_69$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_69$n_ts <- ts(dat_69$n)
dat_69$n_w_ts <- ts(dat_69$n_w)
dat_69$ton_neg_ts <- ts(dat_69$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_69$n_ts_sm <- sma(dat_69$n_ts, order = 4)$fitted
dat_69$n_w_ts_sm <- sma(dat_69$n_w_ts, order = 4)$fitted
dat_69$ton_neg_ts_sm <- sma(dat_69$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_69$week <- as.Date(paste(dat_69$years, dat_69$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_69.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_69$week, dat_69$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="société générale de surveillance")
abline(h = mean(dat_69$n_ts_sm))
abline(h = mean(dat_69$n_ts_sm)+sd(dat_69$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_69$n_ts_sm)-sd(dat_69$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_69.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_69$week, dat_69$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="société générale de surveillance")
abline(h = mean(dat_69$ton_neg_ts_sm))
abline(h = mean(dat_69$ton_neg_ts_sm)+sd(dat_69$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_69$ton_neg_ts_sm)-sd(dat_69$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_69$peak <- 0
dat_69$peak[dat_69$n_ts_sm>mean(dat_69$n_ts_sm)+sd(dat_69$n_ts_sm)] <- 1
dat_69$peak[dat_69$n_ts_sm<mean(dat_69$n_ts_sm)-sd(dat_69$n_ts_sm)] <- -1
#Tonality
dat_69$peak_neg <- 0
dat_69$peak_neg[dat_69$ton_neg_ts_sm>mean(dat_69$ton_neg_ts_sm)+sd(dat_69$ton_neg_ts_sm)] <- 1
dat_69$peak_neg[dat_69$ton_neg_ts_sm<mean(dat_69$ton_neg_ts_sm)-sd(dat_69$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_69$entity_id <- 69

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_69$n <- NULL
dat_69$n_w <- NULL


#standard & poor's
#----------
dat_70 <- dat_base[dat_base$entity_id==70,]
require(plyr)
dat_70 <- ddply(dat_70, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_70 <- dat_70[order(dat_70$year, dat_70$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_70 <- dat_70[dat_70$week != 53,]
dat_70 <- dat_70[!is.na(dat_70$week),]

#do we have censored data?
hist(dat_70$n, breaks = 100)
hist(dat_70$n_w, breaks = 100)
hist(dat_70$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_70$n)
adf.test(dat_70$n_w)
adf.test(dat_70$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_70$n) # monthly seasonality (seas = 7; 1.5 monthly); 
findfrequency(dat_70$n_w) # monthly seasonality (seas = 7; 1.5 monthly); 
findfrequency(dat_70$ton_neg) # monthly seasonality (seas = 6; 1.5 monthly); 

#Build a time series
dat_70$n_ts <- ts(dat_70$n)
dat_70$n_w_ts <- ts(dat_70$n_w)
dat_70$ton_neg_ts <- ts(dat_70$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_70$n_ts_sm <- sma(dat_70$n_ts, order = 4)$fitted
dat_70$n_w_ts_sm <- sma(dat_70$n_w_ts, order = 4)$fitted
dat_70$ton_neg_ts_sm <- sma(dat_70$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_70$week <- as.Date(paste(dat_70$years, dat_70$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_70.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_70$week, dat_70$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="standard & poor's")
abline(h = mean(dat_70$n_ts_sm))
abline(h = mean(dat_70$n_ts_sm)+sd(dat_70$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_70$n_ts_sm)-sd(dat_70$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_70.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_70$week, dat_70$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="standard & poor's")
abline(h = mean(dat_70$ton_neg_ts_sm))
abline(h = mean(dat_70$ton_neg_ts_sm)+sd(dat_70$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_70$ton_neg_ts_sm)-sd(dat_70$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_70$peak <- 0
dat_70$peak[dat_70$n_ts_sm>mean(dat_70$n_ts_sm)+sd(dat_70$n_ts_sm)] <- 1
dat_70$peak[dat_70$n_ts_sm<mean(dat_70$n_ts_sm)-sd(dat_70$n_ts_sm)] <- -1
#Tonality
dat_70$peak_neg <- 0
dat_70$peak_neg[dat_70$ton_neg_ts_sm>mean(dat_70$ton_neg_ts_sm)+sd(dat_70$ton_neg_ts_sm)] <- 1
dat_70$peak_neg[dat_70$ton_neg_ts_sm<mean(dat_70$ton_neg_ts_sm)-sd(dat_70$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_70$entity_id <- 70

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_70$n <- NULL
dat_70$n_w <- NULL


#transport for london
#----------
dat_73 <- dat_base[dat_base$entity_id==73,]
require(plyr)
dat_73 <- ddply(dat_73, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_73 <- dat_73[order(dat_73$year, dat_73$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_73 <- dat_73[dat_73$week != 53,]
dat_73 <- dat_73[!is.na(dat_73$week),]


#do we have censored data?
hist(dat_73$n, breaks = 100)
hist(dat_73$n_w, breaks = 100)
hist(dat_73$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_73$n)
adf.test(dat_73$n_w)
adf.test(dat_73$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_73$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_73$n_w) # monthly seasonality (seas = 13; 3-monthly); 
findfrequency(dat_73$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_73$n_ts <- ts(dat_73$n)
dat_73$n_w_ts <- ts(dat_73$n_w)
dat_73$ton_neg_ts <- ts(dat_73$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_73$n_ts_sm <- sma(dat_73$n_ts, order = 4)$fitted
dat_73$n_w_ts_sm <- sma(dat_73$n_w_ts, order = 4)$fitted
dat_73$ton_neg_ts_sm <- sma(dat_73$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_73$week <- as.Date(paste(dat_73$years, dat_73$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_73.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_73$week, dat_73$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="transport for london")
abline(h = mean(dat_73$n_ts_sm))
abline(h = mean(dat_73$n_ts_sm)+sd(dat_73$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_73$n_ts_sm)-sd(dat_73$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_73.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_73$week, dat_73$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="transport for london")
abline(h = mean(dat_73$ton_neg_ts_sm))
abline(h = mean(dat_73$ton_neg_ts_sm)+sd(dat_73$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_73$ton_neg_ts_sm)-sd(dat_73$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_73$peak <- 0
dat_73$peak[dat_73$n_ts_sm>mean(dat_73$n_ts_sm)+sd(dat_73$n_ts_sm)] <- 1
dat_73$peak[dat_73$n_ts_sm<mean(dat_73$n_ts_sm)-sd(dat_73$n_ts_sm)] <- -1
#Tonality
dat_73$peak_neg <- 0
dat_73$peak_neg[dat_73$ton_neg_ts_sm>mean(dat_73$ton_neg_ts_sm)+sd(dat_73$ton_neg_ts_sm)] <- 1
dat_73$peak_neg[dat_73$ton_neg_ts_sm<mean(dat_73$ton_neg_ts_sm)-sd(dat_73$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_73$entity_id <- 73

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_73$n <- NULL
dat_73$n_w <- NULL


#tüv austria: only 1 article
#----------
dat_74 <- dat_base[dat_base$entity_id==74,]
require(plyr)
dat_74 <- ddply(dat_74, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_74 <- dat_74[order(dat_74$year, dat_74$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_74 <- dat_74[dat_74$week != 53,]
dat_74 <- dat_74[!is.na(dat_74$week),]

#do we have censored data?
hist(dat_74$n, breaks = 100)
hist(dat_74$n_w, breaks = 100)
hist(dat_74$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_74$n_ts <- dat_74$n
dat_74$n_w_ts <- dat_74$n_w
dat_74$ton_neg_ts <- dat_74$ton_neg

#duplicate variable with different name
dat_74$n_ts_sm <- dat_74$n_ts
dat_74$n_w_ts_sm <- dat_74$n_w_ts
dat_74$ton_neg_ts_sm <- dat_74$ton_neg_ts

# create starting date of every week
dat_74$week <- as.Date(paste(dat_74$years, dat_74$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_74.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_74$week, dat_74$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="tüv austria")
abline(h = mean(dat_74$n_ts_sm))
abline(h = mean(dat_74$n_ts_sm)+sd(dat_74$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_74$n_ts_sm)-sd(dat_74$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_74.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_74$week, dat_74$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="tüv austria")
abline(h = mean(dat_74$ton_neg_ts_sm))
abline(h = mean(dat_74$ton_neg_ts_sm)+sd(dat_74$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_74$ton_neg_ts_sm)-sd(dat_74$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_74$peak <- 0
dat_74$peak[dat_74$n_ts_sm>mean(dat_74$n_ts_sm)+sd(dat_74$n_ts_sm)] <- 1
dat_74$peak[dat_74$n_ts_sm<mean(dat_74$n_ts_sm)-sd(dat_74$n_ts_sm)] <- -1
#Tonality
dat_74$peak_neg <- 0
dat_74$peak_neg[dat_74$ton_neg_ts_sm>mean(dat_74$ton_neg_ts_sm)+sd(dat_74$ton_neg_ts_sm)] <- 1
dat_74$peak_neg[dat_74$ton_neg_ts_sm<mean(dat_74$ton_neg_ts_sm)-sd(dat_74$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_74$entity_id <- 74

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_74$n <- NULL
dat_74$n_w <- NULL


#tüv nord: 7 weeks with coverage, 8 articles
#----------
dat_77 <- dat_base[dat_base$entity_id==77,]
require(plyr)
dat_77 <- ddply(dat_77, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_77 <- dat_77[order(dat_77$year, dat_77$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_77 <- dat_77[dat_77$week != 53,]
dat_77 <- dat_77[!is.na(dat_77$week),]

#do we have censored data?
hist(dat_77$n, breaks = 100)
hist(dat_77$n_w, breaks = 100)
hist(dat_77$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_77$n)
adf.test(dat_77$n_w)
adf.test(dat_77$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_77$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_77$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_77$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_77$n_ts <- ts(dat_77$n)
dat_77$n_w_ts <- ts(dat_77$n_w)
dat_77$ton_neg_ts <- ts(dat_77$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_77$n_ts_sm <- sma(dat_77$n_ts, order = 4)$fitted
dat_77$n_w_ts_sm <- sma(dat_77$n_w_ts, order = 4)$fitted
dat_77$ton_neg_ts_sm <- sma(dat_77$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_77$week <- as.Date(paste(dat_77$years, dat_77$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_77.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_77$week, dat_77$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="tüv nord")
abline(h = mean(dat_77$n_ts_sm))
abline(h = mean(dat_77$n_ts_sm)+sd(dat_77$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_77$n_ts_sm)-sd(dat_77$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_77.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_77$week, dat_77$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="tüv nord")
abline(h = mean(dat_77$ton_neg_ts_sm))
abline(h = mean(dat_77$ton_neg_ts_sm)+sd(dat_77$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_77$ton_neg_ts_sm)-sd(dat_77$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_77$peak <- 0
dat_77$peak[dat_77$n_ts_sm>mean(dat_77$n_ts_sm)+sd(dat_77$n_ts_sm)] <- 1
dat_77$peak[dat_77$n_ts_sm<mean(dat_77$n_ts_sm)-sd(dat_77$n_ts_sm)] <- -1
#Tonality
dat_77$peak_neg <- 0
dat_77$peak_neg[dat_77$ton_neg_ts_sm>mean(dat_77$ton_neg_ts_sm)+sd(dat_77$ton_neg_ts_sm)] <- 1
dat_77$peak_neg[dat_77$ton_neg_ts_sm<mean(dat_77$ton_neg_ts_sm)-sd(dat_77$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_77$entity_id <- 77

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_77$n <- NULL
dat_77$n_w <- NULL


#tüv rheinland
#----------
dat_78 <- dat_base[dat_base$entity_id==78,]
require(plyr)
dat_78 <- ddply(dat_78, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_78 <- dat_78[order(dat_78$year, dat_78$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_78 <- dat_78[dat_78$week != 53,]
dat_78 <- dat_78[!is.na(dat_78$week),]


#do we have censored data?
hist(dat_78$n, breaks = 100)
hist(dat_78$n_w, breaks = 100)
hist(dat_78$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_78$n) 
adf.test(dat_78$n_w) 
adf.test(dat_78$ton_neg) 
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_78$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_78$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_78$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_78$n_ts <- ts(dat_78$n)
dat_78$n_w_ts <- ts(dat_78$n_w)
dat_78$ton_neg_ts <- ts(dat_78$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_78$n_ts_sm <- sma(dat_78$n_ts, order = 4)$fitted
dat_78$n_w_ts_sm <- sma(dat_78$n_w_ts, order = 4)$fitted
dat_78$ton_neg_ts_sm <- sma(dat_78$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_78$week <- as.Date(paste(dat_78$years, dat_78$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_78.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_78$week, dat_78$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="tüv rheinland")
abline(h = mean(dat_78$n_ts_sm))
abline(h = mean(dat_78$n_ts_sm)+sd(dat_78$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_78$n_ts_sm)-sd(dat_78$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_78.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_78$week, dat_78$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="tüv rheinland")
abline(h = mean(dat_78$ton_neg_ts_sm))
abline(h = mean(dat_78$ton_neg_ts_sm)+sd(dat_78$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_78$ton_neg_ts_sm)-sd(dat_78$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_78$peak <- 0
dat_78$peak[dat_78$n_ts_sm>mean(dat_78$n_ts_sm)+sd(dat_78$n_ts_sm)] <- 1
dat_78$peak[dat_78$n_ts_sm<mean(dat_78$n_ts_sm)-sd(dat_78$n_ts_sm)] <- -1
#Tonality
dat_78$peak_neg <- 0
dat_78$peak_neg[dat_78$ton_neg_ts_sm>mean(dat_78$ton_neg_ts_sm)+sd(dat_78$ton_neg_ts_sm)] <- 1
dat_78$peak_neg[dat_78$ton_neg_ts_sm<mean(dat_78$ton_neg_ts_sm)-sd(dat_78$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_78$entity_id <- 78

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_78$n <- NULL
dat_78$n_w <- NULL


#tüv saarland: only 1 1 article
#----------
dat_79 <- dat_base[dat_base$entity_id==79,]
require(plyr)
dat_79 <- ddply(dat_79, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_79 <- dat_79[order(dat_79$year, dat_79$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_79 <- dat_79[dat_79$week != 53,]
dat_79 <- dat_79[!is.na(dat_79$week),]

#do we have censored data?
hist(dat_79$n, breaks = 100)
hist(dat_79$n_w, breaks = 100)
hist(dat_79$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

#Duplicate variable with different name
dat_79$n_ts <- dat_79$n
dat_79$n_w_ts <- dat_79$n_w
dat_79$ton_neg_ts <- dat_79$ton_neg

#duplicate variable with different name
dat_79$n_ts_sm <- dat_79$n_ts
dat_79$n_w_ts_sm <- dat_79$n_w_ts
dat_79$ton_neg_ts_sm <- dat_79$ton_neg_ts

# create starting date of every week
dat_79$week <- as.Date(paste(dat_79$years, dat_79$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_79.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_79$week, dat_79$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="tüv saarland")
abline(h = mean(dat_79$n_ts_sm))
abline(h = mean(dat_79$n_ts_sm)+sd(dat_79$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_79$n_ts_sm)-sd(dat_79$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_79.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_79$week, dat_79$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="tüv saarland")
abline(h = mean(dat_79$ton_neg_ts_sm))
abline(h = mean(dat_79$ton_neg_ts_sm)+sd(dat_79$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_79$ton_neg_ts_sm)-sd(dat_79$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_79$peak <- 0
dat_79$peak[dat_79$n_ts_sm>mean(dat_79$n_ts_sm)+sd(dat_79$n_ts_sm)] <- 1
dat_79$peak[dat_79$n_ts_sm<mean(dat_79$n_ts_sm)-sd(dat_79$n_ts_sm)] <- -1
#Tonality
dat_79$peak_neg <- 0
dat_79$peak_neg[dat_79$ton_neg_ts_sm>mean(dat_79$ton_neg_ts_sm)+sd(dat_79$ton_neg_ts_sm)] <- 1
dat_79$peak_neg[dat_79$ton_neg_ts_sm<mean(dat_79$ton_neg_ts_sm)-sd(dat_79$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_79$entity_id <- 79

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_79$n <- NULL
dat_79$n_w <- NULL


#tüv süd
#----------
dat_80 <- dat_base[dat_base$entity_id==80,]
require(plyr)
dat_80 <- ddply(dat_80, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_80 <- dat_80[order(dat_80$year, dat_80$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_80 <- dat_80[dat_80$week != 53,]
dat_80 <- dat_80[!is.na(dat_80$week),]

#do we have censored data?
hist(dat_80$n, breaks = 100)
hist(dat_80$n_w, breaks = 100)
hist(dat_80$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_80$n) #non-stationary
adf.test(dat_80$n_w) #non-stationary
adf.test(dat_80$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_80$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_80$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_80$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_80$n_ts <- ts(dat_80$n)
dat_80$n_w_ts <- ts(dat_80$n_w)
dat_80$ton_neg_ts <- ts(dat_80$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_80$n_ts_sm <- sma(dat_80$n_ts, order = 4)$fitted
dat_80$n_w_ts_sm <- sma(dat_80$n_w_ts, order = 4)$fitted
dat_80$ton_neg_ts_sm <- sma(dat_80$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_80$week <- as.Date(paste(dat_80$years, dat_80$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_80.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_80$week, dat_80$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="tüv süd")
abline(h = mean(dat_80$n_ts_sm))
abline(h = mean(dat_80$n_ts_sm)+sd(dat_80$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_80$n_ts_sm)-sd(dat_80$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_80.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_80$week, dat_80$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="tüv süd")
abline(h = mean(dat_80$ton_neg_ts_sm))
abline(h = mean(dat_80$ton_neg_ts_sm)+sd(dat_80$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_80$ton_neg_ts_sm)-sd(dat_80$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_80$peak <- 0
dat_80$peak[dat_80$n_ts_sm>mean(dat_80$n_ts_sm)+sd(dat_80$n_ts_sm)] <- 1
dat_80$peak[dat_80$n_ts_sm<mean(dat_80$n_ts_sm)-sd(dat_80$n_ts_sm)] <- -1
#Tonality
dat_80$peak_neg <- 0
dat_80$peak_neg[dat_80$ton_neg_ts_sm>mean(dat_80$ton_neg_ts_sm)+sd(dat_80$ton_neg_ts_sm)] <- 1
dat_80$peak_neg[dat_80$ton_neg_ts_sm<mean(dat_80$ton_neg_ts_sm)-sd(dat_80$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_80$entity_id <- 80

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_80$n <- NULL
dat_80$n_w <- NULL


#underwriters laboratories
#----------
dat_82 <- dat_base[dat_base$entity_id==82,]
require(plyr)
dat_82 <- ddply(dat_82, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_82 <- dat_82[order(dat_82$year, dat_82$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_82 <- dat_82[dat_82$week != 53,]
dat_82 <- dat_82[!is.na(dat_82$week),]

#do we have censored data?
hist(dat_82$n, breaks = 100)
hist(dat_82$n_w, breaks = 100)
hist(dat_82$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_82$n) #non-stationary
adf.test(dat_82$n_w) #non-stationary
adf.test(dat_82$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_82$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_82$n_w) # monthly seasonality (seas = 3; tri-weekly); 
findfrequency(dat_82$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_82$n_ts <- ts(dat_82$n)
dat_82$n_w_ts <- ts(dat_82$n_w)
dat_82$ton_neg_ts <- ts(dat_82$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_82$n_ts_sm <- sma(dat_82$n_ts, order = 4)$fitted
dat_82$n_w_ts_sm <- sma(dat_82$n_w_ts, order = 4)$fitted
dat_82$ton_neg_ts_sm <- sma(dat_82$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_82$week <- as.Date(paste(dat_82$years, dat_82$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_82.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_82$week, dat_82$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="underwriters laboratories")
abline(h = mean(dat_82$n_ts_sm))
abline(h = mean(dat_82$n_ts_sm)+sd(dat_82$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_82$n_ts_sm)-sd(dat_82$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_82.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_82$week, dat_82$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="underwriters laboratories")
abline(h = mean(dat_82$ton_neg_ts_sm))
abline(h = mean(dat_82$ton_neg_ts_sm)+sd(dat_82$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_82$ton_neg_ts_sm)-sd(dat_82$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_82$peak <- 0
dat_82$peak[dat_82$n_ts_sm>mean(dat_82$n_ts_sm)+sd(dat_82$n_ts_sm)] <- 1
dat_82$peak[dat_82$n_ts_sm<mean(dat_82$n_ts_sm)-sd(dat_82$n_ts_sm)] <- -1
#Tonality
dat_82$peak_neg <- 0
dat_82$peak_neg[dat_82$ton_neg_ts_sm>mean(dat_82$ton_neg_ts_sm)+sd(dat_82$ton_neg_ts_sm)] <- 1
dat_82$peak_neg[dat_82$ton_neg_ts_sm<mean(dat_82$ton_neg_ts_sm)-sd(dat_82$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_82$entity_id <- 82

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_82$n <- NULL
dat_82$n_w <- NULL


#west midlands integrated transport authority (wmita)
#----------
dat_87 <- dat_base[dat_base$entity_id==87,]
require(plyr)
dat_87 <- ddply(dat_87, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_87 <- dat_87[order(dat_87$year, dat_87$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_87 <- dat_87[dat_87$week != 53,]
dat_87 <- dat_87[!is.na(dat_87$week),]

#do we have censored data?
hist(dat_87$n, breaks = 100)
hist(dat_87$n_w, breaks = 100)
hist(dat_87$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_87$n)
adf.test(dat_87$n_w)
adf.test(dat_87$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_87$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_87$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_87$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_87$n_ts <- ts(dat_87$n)
dat_87$n_w_ts <- ts(dat_87$n_w)
dat_87$ton_neg_ts <- ts(dat_87$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_87$n_ts_sm <- sma(dat_87$n_ts, order = 4)$fitted
dat_87$n_w_ts_sm <- sma(dat_87$n_w_ts, order = 4)$fitted
dat_87$ton_neg_ts_sm <- sma(dat_87$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_87$week <- as.Date(paste(dat_87$years, dat_87$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_87.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_87$week, dat_87$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="west midlands integrated transport authority (wmita)")
abline(h = mean(dat_87$n_ts_sm))
abline(h = mean(dat_87$n_ts_sm)+sd(dat_87$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_87$n_ts_sm)-sd(dat_87$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_87.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_87$week, dat_87$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="west midlands integrated transport authority (wmita)")
abline(h = mean(dat_87$ton_neg_ts_sm))
abline(h = mean(dat_87$ton_neg_ts_sm)+sd(dat_87$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_87$ton_neg_ts_sm)-sd(dat_87$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_87$peak <- 0
dat_87$peak[dat_87$n_ts_sm>mean(dat_87$n_ts_sm)+sd(dat_87$n_ts_sm)] <- 1
dat_87$peak[dat_87$n_ts_sm<mean(dat_87$n_ts_sm)-sd(dat_87$n_ts_sm)] <- -1
#Tonality
dat_87$peak_neg <- 0
dat_87$peak_neg[dat_87$ton_neg_ts_sm>mean(dat_87$ton_neg_ts_sm)+sd(dat_87$ton_neg_ts_sm)] <- 1
dat_87$peak_neg[dat_87$ton_neg_ts_sm<mean(dat_87$ton_neg_ts_sm)-sd(dat_87$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_87$entity_id <- 87

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_87$n <- NULL
dat_87$n_w <- NULL


#worker rights consortium
#----------
dat_88 <- dat_base[dat_base$entity_id==88,]
require(plyr)
dat_88 <- ddply(dat_88, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_88 <- dat_88[order(dat_88$year, dat_88$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_88 <- dat_88[dat_88$week != 53,]
dat_88 <- dat_88[!is.na(dat_88$week),]

#do we have censored data?
hist(dat_88$n, breaks = 100)
hist(dat_88$n_w, breaks = 100)
hist(dat_88$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_88$n) #non-stationary
adf.test(dat_88$n_w) #non-stationary
adf.test(dat_88$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_88$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_88$n_w) # monthly seasonality (seas = 5; monthly); 
findfrequency(dat_88$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_88$n_ts <- ts(dat_88$n)
dat_88$n_w_ts <- ts(dat_88$n_w)
dat_88$ton_neg_ts <- ts(dat_88$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_88$n_ts_sm <- sma(dat_88$n_ts, order = 4)$fitted
dat_88$n_w_ts_sm <- sma(dat_88$n_w_ts, order = 4)$fitted
dat_88$ton_neg_ts_sm <- sma(dat_88$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_88$week <- as.Date(paste(dat_88$years, dat_88$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_88.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_88$week, dat_88$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="worker rights consortium")
abline(h = mean(dat_88$n_ts_sm))
abline(h = mean(dat_88$n_ts_sm)+sd(dat_88$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_88$n_ts_sm)-sd(dat_88$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_88.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_88$week, dat_88$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="worker rights consortium")
abline(h = mean(dat_88$ton_neg_ts_sm))
abline(h = mean(dat_88$ton_neg_ts_sm)+sd(dat_88$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_88$ton_neg_ts_sm)-sd(dat_88$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_88$peak <- 0
dat_88$peak[dat_88$n_ts_sm>mean(dat_88$n_ts_sm)+sd(dat_88$n_ts_sm)] <- 1
dat_88$peak[dat_88$n_ts_sm<mean(dat_88$n_ts_sm)-sd(dat_88$n_ts_sm)] <- -1
#Tonality
dat_88$peak_neg <- 0
dat_88$peak_neg[dat_88$ton_neg_ts_sm>mean(dat_88$ton_neg_ts_sm)+sd(dat_88$ton_neg_ts_sm)] <- 1
dat_88$peak_neg[dat_88$ton_neg_ts_sm<mean(dat_88$ton_neg_ts_sm)-sd(dat_88$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_88$entity_id <- 88

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_88$n <- NULL
dat_88$n_w <- NULL


#world business council for sustainable development
#----------
dat_89 <- dat_base[dat_base$entity_id==89,]
require(plyr)
dat_89 <- ddply(dat_89, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_89 <- dat_89[order(dat_89$year, dat_89$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_89 <- dat_89[dat_89$week != 53,]
dat_89 <- dat_89[!is.na(dat_89$week),]

#do we have censored data?
hist(dat_89$n, breaks = 100)
hist(dat_89$n_w, breaks = 100)
hist(dat_89$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_89$n)
adf.test(dat_89$n_w)
adf.test(dat_89$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_89$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_89$n_w) # monthly seasonality (seas = 2; bi-weekly); 
findfrequency(dat_89$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_89$n_ts <- ts(dat_89$n)
dat_89$n_w_ts <- ts(dat_89$n_w)
dat_89$ton_neg_ts <- ts(dat_89$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_89$n_ts_sm <- sma(dat_89$n_ts, order = 4)$fitted
dat_89$n_w_ts_sm <- sma(dat_89$n_w_ts, order = 4)$fitted
dat_89$ton_neg_ts_sm <- sma(dat_89$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_89$week <- as.Date(paste(dat_89$years, dat_89$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_89.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_89$week, dat_89$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="world business council for sustainable development")
abline(h = mean(dat_89$n_ts_sm))
abline(h = mean(dat_89$n_ts_sm)+sd(dat_89$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_89$n_ts_sm)-sd(dat_89$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_89.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_89$week, dat_89$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="world business council for sustainable development")
abline(h = mean(dat_89$ton_neg_ts_sm))
abline(h = mean(dat_89$ton_neg_ts_sm)+sd(dat_89$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_89$ton_neg_ts_sm)-sd(dat_89$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_89$peak <- 0
dat_89$peak[dat_89$n_ts_sm>mean(dat_89$n_ts_sm)+sd(dat_89$n_ts_sm)] <- 1
dat_89$peak[dat_89$n_ts_sm<mean(dat_89$n_ts_sm)-sd(dat_89$n_ts_sm)] <- -1
#Tonality
dat_89$peak_neg <- 0
dat_89$peak_neg[dat_89$ton_neg_ts_sm>mean(dat_89$ton_neg_ts_sm)+sd(dat_89$ton_neg_ts_sm)] <- 1
dat_89$peak_neg[dat_89$ton_neg_ts_sm<mean(dat_89$ton_neg_ts_sm)-sd(dat_89$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_89$entity_id <- 89

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_89$n <- NULL
dat_89$n_w <- NULL


#world diamond council
#----------
dat_90 <- dat_base[dat_base$entity_id==90,]
require(plyr)
dat_90 <- ddply(dat_90, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_90 <- dat_90[order(dat_90$year, dat_90$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_90 <- dat_90[dat_90$week != 53,]
dat_90 <- dat_90[!is.na(dat_90$week),]

#do we have censored data?
hist(dat_90$n, breaks = 100)
hist(dat_90$n_w, breaks = 100)
hist(dat_90$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_90$n)
adf.test(dat_90$n_w) #non-stationary
adf.test(dat_90$ton_neg)
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_90$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_90$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_90$ton_neg) # monthly seasonality (seas = 1; +/- 4.5 months); 

#Build a time series
dat_90$n_ts <- ts(dat_90$n)
dat_90$n_w_ts <- ts(dat_90$n_w)
dat_90$ton_neg_ts <- ts(dat_90$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_90$n_ts_sm <- sma(dat_90$n_ts, order = 4)$fitted
dat_90$n_w_ts_sm <- sma(dat_90$n_w_ts, order = 4)$fitted
dat_90$ton_neg_ts_sm <- sma(dat_90$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_90$week <- as.Date(paste(dat_90$years, dat_90$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_90.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_90$week, dat_90$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="world diamond council")
abline(h = mean(dat_90$n_ts_sm))
abline(h = mean(dat_90$n_ts_sm)+sd(dat_90$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_90$n_ts_sm)-sd(dat_90$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_90.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_90$week, dat_90$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="world diamond council")
abline(h = mean(dat_90$ton_neg_ts_sm))
abline(h = mean(dat_90$ton_neg_ts_sm)+sd(dat_90$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_90$ton_neg_ts_sm)-sd(dat_90$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_90$peak <- 0
dat_90$peak[dat_90$n_ts_sm>mean(dat_90$n_ts_sm)+sd(dat_90$n_ts_sm)] <- 1
dat_90$peak[dat_90$n_ts_sm<mean(dat_90$n_ts_sm)-sd(dat_90$n_ts_sm)] <- -1
#Tonality
dat_90$peak_neg <- 0
dat_90$peak_neg[dat_90$ton_neg_ts_sm>mean(dat_90$ton_neg_ts_sm)+sd(dat_90$ton_neg_ts_sm)] <- 1
dat_90$peak_neg[dat_90$ton_neg_ts_sm<mean(dat_90$ton_neg_ts_sm)-sd(dat_90$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_90$entity_id <- 90

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_90$n <- NULL
dat_90$n_w <- NULL


#world federation of diamond bourses
#----------
dat_91 <- dat_base[dat_base$entity_id==91,]
require(plyr)
dat_91 <- ddply(dat_91, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_91 <- dat_91[order(dat_91$year, dat_91$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_91 <- dat_91[dat_91$week != 53,]
dat_91 <- dat_91[!is.na(dat_91$week),]

#do we have censored data?
hist(dat_91$n, breaks = 100)
hist(dat_91$n_w, breaks = 100)
hist(dat_91$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_91$n) #non-stationary
adf.test(dat_91$n_w) #non-stationary
adf.test(dat_91$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_91$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_91$n_w) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_91$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_91$n_ts <- ts(dat_91$n)
dat_91$n_w_ts <- ts(dat_91$n_w)
dat_91$ton_neg_ts <- ts(dat_91$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_91$n_ts_sm <- sma(dat_91$n_ts, order = 4)$fitted
dat_91$n_w_ts_sm <- sma(dat_91$n_w_ts, order = 4)$fitted
dat_91$ton_neg_ts_sm <- sma(dat_91$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_91$week <- as.Date(paste(dat_91$years, dat_91$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_91.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_91$week, dat_91$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="world federation of diamond bourses")
abline(h = mean(dat_91$n_ts_sm))
abline(h = mean(dat_91$n_ts_sm)+sd(dat_91$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_91$n_ts_sm)-sd(dat_91$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_91.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_91$week, dat_91$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="world federation of diamond bourses")
abline(h = mean(dat_91$ton_neg_ts_sm))
abline(h = mean(dat_91$ton_neg_ts_sm)+sd(dat_91$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_91$ton_neg_ts_sm)-sd(dat_91$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_91$peak <- 0
dat_91$peak[dat_91$n_ts_sm>mean(dat_91$n_ts_sm)+sd(dat_91$n_ts_sm)] <- 1
dat_91$peak[dat_91$n_ts_sm<mean(dat_91$n_ts_sm)-sd(dat_91$n_ts_sm)] <- -1
#Tonality
dat_91$peak_neg <- 0
dat_91$peak_neg[dat_91$ton_neg_ts_sm>mean(dat_91$ton_neg_ts_sm)+sd(dat_91$ton_neg_ts_sm)] <- 1
dat_91$peak_neg[dat_91$ton_neg_ts_sm<mean(dat_91$ton_neg_ts_sm)-sd(dat_91$ton_neg_ts_sm)] <- -1

#Entity-Identifier for merging
dat_91$entity_id <- 91

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_91$n <- NULL
dat_91$n_w <- NULL


#world wide web consortium
#----------
dat_92 <- dat_base[dat_base$entity_id==92,]
require(plyr)
dat_92 <- ddply(dat_92, .(weeks, years), function (x) {
  data.frame(n = sum(x$n), n_w = sum(x$n_w), ton_neg=sum(x$tonality_verbalized_negative))})
dat_92 <- dat_92[order(dat_92$year, dat_92$weeks),]

# week 53 is odd as well, so lets get rid of it as well
dat_92 <- dat_92[dat_92$week != 53,]
dat_92 <- dat_92[!is.na(dat_92$week),]

#do we have censored data?
hist(dat_92$n, breaks = 100)
hist(dat_92$n_w, breaks = 100)
hist(dat_92$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
require(tseries)
adf.test(dat_92$n)
adf.test(dat_92$n_w)
adf.test(dat_92$ton_neg) #non-stationary
# p-value is smaller than 0.01 => stationary time series

# check whether and if yes, which seasonality dominates the data
require(forecast)
findfrequency(dat_92$n) # monthly seasonality (seas = 1; weekly); 
findfrequency(dat_92$n_w) # monthly seasonality (seas = 5; monthly); 
findfrequency(dat_92$ton_neg) # monthly seasonality (seas = 1; weekly); 

#Build a time series
dat_92$n_ts <- ts(dat_92$n)
dat_92$n_w_ts <- ts(dat_92$n_w)
dat_92$ton_neg_ts <- ts(dat_92$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
require(smooth)
dat_92$n_ts_sm <- sma(dat_92$n_ts, order = 4)$fitted
dat_92$n_w_ts_sm <- sma(dat_92$n_w_ts, order = 4)$fitted
dat_92$ton_neg_ts_sm <- sma(dat_92$ton_neg_ts, order = 4)$fitted

# create starting date of every week
dat_92$week <- as.Date(paste(dat_92$years, dat_92$weeks, 1, sep="-"), "%Y-%U-%u")

# illustrate peaks and slumps
#Salience
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_92.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_92$week, dat_92$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",
     main="world wide web consortium")
abline(h = mean(dat_92$n_ts_sm))
abline(h = mean(dat_92$n_ts_sm)+sd(dat_92$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_92$n_ts_sm)-sd(dat_92$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./Prototype_UK/Results/Graphs/timeline_ton_92.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat_92$week, dat_92$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",
     main="world wide web consortium")
abline(h = mean(dat_92$ton_neg_ts_sm))
abline(h = mean(dat_92$ton_neg_ts_sm)+sd(dat_92$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat_92$ton_neg_ts_sm)-sd(dat_92$ton_neg_ts_sm), lty = "dashed")
dev.off()

#Variable for peak and low times
#Salience
dat_92$peak <- 0
dat_92$peak[dat_92$n_ts_sm>mean(dat_92$n_ts_sm)+sd(dat_92$n_ts_sm)] <- 1
dat_92$peak[dat_92$n_ts_sm<mean(dat_92$n_ts_sm)-sd(dat_92$n_ts_sm)] <- -1
#Tonality
dat_92$peak_neg <- 0
dat_92$peak_neg[dat_92$ton_neg_ts_sm>mean(dat_92$ton_neg_ts_sm)+sd(dat_92$ton_neg_ts_sm)] <- 1
dat_92$peak_neg[dat_92$ton_neg_ts_sm<mean(dat_92$ton_neg_ts_sm)-sd(dat_92$ton_neg_ts_sm)] <- -1


#Entity-Identifier for merging
dat_92$entity_id <- 92

#Remove these variables: same names in dat_base (initial corpus); later we will merge them
dat_92$n <- NULL
dat_92$n_w <- NULL


}

#Bind all the individual governors datasets together (i.e. staple them)

#Since rbind cannot handle time-series operators, we have to transform the datasets

  #create a list of datasets over which we can loop ('' is used to be able to assign list to them later)
dat_df=list('dat_1','dat_2','dat_3','dat_4','dat_5','dat_6','dat_7','dat_8','dat_9','dat_10',
            'dat_11','dat_12','dat_13','dat_14','dat_15','dat_16','dat_17','dat_18','dat_19','dat_20',
            'dat_21','dat_22','dat_23','dat_24','dat_25','dat_26','dat_28','dat_30',
            'dat_31','dat_32','dat_34','dat_35','dat_36','dat_37','dat_38','dat_39','dat_40',
            'dat_41','dat_43','dat_44','dat_45','dat_46','dat_48','dat_49','dat_50',
            'dat_51','dat_52','dat_53','dat_55','dat_56','dat_57','dat_58','dat_60',
            'dat_61','dat_62','dat_63','dat_64','dat_66','dat_67','dat_68','dat_69','dat_70',
            'dat_73','dat_74','dat_77','dat_78','dat_79','dat_80',
            'dat_82','dat_87','dat_88','dat_89','dat_90',
            'dat_91','dat_92')

  #apply the same function to all 11 dfs, create a list
d_list <- lapply(dat_df, function(df) {
  df <- get(df)
  df$week <- NULL
  df$n_ts <- as.numeric(df$n_ts)
  df$n_w_ts <- as.numeric(df$n_w_ts)
  df$ton_neg_ts <- as.numeric(df$ton_neg_ts)
  df$n_ts_sm <- as.numeric(df$n_ts_sm)
  df$n_w_ts_sm <- as.numeric(df$n_w_ts_sm)
  df$ton_neg_ts_sm <- as.numeric(df$ton_neg_ts_sm)
  return(df)})

  #use the list just created to assign the transformations to the 11 datasets
for (i in 1:length(dat_df)) {
  assign(dat_df[[i]], d_list[[i]])
}

#1 dataset for all individual governor datasets
dat_org <- rbind(dat_1,dat_2,dat_3,dat_4,dat_5,dat_6,dat_7,dat_8,dat_9,dat_10,
                 dat_11,dat_12,dat_13,dat_14,dat_15,dat_16,dat_17,dat_18,dat_19,dat_20,
                 dat_21,dat_22,dat_23,dat_24,dat_25,dat_26,dat_28,dat_30,
                 dat_31,dat_32,dat_34,dat_35,dat_36,dat_37,dat_38,dat_39,dat_40,
                 dat_41,dat_43,dat_44,dat_45,dat_46,dat_48,dat_49,dat_50,
                 dat_51,dat_52,dat_53,dat_55,dat_56,dat_57,dat_58,dat_60,
                 dat_61,dat_62,dat_63,dat_64,dat_66,dat_67,dat_68,dat_69,dat_70,
                 dat_73,dat_74,dat_77,dat_78,dat_79,dat_80,
                 dat_82,dat_87,dat_88,dat_89,dat_90,
                 dat_91,dat_92)

#-> merge with the full dataset: Assign for each article whether it was published in peak/low time or not
#Merge (many-to-one) Organization dataset (dat_org) with full Corpus (dat_base)
dat_peak <- join(dat_base, dat_org, by = c("weeks","years","entity_id"), 
                    type="left",match="all")


#Save that file
save(dat_peak, file=paste("./Prototype_UK/UKCorpus_PeakLow.Rda"))

#Remove all the individual governor files
rm(dat_1,dat_2,dat_3,dat_4,dat_5,dat_6,dat_7,dat_8,dat_9,dat_10,
        dat_11,dat_12,dat_13,dat_14,dat_15,dat_16,dat_17,dat_18,dat_19,dat_20,
        dat_21,dat_22,dat_23,dat_24,dat_25,dat_26,dat_28,dat_30,
        dat_31,dat_32,dat_34,dat_35,dat_36,dat_37,dat_38,dat_39,dat_40,
        dat_41,dat_43,dat_44,dat_45,dat_46,dat_48,dat_49,dat_50,
        dat_51,dat_52,dat_53,dat_55,dat_56,dat_57,dat_58,dat_60,
        dat_61,dat_62,dat_63,dat_64,dat_66,dat_67,dat_68,dat_69,dat_70,
        dat_73,dat_74,dat_77,dat_78,dat_79,dat_80,
        dat_82,dat_87,dat_88,dat_89,dat_90,
        dat_91,dat_92,input)

# BIVARIATE ANALYSIS #
#---------------------

#Are certain governor characteristics/media types clearly under-/overrepresented in peak/non-peak times (salience & tonality)?

#SALIENCE
{
#Actor Type
  #Absolute values
unique(dat_peak$actor.type)
medtyp_peak <- table(dat_peak$peak,dat_peak$actor.type)
png(filename=paste("./Prototype_UK/Results/Graphs/ActorType_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Actor Type \n and Salience", 
        xlab="Actor Type",ylab="Number of Articles", beside=T,col=c("gray8","gray48","gray88"),
        names.arg=c("hybrid","private","public (elected)","public \n (non-elected)")) 
legend("topleft",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
dev.off()

  #Article Shares by Actor type
require(reshape2)
dat_peak2 <- dat_peak[!is.na(dat_peak$peak),]
dat_peak2 <- dat_peak[!is.na(dat_peak$actor.type),]
unique(dat_peak2$peak)

dat_peak2$peaksal <- dat_peak2$peak
medtyp_share <- dcast(dat_peak2, actor.type ~ peaksal)
medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
medtyp_share$sum <- NULL
medtyp_share$`-1` <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
    #Share Peak
png(filename=paste("./Prototype_UK/Results/Graphs/ActorType_SharePeak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercPeak,main="% Articles Appearing in Peak Times \n (by Actor Type)", xlab="Actor Type",
        names.arg=c("hybrid","private","public (elected)","public (non-elected)"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()
    #Share Low
png(filename=paste("./Prototype_UK/Results/Graphs/ActorType_ShareLow.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercLow,main="% Articles Appearing in Low Times \n (by Actor Type)", xlab="Actor Type",
        names.arg=c("hybrid","private","public (elected)","public (non-elected)"),
        ylim=c(0,100),ylab="% of Articles in Low Times")
dev.off()

#Policy Scope
  #Absolute Values
unique(dat_peak$policy.scope)
medtyp_peak <- table(dat_peak$peak,dat_peak$policy.scope)
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyScope_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Policy Scope \n and Salience", 
        xlab="Policy Scope",ylab="Number of Articles", beside=T,col=c("gray8","gray48","gray88"),
        names.arg=c("narrow","wide")) 
legend("topright",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
dev.off()

  #Article Shares by Policy Scope
require(reshape2)
dat_peak2 <- dat_peak[!is.na(dat_peak$peak),]
dat_peak2 <- dat_peak[!is.na(dat_peak$policy.scope),]
dat_peak2$peaksal <- dat_peak2$peak
medtyp_share <- dcast(dat_peak2, policy.scope ~ peaksal)
medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
medtyp_share$sum <- NULL
medtyp_share$`-1` <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
#Share Peak
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyScope_SharePeak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercPeak,main="% Articles Appearing in Peak Times \n (by Policy Scope)", xlab="Policy Scope",
        names.arg=c("narrow","wide"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()
#Share Low
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyScope_ShareLow.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercLow,main="% Articles Appearing in Low Times \n (by Policy Scope)", xlab="Policy Scope",
        names.arg=c("narrow","wide"),
        ylim=c(0,100),ylab="% of Articles in Low Times")
dev.off()

#Functional Scope
  #Absolute Values
unique(dat_peak$functional.scope..old.)
medtyp_peak <- table(dat_peak$peak,dat_peak$functional.scope..old.)
png(filename=paste("./Prototype_UK/Results/Graphs/FunctionalScope_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Functional Scope \n and Salience", 
        xlab="Functional Scope",ylab="Number of Articles", beside=T,col=c("gray8","gray48","gray88"),
        names.arg=c("exchange & \n agenda-setting","implementation","decision-making")) 
legend("topright",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
dev.off()

  #Article Shares
require(reshape2)
dat_peak2 <- dat_peak[!is.na(dat_peak$peak),]
dat_peak2 <- dat_peak[!is.na(dat_peak$functional.scope..old.),]
unique(dat_peak2$peak)

dat_peak2$peaksal <- dat_peak2$peak
medtyp_share <- dcast(dat_peak2, functional.scope..old. ~ peaksal)
medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
medtyp_share$sum <- NULL
medtyp_share$`-1` <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Prototype_UK/Results/Graphs/FunctionalScope_SharePeak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Peak Times \n (by Functional Scope)", xlab="Functional Scope",
        names.arg=c("exchange & \n agenda-setting","implementation","decision-making"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()
png(filename=paste("./Prototype_UK/Results/Graphs/FunctionalScope_ShareLow.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Low Times \n (by Functional Scope)", xlab="Functional Scope",
        names.arg=c("exchange & \n agenda-setting","implementation","decision-making"),
        ylim=c(0,100),ylab="% of Articles in Low Times")
dev.off()

#Territorial Scope
#Absolute Values
unique(dat_peak$territorial.scope)
medtyp_peak <- table(dat_peak$peak,dat_peak$territorial.scope)
png(filename=paste("./Prototype_UK/Results/Graphs/TerritorialScope_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Territorial Scope \n and Salience", 
        xlab="Territorial Scope",ylab="Number of Articles", beside=T,col=c("gray8","gray48","gray88"),
        names.arg=c("global","regional","subnational")) 
legend("topright",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
dev.off()

#Article Shares
require(reshape2)
dat_peak2 <- dat_peak[!is.na(dat_peak$peak),]
dat_peak2 <- dat_peak[!is.na(dat_peak$territorial.scope),]
unique(dat_peak2$peak)

dat_peak2$peaksal <- dat_peak2$peak
medtyp_share <- dcast(dat_peak2, territorial.scope ~ peaksal)
medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
medtyp_share$sum <- NULL
medtyp_share$`-1` <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Prototype_UK/Results/Graphs/TerritorialScope_SharePeak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Peak Times \n (by Territorial Scope)", xlab="Territorial Scope",
        names.arg=c("global","regional","subnational"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()
png(filename=paste("./Prototype_UK/Results/Graphs/TerritorialScope_ShareLow.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Low Times \n (by Territorial Scope)", xlab="Territorial Scope",
        names.arg=c("global","regional","subnational"),
        ylim=c(0,100),ylab="% of Articles in Low Times")
dev.off()

#Policy Output
#Absolute Values
unique(dat_peak$policy.output)
medtyp_peak <- table(dat_peak$peak,dat_peak$policy.output)
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyOutput_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Policy Output \n and Salience", 
        xlab="Policy Output",ylab="Number of Articles", beside=T,col=c("gray8","gray48","gray88"),
        names.arg=c("hard","hard/soft","soft")) 
legend("topleft",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
dev.off()

#Article Shares
require(reshape2)
dat_peak2 <- dat_peak[!is.na(dat_peak$peak),]
dat_peak2 <- dat_peak[!is.na(dat_peak$policy.output),]
unique(dat_peak2$peak)

dat_peak2$peaksal <- dat_peak2$peak
medtyp_share <- dcast(dat_peak2, policy.output ~ peaksal)
medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
medtyp_share$sum <- NULL
medtyp_share$`-1` <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyOutput_SharePeak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Peak Times \n (by Policy Output)", xlab="Policy Output",
        names.arg=c("hard","hard/soft","soft"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()
png(filename=paste("./Prototype_UK/Results/Graphs/PolicyOutput_ShareLow.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Low Times \n (by Policy Output)", xlab="Policy Output",
        names.arg=c("hard","hard/soft","soft"),
        ylim=c(0,100),ylab="% of Articles in Low Times")
dev.off()


#Media Type
#Absolute Values
unique(dat_peak$media_type)
medtyp_peak <- table(dat_peak$peak,dat_peak$media_type)
png(filename=paste("./Prototype_UK/Results/Graphs/MediaType_Peak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_peak,main="Number of Articles by Media Type \n and Salience", 
        xlab="Media Type",ylab="Number of Articles", beside=T,col=c("gray8","gray48","gray88"),
        names.arg=c("magazines","newswire","quality","regional","tabloid/free")) 
legend("topleft",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
dev.off()

#Article Shares
require(reshape2)
dat_peak2 <- dat_peak[!is.na(dat_peak$peak),]
dat_peak2 <- dat_peak[!is.na(dat_peak$media_type),]
unique(dat_peak2$peak)

dat_peak2$peaksal <- dat_peak2$peak
medtyp_share <- dcast(dat_peak2, media_type ~ peaksal)
medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
medtyp_share$sum <- NULL
medtyp_share$`-1` <- NULL
medtyp_share$`0` <- NULL
medtyp_share$`1` <- NULL
medtyp_share$`NA` <- NULL
png(filename=paste("./Prototype_UK/Results/Graphs/MediaType_SharePeak.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Peak Times \n (by Media Type)", xlab="Media Type",
        names.arg=c("magazines","newswire","quality","regional","tabloid/free"),
        ylim=c(0,100),ylab="% of Articles in Peak Times")
dev.off()
png(filename=paste("./Prototype_UK/Results/Graphs/MediaType_ShareLow.png", sep=""), width=9, height=6, units="in", res=600)
barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Low Times \n (by Media Type)", xlab="Media Type",
        names.arg=c("magazines","newswire","quality","regional","tabloid/free"),
        ylim=c(0,100),ylab="% of Articles in Low Times")
dev.off()
}


#NEGATIVE TONALITY
{
  #Actor Type
  #Absolute values
  unique(dat_peak$actor.type)
  medtyp_peak <- table(dat_peak$peak_neg,dat_peak$actor.type)
  png(filename=paste("./Prototype_UK/Results/Graphs/ActorType_PeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_peak,main="Number of Articles by Actor Type \n and Negative Tonality", 
          xlab="Actor Type",ylab="Number of Negative Articles", beside=T,col=c("gray8","gray48","gray88"),
          names.arg=c("hybrid","private","public (elected)","public \n (non-elected)")) 
  legend("topleft",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
  dev.off()
  
  #Article Shares by Actor type
  require(reshape2)
  dat_peak2 <- dat_peak[!is.na(dat_peak$peak_neg),]
  dat_peak2 <- dat_peak[!is.na(dat_peak$actor.type),]
  unique(dat_peak2$peak_neg)
  
  medtyp_share <- dcast(dat_peak2, actor.type ~ peak_neg)
  medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
  medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
  medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
  medtyp_share$sum <- NULL
  medtyp_share$`-1` <- NULL
  medtyp_share$`0` <- NULL
  medtyp_share$`1` <- NULL
  medtyp_share$`NA` <- NULL
  #Share Peak
  png(filename=paste("./Prototype_UK/Results/Graphs/ActorType_SharePeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercPeak,main="% Articles Appearing in Negative Peak Times \n (by Actor Type)", xlab="Actor Type",
          names.arg=c("hybrid","private","public (elected)","public (non-elected)"),
          ylim=c(0,100),ylab="% of Articles in Negative Peak Times")
  dev.off()
  #Share Low
  png(filename=paste("./Prototype_UK/Results/Graphs/ActorType_ShareLowTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercLow,main="% Articles Appearing in Negative Low Times \n (by Actor Type)", xlab="Actor Type",
          names.arg=c("hybrid","private","public (elected)","public (non-elected)"),
          ylim=c(0,100),ylab="% of Articles in Negative Low Times")
  dev.off()
  
  #Policy Scope
  #Absolute Values
  unique(dat_peak$policy.scope)
  medtyp_peak <- table(dat_peak$peak_neg,dat_peak$policy.scope)
  png(filename=paste("./Prototype_UK/Results/Graphs/PolicyScope_PeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_peak,main="Number of Negative Articles by Policy Scope \n and Negative Tonality", 
          xlab="Policy Scope",ylab="Number of Negative Articles", beside=T,col=c("gray8","gray48","gray88"),
          names.arg=c("narrow","wide")) 
  legend("topright",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
  dev.off()
  
  #Article Shares by Policy Scope
  require(reshape2)
  dat_peak2 <- dat_peak[!is.na(dat_peak$peak_neg),]
  dat_peak2 <- dat_peak[!is.na(dat_peak$policy.scope),]
  medtyp_share <- dcast(dat_peak2, policy.scope ~ peak_neg)
  medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
  medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
  medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
  medtyp_share$sum <- NULL
  medtyp_share$`-1` <- NULL
  medtyp_share$`0` <- NULL
  medtyp_share$`1` <- NULL
  medtyp_share$`NA` <- NULL
  #Share Peak
  png(filename=paste("./Prototype_UK/Results/Graphs/PolicyScope_SharePeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercPeak,main="% Articles Appearing in Negative Peak Times \n (by Policy Scope)", xlab="Policy Scope",
          names.arg=c("narrow","wide"),
          ylim=c(0,100),ylab="% of Articles in Negative Peak Times")
  dev.off()
  #Share Low
  png(filename=paste("./Prototype_UK/Results/Graphs/PolicyScope_ShareLowTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercLow,main="% Articles Appearing in Negative Low Times \n (by Policy Scope)", xlab="Policy Scope",
          names.arg=c("narrow","wide"),
          ylim=c(0,100),ylab="% of Articles in Negative Low Times")
  dev.off()
  
  #Functional Scope
  #Absolute Values
  unique(dat_peak$functional.scope..old.)
  medtyp_peak <- table(dat_peak$peak_neg,dat_peak$functional.scope..old.)
  png(filename=paste("./Prototype_UK/Results/Graphs/FunctionalScope_PeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_peak,main="Number of Negative Articles by Functional Scope \n and Negative Tonality", 
          xlab="Functional Scope",ylab="Number of Negative Articles", beside=T,col=c("gray8","gray48","gray88"),
          names.arg=c("exchange & \n agenda-setting","implementation","decision-making")) 
  legend("topright",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
  dev.off()
  
  #Article Shares
  require(reshape2)
  dat_peak2 <- dat_peak[!is.na(dat_peak$peak_neg),]
  dat_peak2 <- dat_peak[!is.na(dat_peak$functional.scope..old.),]
  unique(dat_peak2$peak_neg)
  
  medtyp_share <- dcast(dat_peak2, functional.scope..old. ~ peak_neg)
  medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
  medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
  medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
  medtyp_share$sum <- NULL
  medtyp_share$`-1` <- NULL
  medtyp_share$`0` <- NULL
  medtyp_share$`1` <- NULL
  medtyp_share$`NA` <- NULL
  png(filename=paste("./Prototype_UK/Results/Graphs/FunctionalScope_SharePeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Negative Peak Times \n (by Functional Scope)", xlab="Functional Scope",
          names.arg=c("exchange & \n agenda-setting","implementation","decision-making"),
          ylim=c(0,100),ylab="% of Articles in Negative Peak Times")
  dev.off()
  png(filename=paste("./Prototype_UK/Results/Graphs/FunctionalScope_ShareLowTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Negative Low Times \n (by Functional Scope)", xlab="Functional Scope",
          names.arg=c("exchange & \n agenda-setting","implementation","decision-making"),
          ylim=c(0,100),ylab="% of Articles in Negative Low Times")
  dev.off()
  
  #Territorial Scope
  #Absolute Values
  unique(dat_peak$territorial.scope)
  medtyp_peak <- table(dat_peak$peak_neg,dat_peak$territorial.scope)
  png(filename=paste("./Prototype_UK/Results/Graphs/TerritorialScope_PeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_peak,main="Number of Negative Articles by Territorial Scope \n and Negative Tonality", 
          xlab="Territorial Scope",ylab="Number of Negative Articles", beside=T,col=c("gray8","gray48","gray88"),
          names.arg=c("global","regional","subnational")) 
  legend("topright",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
  dev.off()
  
  #Article Shares
  require(reshape2)
  dat_peak2 <- dat_peak[!is.na(dat_peak$peak_neg),]
  dat_peak2 <- dat_peak[!is.na(dat_peak$territorial.scope),]
  unique(dat_peak2$peak_neg)
  
  medtyp_share <- dcast(dat_peak2, territorial.scope ~ peak_neg)
  medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
  medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
  medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
  medtyp_share$sum <- NULL
  medtyp_share$`-1` <- NULL
  medtyp_share$`0` <- NULL
  medtyp_share$`1` <- NULL
  medtyp_share$`NA` <- NULL
  png(filename=paste("./Prototype_UK/Results/Graphs/TerritorialScope_SharePeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Negative Peak Times \n (by Territorial Scope)", xlab="Territorial Scope",
          names.arg=c("global","regional","subnational"),
          ylim=c(0,100),ylab="% of Articles in Negative Peak Times")
  dev.off()
  png(filename=paste("./Prototype_UK/Results/Graphs/TerritorialScope_ShareLowTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Negative Low Times \n (by Territorial Scope)", xlab="Territorial Scope",
          names.arg=c("global","regional","subnational"),
          ylim=c(0,100),ylab="% of Articles in Negative Low Times")
  dev.off()
  
  #Policy Output
  #Absolute Values
  unique(dat_peak$policy.output)
  medtyp_peak <- table(dat_peak$peak_neg,dat_peak$policy.output)
  png(filename=paste("./Prototype_UK/Results/Graphs/PolicyOutput_PeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_peak,main="Number of Negative Articles by Policy Output \n and Negative Tonality", 
          xlab="Policy Output",ylab="Number of Negative Articles", beside=T,col=c("gray8","gray48","gray88"),
          names.arg=c("hard","hard/soft","soft")) 
  legend("topleft",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
  dev.off()
  
  #Article Shares
  require(reshape2)
  dat_peak2 <- dat_peak[!is.na(dat_peak$peak_neg),]
  dat_peak2 <- dat_peak[!is.na(dat_peak$policy.output),]
  unique(dat_peak2$peak_neg)
  
  medtyp_share <- dcast(dat_peak2, policy.output ~ peak_neg)
  medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
  medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
  medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
  medtyp_share$sum <- NULL
  medtyp_share$`-1` <- NULL
  medtyp_share$`0` <- NULL
  medtyp_share$`1` <- NULL
  medtyp_share$`NA` <- NULL
  png(filename=paste("./Prototype_UK/Results/Graphs/PolicyOutput_SharePeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Negative Peak Times \n (by Policy Output)", xlab="Policy Output",
          names.arg=c("hard","hard/soft","soft"),
          ylim=c(0,100),ylab="% of Articles in Negative Peak Times")
  dev.off()
  png(filename=paste("./Prototype_UK/Results/Graphs/PolicyOutput_ShareLowTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Negative Low Times \n (by Policy Output)", xlab="Policy Output",
          names.arg=c("hard","hard/soft","soft"),
          ylim=c(0,100),ylab="% of Articles in Negative Low Times")
  dev.off()
  
  
  #Media Type
  #Absolute Values
  unique(dat_peak$media_type)
  medtyp_peak <- table(dat_peak$peak_neg,dat_peak$media_type)
  png(filename=paste("./Prototype_UK/Results/Graphs/MediaType_PeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_peak,main="Number of Negative Articles by Media Type \n and Negative Tonality", 
          xlab="Media Type",ylab="Number of Negative Articles", beside=T,col=c("gray8","gray48","gray88"),
          names.arg=c("magazines","newswire","quality","regional","tabloid/free")) 
  legend("topleft",c("Low Time","Normal Time","Peak Time"),fill=c("gray8","gray48","gray88"),bty="n") #()
  dev.off()
  
  #Article Shares
  require(reshape2)
  dat_peak2 <- dat_peak[!is.na(dat_peak$peak_neg),]
  dat_peak2 <- dat_peak[!is.na(dat_peak$media_type),]
  unique(dat_peak2$peak_neg)
  
  medtyp_share <- dcast(dat_peak2, media_type ~ peak_neg)
  medtyp_share$sum <- medtyp_share[,2]+medtyp_share[,3]+medtyp_share[,4]+medtyp_share[,5]
  medtyp_share$PercLow <- (medtyp_share[,2]/medtyp_share[,6])*100
  medtyp_share$PercPeak <- (medtyp_share[,4]/medtyp_share[,6])*100
  medtyp_share$sum <- NULL
  medtyp_share$`-1` <- NULL
  medtyp_share$`0` <- NULL
  medtyp_share$`1` <- NULL
  medtyp_share$`NA` <- NULL
  png(filename=paste("./Prototype_UK/Results/Graphs/MediaType_SharePeakTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercPeak,main="Share of Articles Appearing in Negative Peak Times \n (by Media Type)", xlab="Media Type",
          names.arg=c("magazines","newswire","quality","regional","tabloid/free"),
          ylim=c(0,100),ylab="% of Articles in Negative Peak Times")
  dev.off()
  png(filename=paste("./Prototype_UK/Results/Graphs/MediaType_ShareLowTon.png", sep=""), width=9, height=6, units="in", res=600)
  barplot(medtyp_share$PercLow,main="Share of Articles Appearing in Negative Low Times \n (by Media Type)", xlab="Media Type",
          names.arg=c("magazines","newswire","quality","regional","tabloid/free"),
          ylim=c(0,100),ylab="% of Articles in Negative Low Times")
  dev.off()
}


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
