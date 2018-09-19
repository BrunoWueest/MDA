##### Media-driven accountability
# UK-PROTOTYPE
# bw and ms

# Do not clear your workspace, otherwise the working directory changes.
# Restart R if you have to free memory or detach packages, objects or commands
# rm(list = ls(envir = globalenv()), envir = globalenv())

# do not set the working directory every time but use pathes relative to your git clone--which
# should be located in the Dropbox of this project (the one called 'chapters')
setwd("~/Dropbox/WorkInProgress/NCCRBook/chapters/MDA")
getwd()

### PREPARATION ###
#------------------

#load corpus
input <- readRDS("../data/MDA_corpus-100418.RData") # note the relative path. This should now work for everyone.
unique(input$Media_Language) #what values does language have? see also the file "../data/overview-090418.xlsx' for this.
unique(input$Media_Country) #what values does country have?


corpus <- input[input$Media_Language=="EN",] #take only English articles
unique(corpus$Entity_Name) #what entities are present in the English sample?
corpus <- corpus[!is.na(corpus$Entity_Name),] #remove articles that are "NA" for entity name (~6)
unique(corpus$Tonality_Verbalized)
corpus <- corpus[!is.na(corpus$Tonality_Verbalized),] #remove articles that are "NA" for Tonality (~3), otherwise the following 
rm(input) # remove unused objects to free memory 

# clean variable names
library(dplyr)
colnames(corpus) <- tolower(colnames(corpus))
colnames(corpus) <- gsub("\\.+", "_", colnames(corpus))
colnames(corpus) <- gsub("_new_", "_", colnames(corpus))

# set dummy variables
corpus <- corpus %>%
          mutate_at(vars(functional_scope_informative,
                 functional_scope_implementing,
                 functional_scope_decisive),
            funs(ifelse(. == "yes", 1, 0))
          )


####Preparatory steps

# aggregate number of articles per week (or day, or month? and weight by number of words? if yes, how?)
min(corpus$article_date)
max(corpus$article_date)

library(lubridate)
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
dat_base$tonality_verbalized_negative <- ifelse(dat_base$tonality_verbalized == "negative", 1, 0)
dat_base$tonality_verbalized_negative[is.na(dat_base$tonality_verbalized_negative)] <- 0

### DESCRIPTIVE STATISTICS ###
#-----------------------------

# make plot directory if not exists
dir.create("./plots", showWarnings = FALSE)

#Number of Articles by Governor Characteristics
table(dat_base$entity_name)
png(filename=paste("./plots/ActorType.png", sep=""), width=9, height=6, units="in", res=600)
barplot(table(dat_base$actor_type), main="Actor Type", ylab="Number of Articles")
dev.off()

png(filename=paste("./plots/TerritorialScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(table(dat_base$territorial_scope), main="Territorial Scope", ylab="Number of Articles")
dev.off()

png(filename=paste("./plots/PolicyScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(table(dat_base$policy_scope), main="Policy Scope", ylab="Number of Articles")
dev.off()

png(filename=paste("./plots/FunctScope.png", sep=""), width=9, height=6, units="in", res=600)
barplot(table(dat_base$functional_scope_old_), main="Functional Scope", ylab="Number of Articles")
dev.off()

png(filename=paste("./plots/PolicyOutput.png", sep=""), width=9, height=6, units="in", res=600)
barplot(table(dat_base$policy_output), main="Policy Output", ylab="Number of Articles")
dev.off()

#Number of Articles by Media Characteristics
png(filename=paste("./plotsMediaType.png", sep=""), width=9, height=6, units="in", res=600)
barplot(table(dat_base$media_type), main="Media Type", ylab="Number of Articles")
dev.off()


# Coverage (Salience and Tonality over all entities)
#---------

# get n of articles per week and n of articles weighted by article length (aggregated per week)
dat <- dat_base %>%
        group_by(weeks, years) %>% 
        summarise_at(vars(n, n_w, tonality_verbalized_negative), funs(sum(.)))
dat <- dat[order(dat$years, dat$weeks),]
colnames(dat)[5] <- "ton_neg"

# week 53 is odd, so lets get rid of it
dat <- dat[dat$weeks != 53,]

# add zeros for weeks with no article
weekz <- unique(days[,c("weeks", "years")])
weekz <- weekz[weekz$weeks != 53,]
dat <- merge(dat, weekz, by = c("weeks", "years"), all = T)
dat$n[is.na(dat$n)] <- 0
dat$n_w[is.na(dat$n_w)] <- 0
dat$ton_neg[is.na(dat$ton_neg)] <- 0

dat <- dat[order(dat$years, dat$weeks),]

# do we have strange pattern in the count data that may hint to censored data?
hist(dat$n, breaks = 100)
hist(dat$n_w, breaks = 100)
hist(dat$ton_neg, breaks=100)

# QUESTION: How does that check for censored data?
# > this is not a bullet-proof test, but if you discover unusual
# breaks in the distribution, you might have found evidence for irregularities
# of how the data were produced

# check for unit roots (stationarity) with Augmented Dickey-Fowler test
library(tseries)
adf.test(dat$n)
adf.test(dat$n_w)
adf.test(dat$ton_neg)
# p-value is higher than 0.01, so we have a non-stationary time series

# check whether and if yes, which seasonality dominates the data
library(forecast)
findfrequency(dat$n) # weekly seasonality (seas = 1)
findfrequency(dat$n_w) # weekly seasonality (seas = 1)
findfrequency(dat$ton_neg) # 1.5 monthly seasonality (seas = 6) 


#Build a time series
dat$n_ts <- ts(dat$n)
dat$n_w_ts <- ts(dat$n_w)
dat$ton_neg_ts <- ts(dat$ton_neg)

#smooth with monthly moving average (4 is rather arbitrary, but automated
# detection yields 59 which clearly is nonsense!) to see trends better
library(smooth)
dat$n_ts_sm <- sma(dat$n_ts, order = 4)$fitted
dat$n_w_ts_sm <- sma(dat$n_w_ts, order = 4)$fitted
dat$ton_neg_ts_sm <- sma(dat$ton_neg_ts, order = 4)$fitted


# plot the timelines (weighted and unweighted timelines look very similar; weighted peaks are somwhat less pronounced)
dat$week <- as.Date(paste(dat$years, dat$weeks, 1, sep="-"), "%Y-%U-%u")

png(filename=paste("./plots/NArticles.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$n_ts_sm, type = "l", main="Weekly aggregated, monthly smoothed number of articles", ylab="Number of Articles")
dev.off()

png(filename=paste("./plots/NArticles_logged.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$n_w_ts_sm, type = "l", main="Weekly aggregated, monthly smoothed logged number of articles", ylab="Number of Articles")
dev.off()

png(filename=paste("./plots/NArticles_neg.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$ton_neg_ts_sm, type = "l", main="Weekly aggregated, monthly smoothed number of negative articles", ylab="Number of Articles")
dev.off()

# illustrate peaks and slumps
#Salience
png(filename=paste("./plots/timeline.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$n_ts_sm, type = "l",xlab="Weeks",ylab="Number of Articles",main="Governors in English Coverage")
abline(h = mean(dat$n_ts_sm))
abline(h = mean(dat$n_ts_sm)+sd(dat$n_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat$n_ts_sm)-sd(dat$n_ts_sm), lty = "dashed")
dev.off()

#Tonality
png(filename=paste("./plots/timeline_ton.png", sep=""), width=9, height=6, units="in", res=600)
plot(dat$week, dat$ton_neg_ts_sm, type = "l",xlab="Weeks",ylab="Number of Negative Articles",main="Governors in English Coverage")
abline(h = mean(dat$ton_neg_ts_sm))
abline(h = mean(dat$ton_neg_ts_sm)+sd(dat$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
abline(h = mean(dat$ton_neg_ts_sm)-sd(dat$ton_neg_ts_sm), lty = "dashed")
dev.off()

dat_all <- dat

rm(corpus)

### SALIENCE AND TONALITY GRAPHS BY INDIVIDUAL GOVERNOR ###
#----------

governors <- unique(dat_base[, c("entity_id", "entity_name")])
governors <- governors[!is.na(governors$entity_id),]

# initialize a list as a container for the results
governor_list <- list()

for (i in 1:length(governors[,1])) {
  
  print(i)
  #print(governors[i,1])
  print(governors[i,2])
  dat <- dat_base[dat_base$entity_id==i,]
  
  # get n of articles per week and n of articles weighted by article length (aggregated per week)
  dat <- dat %>%
    group_by(weeks, years) %>% 
    summarise_at(vars(n, n_w, tonality_verbalized_negative), funs(sum(.)))
  
  dat <- dat[order(dat$years, dat$weeks),]
  colnames(dat)[5] <- "ton_neg"
  
  # week 53 is odd as well, so lets get rid of it as well
  dat <- dat[dat$weeks != 53,]
  dat <- dat[!is.na(dat$weeks),]
  dat <- merge(dat, weekz, by = c("weeks", "years"), all = T)
  dat$n[is.na(dat$n)] <- 0
  dat$n_w[is.na(dat$n_w)] <- 0
  dat$ton_neg[is.na(dat$ton_neg)] <- 0
  
  dat <- dat[order(dat$years, dat$weeks),]
  
  #Build a time series
  dat$n_ts <- ts(dat$n)
  dat$n_w_ts <- ts(dat$n_w)
  dat$ton_neg_ts <- ts(dat$ton_neg)
  
  #smooth with monthly moving average (4 is rather arbitrary, but automated
  # detection yields 59 which clearly is nonsense!) to see trends better
  dat$n_ts_sm <- sma(dat$n_ts, order = 4)$fitted
  dat$n_w_ts_sm <- sma(dat$n_w_ts, order = 4)$fitted
  dat$ton_neg_ts_sm <- sma(dat$ton_neg_ts, order = 4)$fitted
  
  # create starting date of every week
  dat$week <- as.Date(paste(dat$years, dat$weeks, 1, sep="-"), "%Y-%U-%u")
  
  # illustrate peaks and slumps
  #Salience
  png(filename=paste0("./plots/timeline_", i, ".png"), width=9, height=6, units="in", res=600)
  plot(dat$week, dat$n_ts_sm, type = "l", xlab = "Weeks", ylab= "Number of Articles", main= governors[i,2])
  abline(h = mean(dat$n_ts_sm))
  abline(h = mean(dat$n_ts_sm)+sd(dat$n_ts_sm), lty = "dashed") #Standard Deviation lines
  abline(h = mean(dat$n_ts_sm)-sd(dat$n_ts_sm), lty = "dashed")
  dev.off()
  
  #Tonality
  png(filename=paste0("./plots/timeline_ton_", i, ".png"), width=9, height=6, units="in", res=600)
  plot(dat$week, dat$ton_neg_ts_sm, type = "l", xlab="Weeks", ylab="Number of Negative Articles",
       main = governors[i,2])
  abline(h = mean(dat$ton_neg_ts_sm))
  abline(h = mean(dat$ton_neg_ts_sm)+sd(dat$ton_neg_ts_sm), lty = "dashed") #Standard Deviation lines
  abline(h = mean(dat$ton_neg_ts_sm)-sd(dat$ton_neg_ts_sm), lty = "dashed")
  dev.off()
  
  #Variable for peak and low times
  #Salience
  dat$peak <- 0
  dat$peak[dat$n_ts_sm>mean(dat$n_ts_sm)+sd(dat$n_ts_sm)] <- 1
  dat$peak[dat$n_ts_sm<mean(dat$n_ts_sm)-sd(dat$n_ts_sm)] <- -1
  #Tonality
  dat$peak_neg <- 0
  dat$peak_neg[dat$ton_neg_ts_sm>mean(dat$ton_neg_ts_sm)+sd(dat$ton_neg_ts_sm)] <- 1
  dat$peak_neg[dat$ton_neg_ts_sm<mean(dat$ton_neg_ts_sm)-sd(dat$ton_neg_ts_sm)] <- -1
  
  #QUESTION: Should we use the smoothed indicators for identfiying the peaks, or the initial ones? 
  # I would use the smoothed ones since they correspond to the plots we are interpreting
  # I do not think this makes a big difference, anyway
  
  #Entity-Identifier for merging
  dat$entity_id <- governors[i,1]
  
  #Remove these variables: same names in dat_base (initial corpus); later we will merge them
  dat$n <- NULL
  dat$n_w <- NULL
  
  governor_list[[governors[i,2]]] <- dat
}
#governor_list[["all"]] <- dat_all
rm(dat, dat_all, weekz, i)

#Since rbind cannot handle time-series operators, we have to transform the datasets
transform_ts <- function(df) {
  #df <- get(df)
  df$week <- NULL
  df$n_ts <- as.numeric(df$n_ts)
  df$n_w_ts <- as.numeric(df$n_w_ts)
  df$ton_neg_ts <- as.numeric(df$ton_neg_ts)
  df$n_ts_sm <- as.numeric(df$n_ts_sm)
  df$n_w_ts_sm <- as.numeric(df$n_w_ts_sm)
  df$ton_neg_ts_sm <- as.numeric(df$ton_neg_ts_sm)
  return(df)
  }

#apply the function to all dfs
governor_list <- lapply(governor_list, transform_ts)

#Bind all the individual governors datasets together
governor_df <- do.call("rbind", governor_list)

#-> merge with the full dataset: Assign for each article whether it was published in peak/low time or not
#Merge (many-to-one) Organization dataset (dat_org) with full Corpus (dat_base)
dat_peak <- merge(dat_base, governor_df, by = c("weeks","years","entity_id"), all.x = T)

#Save that file
saveRDS(dat_peak, "../data/UKCorpus_PeakLow.rds") # save an rds file is quite efficient
rm(df, days, dat_base, governor_list, governor_df, governors)
