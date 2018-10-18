##### Media-driven accountability
# UK-PROTOTYPE
# bw, ms, and rw

# Do not clear your workspace, otherwise the working directory changes.
# Restart R if you have to free memory or detach packages, objects or commands
# rm(list = ls(envir = globalenv()), envir = globalenv())

# do not set the working directory every time but use pathes relative to your git clone--which
# should be located in the Dropbox of this project (the one called 'chapters')
setwd("/home/pdm2admin/model")
getwd()

### PREPARATION ###
#------------------

#MS WD
setwd("/Users/noerber/Desktop/PhD/MDA/Data/Media-Data/Prototype_UK")
getwd()

#load corpus
#corpus <- readRDS("../data/UKCorpus_PeakLow.rds") # note the relative path. This should now work for everyone.

corpus <- readRDS("UKCorpus_PeakLow.rds")


#load libraries
library(tm)
library(stm)
library(lubridate)

#first, we have to prepare our texts for the analysis

# topic models should not be run across languages (we should discuss machine translating everything!)

#take only peak time articles
corpus <- corpus[order(corpus$article_date, decreasing = T),]
corpus$weeks <- week(corpus$article_date)
corpus <- corpus[corpus$weeks != 53,]
corpus$years <- year(corpus$article_date)
corpus$week <- as.numeric(as.factor(as.Date(paste(corpus$years,
                                                  corpus$weeks, 1, sep="-"), "%Y-%U-%u")))

# preprocess texts (rather rough but quite standard). Denny and Spirling
# (https://www.nyu.edu/projects/spirling/documents/preprocessing.pdf) argue that we should evaluate this, too (?).
texts <- Corpus(VectorSource(corpus$article_text))
texts <- tm_map(texts, tolower)
texts <- tm_map(texts, removePunctuation)
texts <- tm_map(texts, removeNumbers)
texts <- tm_map(texts, stripWhitespace)
for (i in 1:length(corpus$article_text)) {
  corpus$text_preprocessed[i] <- gsub("\\s", " ", texts[[i]])
}
corpus$text_preprocessed <- gsub("\\s+", " ", corpus$text_preprocessed)
corpus$text_preprocessed <- gsub("^\\s+|\\s+$", "", corpus$text_preprocessed)

#General comments Jofre
#JR: Control variables: there are already more than enough controls. Having the entity name in the group might be redundant if we have 
    #already a dummy that identified problematic cases. On the correlation between variables and types of governors, the kind of problems 
    #pointed out by Michael are going to be present throughout. Each IP will dominate in one particular territorial_scope and policy_scope. 
    #For instance, all of IP2 is going to be actor_type=public and none is going to be "regional_national". However, I do not think we 
    #need to exclude anything from the model, because as long as we include entity name as a control variable 
    #and we can analyze considering the effect of IPs, keeping all variables in the model is how we are able to make general statements.
#JR: Dummy variables to identify particular governors: In my case, it is important to point out the EC, with #entity_id 304219, and entity_name 
    #european commission. 
#JR: Recoding of the independent variables: I checked the recoding according to my IP, see my branch. Only a couple of comments, 
    #I think we are looking at the right categories already.

#MS: I have included the recoding comments in this file above

# recode some vars. The stm likes factors - m.n. in the postestimation -, so we encode our indicators accordingly
corpus$actor_type[corpus$actor_type %in% c("hybrid", "private")] <- "hybrid_private"
corpus$actor_type <- as.factor(corpus$actor_type)
corpus$tonality_verbalized <- as.factor(corpus$tonality_verbalized)
corpus$policy_scope <- as.factor(corpus$policy_scope)
corpus$territorial_scope[corpus$territorial_scope %in% c("subnational", "regional")] <- "regional_national"
corpus$territorial_scope <- as.factor(corpus$territorial_scope)
corpus$media_source <- as.factor(corpus$media_source)
corpus$entity_name <- as.factor(corpus$entity_name)
#JR: is not not easier to work with entity_id numbers?
#MS: I guess it's good to have the entity names accessible and since we're working with factor variables 
    #it doesn't make a difference anyway, right?

#corpus$media_type[corpus$media_type %in% c("regional", "tabloid_or_free")] <- "regional_tabloid_free" 
#MS: my earlier analyses have suggested, that this might be one of the (few) 
  #interesting sources of variation (i.e. regional news alone) for IP1; I would therefore prefer to keep them separate if possible
#JR: I did the same grouping in my phd and it works well

#corpus$media_type[corpus$media_type %in% c("magazines", "quality")] <- "quality_magazines" #JR: for clarity
#MS: @Jofre: so you wouldn't put magazines and quality together "for clarity"? The same argument can be made for regional and tabloid/free. 
    #Do you have a vital interest in differentiating between the two? Otherwise I would suggest to lump them together nevertheless.

corpus$media_type <- as.factor(corpus$media_type)
corpus$peak <- as.factor(corpus$peak)
corpus$policy_output[corpus$policy_output %in% c("hard", "hard / soft")] <- "hard" 
#MS: what is this category "hard / soft" anyways? If it's not soft output, it's hard, right? So we can call it that way...
# RW: by the same logic: if it's not hard, then it's soft, so we can call it soft. I don't know which way is better, but we should have
#     theoretical reasons for whether hard/soft is more like hard or more like soft.
# MS: Yes, you're right. Sorry, I was imprecise. I thought that the "hard/soft" category is a category for governors that can produce hard
    #output (i.e. they can exercise authority through formal means), but that very rarely do so. For me this would qualify as "hard" output,
    #since they have the possibility to do so in principle. But maybe I'm also wrong about this interpretation of the coding
corpus$policy_output <- as.factor(corpus$policy_output)
#JR: In IP2 it is likely that all hard is concentrated in the EC, again with a separate analysis in the chapter this should not be a problem.
#corpus$policy_field_1 <- tolower(corpus$policy_field_1)
#corpus$policy_field_1 <- ifelse(corpus$policy_field_1 %in% c("multiple", "not definable", "", "."), "multiple", "specific")
#corpus$policy_field_1[is.na(corpus$policy_field_1)] <- "multiple"
#corpus$policy_field_1 <- as.factor(corpus$policy_field_1) 
#MS: How does the indicator policy_field_1 exactly differ from policy_scope (narrow vs. wide)? 
  #Shouldn't we just decide for one of the two? My suggestion is to drop the "policy_field_1" variable and only keep "policy_scope"
# RW: in my understanding, policy_field is the policy issue area in which an actor is active, whereas policy_scope describes the
#     scope that the actor has. I could imagine that accountability may differ across policy fields because fields like commerce,
#     economic liberalism, environment, and security are more salient than fields like science or justice.
# MS: Yes, that differentiation might be interesting, but if you look at the levels this recoded factor has, it's just "multiple" 
      #and "specific". So no possibility to distinguish policy fields substantially, but only - as I understand it - whether an actor
      #is engaged in many/several policy domains, or only in one/few. Hence my question how it differs from policy scope, which in my understanding
      #measures the same thing
corpus <- corpus[!is.na(corpus$policy_output),]

#IP-identifier
#MS: Problem with bank for international settlements, it's both listed (in excel-list) for IP2 and IP4 
    #(at the moment I've assigned it to IP2) -> @Jofre and @Reto: you have to decide which one takes it; please
    #also verify that you don't have other overlaps in governors that I might have missed
# RW: sorry, a basic question: what are the criteria that delineate the different IPs? IMO, the entities should be similar within an IP and different across IPs.
#     But when I look at the clusters below, they seem to be pretty heterogeneous.
#MS: You're right, it's not very clear. IP1 is clear I guess, it's the only subnational one. For IP2 I think it's only 
    # the international organizations, for IP5 it's only private governors, but for IP4, I'm not exactly sure what the selection criterion
    # was, apart from the fact that it's transnational governmental networks. So I guess it's mostly about making the difference bw. 
    # IP2 and IP4 more clear. But technically, you should know that ;).

corpus$IP_ID <- "IP5"
corpus$IP_ID[corpus$entity_name %in% c("greater london authority (gla)","transport for london",
                                       "west midlands integrated transport authority (wmita)")] <- "IP1"
corpus$IP_ID[corpus$entity_name %in% c("bank for international settlements","central commission for the navigation of the rhine",
                                       "commonwealth of nations","council of europe","euro free trade association",
                                       "euro-mediterranean parliamentary assembly","european commission","european economic area",
                                       "european parliament","european space agency","francophone parliamentary assembly",
                                       "igad inter-parliamentary union","inter-parliamentary union","intergovernmental organization for international carriage by rail",
                                       "international criminal court","international whaling commission","interparliamentary assembly of the eurasian economic community",
                                       "joint parliamentary assembly africa - caribbean - pacific - european union","organization for economic cooperation & development",
                                       "organization for security and cooperation in europe",
                                       "parliamentary assembly of the council of europe")] <- "IP2"
corpus$IP_ID[corpus$entity_name %in% c("basel committee on banking supervision","european banking authority","european competition network",
                                       "european environment agency","european insurance and occupational pensions authority",
                                       "european securities and markets authority","european union network for the implementation and enforcement of environmental law",
                                       "financial stability institute","international association of insurance supervisors","international competition network",
                                       "international network for environmental compliance and enforcement",
                                       "international organization of securities commissions")] <- "IP4"

# RW: IP_ID should be a factor. Or did I overlook something here?
# MS: You're right, forgot to do that, here it goes
corpus$IP_ID <- as.factor(corpus$IP_ID)

#Dummy-Variables for "extraordinary" governors
#MS: in general we should keep the number of these dummies as low as possible, since each of them means an additional interaction
  #with all the variables in the model to be able to properly distinguish; so only if you think it's absolutely necessary
# RW: agree
table(corpus$entity_name)
#MS: governors with high article counts: s&p (109'453), european parliament (55'067), moody's (34'635)

#MS: makes up for 1/3 of the sample, we should probably be able to analyse the data w/o s&p included
# RW: agree
corpus$sp <- 0
corpus$sp[corpus$entity_name=="standard & poor's"] <- 1 

#CROSS-TABULATION TO DETECT RARE COMBINATIONS / "MULTI-CORRELATION"
table(corpus$actor_type,corpus$territorial_scope) #only 22 in the "hybrid_private, regional_national" cell
table(corpus$actor_type,corpus$policy_scope) #only 1 in the "public (elected), narrow" cell
table(corpus$actor_type,corpus$media_type) #ok
table(corpus$actor_type,corpus$policy_output) #ok
table(corpus$actor_type,corpus$functional_scope_decisive) #60 in the "public (elected), not-decisive" cell
table(corpus$actor_type,corpus$functional_scope_implementing) #211 in the "public (elected), decisive" cell
table(corpus$actor_type,corpus$functional_scope_informative) #none in the "public (elected), not-decisive" cell
table(corpus$actor_type,corpus$media_country) #ok
table(corpus$actor_type,corpus$IP_ID) #none in the "hybrid_private, IP1", "hybrid_private, IP2", "public (elected), IP4", and "public (elected), IP5" cells
#MS: might the empty cells be a problem for IP_ID interactions with actor_type in the model (see also below, especially problem for IP1)?

table(corpus$policy_scope,corpus$territorial_scope) #ok
table(corpus$policy_scope,corpus$media_type) #ok
table(corpus$policy_scope,corpus$policy_output) #ok
table(corpus$policy_scope,corpus$functional_scope_decisive) #ok
table(corpus$policy_scope,corpus$functional_scope_implementing) #ok
table(corpus$policy_scope,corpus$functional_scope_informative) #none in the "wide, not-decisive" cell
table(corpus$policy_scope,corpus$media_country) #ok
table(corpus$policy_scope,corpus$IP_ID) #none in the "wide, IP4" cell, 211 in "wide, IP1" cell

table(corpus$territorial_scope,corpus$media_type) #ok
table(corpus$territorial_scope,corpus$policy_output) #ok
table(corpus$territorial_scope,corpus$functional_scope_decisive) #ok
table(corpus$territorial_scope,corpus$functional_scope_implementing) #ok
table(corpus$territorial_scope,corpus$functional_scope_informative) #ok
table(corpus$territorial_scope,corpus$media_country) #ok
table(corpus$territorial_scope,corpus$IP_ID) #none in the "global, IP1"cell, 117 in "regional_national, IP5" cell

table(corpus$media_type,corpus$policy_output) #ok
table(corpus$media_type,corpus$functional_scope_decisive) #ok
table(corpus$media_type,corpus$functional_scope_implementing) #ok
table(corpus$media_type,corpus$functional_scope_informative) #ok
table(corpus$media_type,corpus$media_country) 
#MS: Perfect overlap of media_type and media_country (newswire=international) -> Media country can be removed from model
table(corpus$media_type,corpus$IP_ID) #none in the "global, IP1"cell, 117 in "regional_national, IP5" cell

table(corpus$policy_output,corpus$functional_scope_decisive) #ok
table(corpus$policy_output,corpus$functional_scope_implementing) #ok
table(corpus$policy_output,corpus$functional_scope_informative) #ok
table(corpus$policy_output,corpus$IP_ID) #none in the "soft, IP1" cell

table(corpus$functional_scope_decisive,corpus$functional_scope_implementing) #ok
table(corpus$functional_scope_decisive,corpus$functional_scope_informative) #ok
table(corpus$functional_scope_decisive,corpus$IP_ID) #ok

table(corpus$functional_scope_implementing,corpus$functional_scope_informative) #ok
table(corpus$functional_scope_implementing,corpus$IP_ID) #none for not-implementing, IP1"

table(corpus$functional_scope_informative,corpus$IP_ID) #none for not-informative, IP4"

#write out corpus for the w2v evaluation (see below)
write.csv(corpus$text_preprocessed, "../Analysis/data/corpus_w2v.csv", row.names = F, fileEncoding = "utf-8")

# Generate a corpus for the stm 
corpus_stm <- textProcessor(corpus[,c("text_preprocessed")], metadata=corpus, stem=T,
                            language="english", removestopwords=T, lowercase=F,
                            removenumbers=F, removepunctuation=F, customstopwords = c("will", "said", "can", "also", "may"))
corpus_stm <- prepDocuments(corpus_stm$documents, corpus_stm$vocab, corpus_stm$meta, lower.thresh = 10)

saveRDS(corpus_stm, "../data/corpus.rds")

# Since we will use a deterministic (spectral) initialization of our topic model, many paramters of the topic model will 
# be fixed. We only need an evaluation of the optimal number of topics, which we achive in three steps

## 1. run a topic model for every granularity in the range of interest

# define range of topic numbers for evaluation
ks <- 3:15 #for testing, we only evaluate 4 models, a more meaningful range is 3:50 or even 3:100

# define the prevalence formula (basically a regression of indicators on the topic prevalences generated by the model)
# for clarity
formula_prev <- "prevalence =~ tonality_verbalized:IP_ID + territorial_scope:IP_ID + entity_name:IP_ID + peak:IP_ID + policy_output:IP_ID + actor_type:IP_ID + policy_scope:IP_ID + media_type:IP_ID + media_source:IP_ID + article_word_count:IP_ID + s(week):IP_ID + functional_scope_informative:IP_ID + functional_scope_implementing:IP_ID + functional_scope_decisive:IP_ID"
#MS: I have now interacted all the variables with the IP_ID variable, so we can make separate analyses for our IP chapters; in addition
  #I have removed the variables "media_country" and "policy_field_1" from the model for the reasons specified in the comments above;
  #what is still missing so far, is the dummy variable for S&P's, which I think should be included, but I'm not sure how: if we want
  #to conduct analysis on a subset of the model w/o S&P's, do we need to include an additional interaction with that dummy for all the
  #variables? If so, can someone please implement that?
  #In addition, I'm not sure, if we have to interact the IP_ID with the entity_name variable, since it will be perfectly covarying

# RW: as all variables are factor variables, it shouldn't be a problem if some cells contain zero observations.
#     Take for example the interaction between functional_scope_informative and IP_ID: IP4 and not-informative is empty, so there will just
#     be no interaction between IP4 and not-informative. What I consider more problematic are the cases with very small numbers of observations
#     in cells. For example, take actor_type and IP_ID: hybrid_private and IP4 has two observations. So there will be an interaction, but only based
#     on two observations (problematic in the sense that we will have much uncertainty around estimates, but I guess we cannot avoid that).

#MS: Ok, that makes sense. But apart from the fact that we will have very uncertain estimates for that interaction, does it affect the rest
    #of the model in a negative way? If not, we can just not interpret that particular interaction and still leave it in, right?


# evaluate on the 100 most probable words per topic
n_words <- 100
rm(texts, i, corpus)

# loop over ks and write out most probable words per topic for each model
for (k in ks) {
  STM <- stm(corpus_stm$documents, corpus_stm$vocab, K=30, 
             as.formula(formula_prev), data = corpus_stm$meta,
             init.type="Spectral", verbose=T, 
             control = list(nits = 100, burnin = 25))
  words <- labelTopics(STM, n = n_words)
  words <- as.data.frame(words[[1]])
  outname <- paste0("./results/words_", k, "_", ".txt")
  write.table(words, outname, col.names = F, row.names = F, sep = "\t", quote = F)
} 
#this command seems to take too much working space and cannot be conducted; what to do? > ask Lucien Baumgartner (luciengeorge.baumgartner@uzh.ch) for
# a spot to my old production server. Lucien can give you a Login to an RStudio-instance that will be able to stem this large files.
# BTW: My three years old MacBook Air can run the models.

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
saveRDS(STM, "../data/STM_3.rds")
STM <- readRDS("../data/STM_15.rds")

# Save 100 most probable words per topic
words <- labelTopics(STM, n = 100)
words <- as.data.frame(t(words[[1]]))
write.table(words, file = "./results/words_15.txt", col.names = T, row.names = F, sep = "\t", quote = F)

k <- 15

#save 5 most relevant documents per topic
thoughts <- findThoughts(STM, texts = corpus_stm$meta$article_text, n = 5, topics = 1:k)
thoughtsDocs <- data.frame(matrix(unlist(thoughts$docs), nrow=k, byrow=T))
thoughtsIndices <- data.frame(matrix(unlist(thoughts$index), nrow=k, byrow=T))
thoughts <- cbind(thoughtsDocs, thoughtsIndices)
write.csv(thoughts, paste0("./results/thoughts_", k, ".csv"),
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

outname <- paste0("./results/scores_15.txt")
write.table(scores, outname, col.names = T, row.names = F, sep = "\t", quote = F)


# produce word-plots

# Read data and check its structure
words <- read.delim2("./results/words_15.txt")
scores <- read.delim2("./results/scores_15.txt")
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

library(ggplot2)

# Plot and Save Top 50 (n) Words for each Topic (ncol(words)) by Prob, Excl and Freq
n <- 50 
for(i in 1:ncol(words)){
  topic <- data.frame(c(scores[, c(pr[i], ex[i], fr[i], co + 1)], i))
  colnames(topic) <- c("Probability", "Exclusivity", "Frequency", "Words", "Topic")
  topic <- topic[ topic[,4] %in% words[1:n,i] ,]
  pdf(file = paste("./results/wordplot_15_topic_", i, ".pdf", sep = ""), paper = "special", width=7, height=5.5)
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
outname <- paste0("./results/effects_15_", "WeeksPassed.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate peak
prep <- estimateEffect(1:k ~ peak, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "peak", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "peak")
outname <- paste0("./results/effects_15_", "peak.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate media_type
prep <- estimateEffect(1:k ~ media_type, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "media_type", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "media_type")
outname <- paste0("./results/effects_15_", "media_type.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate actor_type
prep <- estimateEffect(1:k ~ actor_type, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "actor_type", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "media_type")
outname <- paste0("./results/effects_15_", "actor_type.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate tonality_verbalized
prep <- estimateEffect(1:k ~ tonality_verbalized, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "tonality_verbalized", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "tonality_verbalized")
outname <- paste0("./results/effects_15_", "tonality_verbalized.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate territorial_scope
prep <- estimateEffect(1:k ~ territorial_scope, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "territorial_scope", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "territorial_scope")
outname <- paste0("./results/effects_15_", "territorial_scope.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate policy_output
prep <- estimateEffect(1:k ~ policy_output, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "policy_output", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "policy_output")
outname <- paste0("./results/effects_15_", "policy_output.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate policy_scope
prep <- estimateEffect(1:k ~ policy_scope, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "policy_scope", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "policy_scope")
outname <- paste0("./results/effects_15_", "policy_scope.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate functional_scope_informative
prep <- estimateEffect(1:k ~ functional_scope_informative, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "functional_scope_informative", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "functional_scope_informative")
outname <- paste0("./results/effects_15_", "functional_scope_informative.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate functional_scope_implementing
prep <- estimateEffect(1:k ~ functional_scope_implementing, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "functional_scope_implementing", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "functional_scope_implementing")
outname <- paste0("./results/effects_15_", "functional_scope_implementing.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)

#estimate functional_scope_decisive
prep <- estimateEffect(1:k ~ functional_scope_decisive, STM, meta = corpus_stm$meta)
x <- plot(prep, covariate = "functional_scope_decisive", model = STM, method="pointestimate")
dfs <- data.frame()
for (i in 1:length(x$means)){
  df <- data.frame(x$means[[i]], x$cis[[i]][1,], x$cis[[i]][2,])
  df <- cbind(df, i, x$uvals)
  dfs <- rbind(dfs, df)
}
colnames(dfs) <- c("pointEstimate", "lb", "ub", "topic", "functional_scope_decisive")
outname <- paste0("./results/effects_15_", "functional_scope_decisive.txt")
write.table(dfs, outname, col.names = T, row.names = F, sep = "\t", quote = F)



#########



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
d <- data.frame(read.delim(paste0("./results/effects_15_", "WeeksPassed.txt")))

d <- d[order(d$topic, d$WeeksPassed),]
length(d$WeeksPassed)
max(d$WeeksPassed)
mo <- seq(as.Date("2005-01-01"), by = "week", length.out = max(d$WeeksPassed))

topic_labels <- 1:15 # can be used later to define substantial topics
d$topicLabels <- NA
d$date <- rep(mo, length(topic_labels))

colnames(d)[5] <- c("var")

d$pointEstimateAverage <- NA

for(j in 1:length(topic_labels)){
  d$topicLabels[d$topic == j] <- topic_labels[j]
  d$pointEstimateAverage[d$topic == j] <- mean(d$pointEstimate[d$topic == j])
}

d$topicLabels <- factor(d$topicLabels, levels = unique(d$topicLabels[order(d$pointEstimateAverage, decreasing = TRUE)]))

pdf(file="./results/correlation_15_weekspassed.pdf", paper="special", width=12, height=9)
print(ggplot(data = d, aes(x = date, y = pointEstimate)) +
        geom_line() +
        geom_hline(aes(yintercept = pointEstimateAverage), linetype = 2) +
        facet_wrap(~ topicLabels, ncol = 3) +
        labs(x = "weeks", y = "Topic prevalence", title = "") +
        theme(legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, "cm")) +
        scale_alpha(guide = "none") +
        geom_ribbon(aes(ymin = lb, ymax= ub, alpha = 0.5))) 
dev.off()

# correlation of topic prevalence with peak
file_name <- "./results/effects_15_peak.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_peak.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()

# correlation of topic prevalence with media_type
file_name <- "./results/effects_15_media_type.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_media_type.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()


# correlation of topic prevalence with actor_type
file_name <- "./results/effects_15_actor_type.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_actor_type.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()


# correlation of topic prevalence with tonality_verbalized
file_name <- "./results/effects_15_tonality_verbalized.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_tonality_verbalized.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()



# correlation of topic prevalence with territorial_scope
file_name <- "./results/effects_15_territorial_scope.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_territorial_scope.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()


# correlation of topic prevalence with policy_output
file_name <- "./results/effects_15_policy_output.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_policy_output.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()


# correlation of topic prevalence with policy_scope
file_name <- "./results/effects_15_policy_scope.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_policy_scope.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()


# correlation of topic prevalence with functional_scope_decisive
file_name <- "./results/effects_15_functional_scope_decisive.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_functional_scope_decisive.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()


# correlation of topic prevalence with functional_scope_implementing
file_name <- "./results/effects_15_functional_scope_implementing.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_functional_scope_implementing.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()

file_name <- "./results/effects_15_functional_scope_informative.txt"
var_label_long <- "?"

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
d$var <- as.factor(d$var)

pdf(file=paste("./results/correlations_15_functional_scope_informative.pdf", sep=""), paper="special", width=7, height=9)
print(
  ggplot(data = d, aes(x = topicLabels, y = pointEstimate, ymin = lb, ymax = ub, shape = var)) + #color = groupLabels, 
    geom_pointrange(position=position_dodge(width=0.5)) +
    coord_flip() +
    geom_hline(yintercept = 1/k, lty = 2, size = 0.5) +
    labs(x = "", y = "Topic prevalence", title = "") +
    theme(legend.position = "bottom", legend.title = element_blank())
)
dev.off()
