#Dependent libraries loading
install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages('twitteR', dependencies=T)
install.packages('R6', dependencies=T)
install.packages('stringi', dependencies=T)
install.packages('stringr', dependencies=T)
library(devtools)
install_github("twitteR", username="geoffjentry")
library(twitteR)
library(R6)  
library(stringi)  
library(stringr)
#Establiashing connection
api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXX"
api_secret <- "XXXXXXXXXXXXXXXXXXXXXXXX"
access_token <- "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
access_token_secret <- "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#test if working well, fetch some tweets from the data incubator
searchTwitter("thedatainc")
#search Twitter and fetch up to 1,500 results
thedatainc.tweets = searchTwitter('@thedatainc', n=1500)
######################################
#Estimating Sentiment

#http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
#Hu and Liu's "opinion lexicon" categorizes nearly 6,800 words as positive or negative
hu.liu.pos = scan('C:/Users/gd_su/Documents/Classes/sentiment analysis/positive-words.txt',
                  what='character', comment.char=';')
hu.liu.neg = scan('C:/Users/gd_su/Documents/Classes/sentiment analysis/negative-words.txt',
                  what='character', comment.char=';')
#adding few context relevant words
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting',
              'epicfail', 'mechanical')

#Using a setiment scoring algm #Ref-http://thinktostart.com/sentiment-analysis-on-twitter/

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use "l" + "a" + "ply" = "laply":  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
  
    # clean up sentences with R's regex-driven global substitute, gsub():  
    sentence = iconv(sentence, 'UTF-8', 'ASCII') #ASCII cursewords    
    sentence = gsub('[[:punct:]]', '', sentence) #punctuations
    sentence = gsub('[[:cntrl:]]', '', sentence) #control characters
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence) #convert to lower case
    
    word.list = str_split(sentence, '\\s+') #split sentences to words into list
    words = unlist(word.list) #unlisting the words
    
    pos.matches = match(words, pos.words) #matching sentence words with pos and neg words 
    neg.matches = match(words, neg.words)
    
    #match() returns the position of the matched word or NA
    #We just want the outcome to be TRUE/FALSE
    
    pos.matches = !is.na(pos.matches) #collecting non NA terms
    neg.matches = !is.na(neg.matches)
    
    #TRUE/ FALSE will be treated as 1/0 by sum() fn:
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

######################################
#Extracting text
#install.packages("plyr")
library(plyr) 
library(stringr)

delta.tweets = searchTwitter('@delta', since = "2016-08-22", until = "2016-09-03",n=1500)
length(delta.tweets)
tweets.df = twListToDF(delta.tweets)
tweet = delta.tweets[[1]]
delta.text = laply(delta.tweets, function(t) t$getText() )
#removing graphical inputs from customers
delta.text=str_replace_all(delta.text,"[^[:graph:]]", " ") 
head(delta.text, 5)

#Scoring the tweets
delta.scores = score.sentiment(delta.text, pos.words,
                               neg.words, .progress='text')
#adding new cols
delta.scores$airline = 'Delta' 
delta.scores$code = 'DL'

#ploting histogram view
hist(delta.scores$score)
library(ggplot2)
q = qplot(delta.scores$score)
q = q + theme_bw()
q
######################################
#similarly capturing other airlines tweets 
american.tweets = searchTwitter('@AmericanAir',since = "2016-08-22", until = "2016-09-03", n=1500)
length(american.tweets)
tweetAA = american.tweets[[1]]
tweetAA$getScreenName() 
tweetAA$getText() 

american.text = laply(american.tweets, function(t) t$getText() )
american.text=str_replace_all(american.text,"[^[:graph:]]", " ") 

#Scoring the tweets
american.scores = score.sentiment(american.text, pos.words,
                                  neg.words, .progress='text')

american.scores$airline = 'American' 
american.scores$code = 'AA'
hist(american.scores$score)

q = qplot(american.scores$score)
q = q + theme_bw()
q
######################################
jetblue.tweets = searchTwitter('@JetBlue', since = "2016-08-22", until = "2016-09-03",n=1500)
length(jetblue.tweets)

tweetJB = jetblue.tweets[[1]]
tweetJB$getScreenName()
tweetJB$getText() 

jetblue.text = laply(jetblue.tweets, function(t) t$getText() )
jetblue.text=str_replace_all(jetblue.text,"[^[:graph:]]", " ") 
#Scoring the tweets
jetblue.scores = score.sentiment(jetblue.text, pos.words,
                                 neg.words, .progress='text')

jetblue.scores$airline = 'JetBlue' 
jetblue.scores$code = 'JB'
hist(jetblue.scores$score)

q = qplot(jetblue.scores$score)
q = q + theme_bw()
q
######################################
southwest.tweets = searchTwitter('@southwestAir',since = "2016-08-22", until = "2016-09-03", n=1500)

tweetSW = southwest.tweets[[1]]
tweetSW$getScreenName()
tweetSW$getText() 

southwest.text = laply(southwest.tweets, function(t) t$getText() )
length(southwest.text)
head(southwest.text, 5)
southwest.text=str_replace_all(southwest.text,"[^[:graph:]]", " ") 

#Scoring the tweets
southwest.scores = score.sentiment(southwest.text, pos.words,
                                   neg.words, .progress='text')

southwest.scores$airline = 'Southwest' 
southwest.scores$code = 'SW'
hist(southwest.scores$score)

q = qplot(southwest.scores$score)
q = q + theme_bw()
q
######################################

united.tweets = searchTwitter('@United Airlines', since = "2016-08-22", until = "2016-09-03",n=1500)
tweet = united.tweets[[1]]
tweet$getScreenName()
tweet$getText() 
united.text = laply(united.tweets, function(t) t$getText() )
united.text=str_replace_all(united.text,"[^[:graph:]]", " ") 
head(united.text, 5)

#Scoring the tweets
united.scores = score.sentiment(united.text, pos.words,
                                neg.words, .progress='text')

united.scores$airline = 'United' 
united.scores$code = 'UAA'
hist(united.scores$score)

q = qplot(united.scores$score)
q = q + theme_bw()
q
######################################
VA.tweets = searchTwitter('@VirginAmerica',since = "2016-08-22", until = "2016-09-03", n=1500)
length(VA.tweets)

tweet = VA.tweets[[1]]
tweet$getScreenName()
tweet$getText() 
VA.text = laply(VA.tweets, function(t) t$getText() )
VA.text=str_replace_all(VA.text,"[^[:graph:]]", " ") 
head(VA.text, 5)

#Scoring the tweets
VA.scores = score.sentiment(VA.text, pos.words,
                            neg.words, .progress='text')

VA.scores$airline = 'VirginAmerica' 
VA.scores$code = 'VA'
hist(VA.scores$score)

q = qplot(VA.scores$score)
q = q + theme_bw()
q

#########################################################################
#combining all tweets into one dataframe
all.scores = rbind( american.scores, delta.scores, jetblue.scores,
                      southwest.scores, united.scores, VA.scores)

##Plotting a score chart
library(ggplot2)
g = ggplot(data=all.scores, mapping=aes(x=score, fill=airline) )
g = g + geom_bar(binwidth=1)
g
#separate subplot
g = g + facet_grid(airline~.)
g

####################################################################################
##Let's create two new boolean columns to focus on tweets with 
##very negative (score <= -2) and very positive (score >= 2) sentiment scores:

all.scores$very.pos.bool = all.scores$score >= 2
all.scores$very.neg.bool = all.scores$score <= -2

all.scores[c(1,6,47,99), c(1, 3:6)]

##We want to count the occurrence of these these strong sentiments for each airline. 
##We can easily cast these TRUE/FALSE values to numeric 1/0
##we can then use sum() to count them:
all.scores$very.pos = as.numeric( all.scores$very.pos.bool )
all.scores$very.neg = as.numeric( all.scores$very.neg.bool )

all.scores[c(1,6,47,99), c(1, 3:8)]

#We can use plyr's ddply() function to aggregate the rows for each airline, calling the summarise() function to create new columns containing the counts:

twitter.df = ddply(all.scores, c('airline', 'code'), summarise, very.pos.count=sum( very.pos ),very.neg.count=sum( very.neg ) )

#As a single, final score for each airline, let's calculate the percentage of these "extreme" tweets which are positive:

twitter.df$very.tot = twitter.df$very.pos.count + twitter.df$very.neg.count
twitter.df$score = round( 100 * twitter.df$very.pos.count / twitter.df$very.tot )

##The orderBy() function from the doBy package makes it easy to sort the results. 
##Note that it preserves the original row names:
library(doBy)
orderBy(~-score, twitter.df)
 
###################################################################################
##Compare with ACSI's customer satisfaction index
##scrape ACSI website - let's compare our results with the American Customer Satisfaction Index. 
##Each year the ACSI conducts tens of thousands of interviews to measure consumer satisfaction with hundreds of companies and organizations. 
#They publish their top-level results on http://www.theacsi.org/.
########################################################################

##Scrape ASCI website - 
##install.packages("XML") 
##provides many useful functions for scraping and parsing data from the web.
##readHTMLTable() will download a web page from a URL, parse the HTML, 
#extract any tables, and return a list of populated data.frames, 
#complete with headers

##url: acsi benchmark by airline industry
library(XML)
acsi.url = 'http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'

# specify which=1 to retrieve only the first table on the page
#header=T to indicate that the table headings should be used as column names
acsi.df = readHTMLTable(acsi.url, header=T, which=1, stringsAsFactors=F)

#For most recent results, only keep 1st col (airline names) and 19th (containing 2016's scores):
acsi.df = acsi.df[,c(1,19)]
colnames(acsi.df)   #Changing the column names
colnames(acsi.df) = c('airline', 'score')
colnames(acsi.df)

## add two-letter airline codes and ensure that the scores are treated as numbers
acsi.df$code = c('B6','WN','As', NA, NA, 'AA', 'DL', 'UA', 'F9', 'G4', 'NK', 'US', 'NW', 'CO')
acsi.df$score = as.numeric(acsi.df$score)

acsi.df


#################################################################################3

##Compare Twitter results with ACSI scores

#Construct new data frame conatining asci and predicted results:
compare.df = merge(twitter.df, acsi.df, by= c('code','airline'), suffixes=c('.twitter', '.acsi'))

##Unless you specify all=T, non-matching rows will be dropped by merge()-like SQL's INNER JOIN-
##and that's what happened to top-scoring JetBlue.
#############################################################3
##not working
##Graph the results
library(ggplot2)
#install.packages("getopt") #library(getopt)
g = ggplot( compare.df, aes(x=score.twitter, y=score.acsi) ) +geom_point( aes(color=airline), size=5 ) + theme_bw() + labs( legend.position=c(0.2, 0.85) )
g = g + geom_smooth(aes(group=1), se=F, method="lm")


##############################################################
##############END#######
 

