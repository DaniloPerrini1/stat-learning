rm(list=ls())
setwd("E:/asddf/progetto stat le")
AirBeB<-read.csv(("train.csv"),header = T)
colnames(AirBeB)
sum(is.na(AirBeB))
AirBeB[AirBeB==""]<-NA
#ID 
#è inserita come numerica e il minimo è 334 strano nb
summary(AirBeB$id)
#guardo il boxplot per vedere se 334 non sia un errore
boxplot(AirBeB$id)
hist(AirBeB$id)
cor(AirBeB$id,AirBeB$number_of_reviews)
plot(log(AirBeB$price)~AirBeB$id)

#logprice
summary(AirBeB$log_price)
hist((AirBeB$log_price))
AirBeB$price<-exp(AirBeB$log_price)
boxplot(AirBeB$price)
quantile(AirBeB$price)
#Propriety type
str(AirBeB$property_type)
levels(AirBeB$property_type)
table(AirBeB$property_type)
ag<-aggregate(price~ property_type,AirBeB ,max)
which.max(ag[,2])
ag[34,]

#Room type
str(AirBeB$room_type)
levels(AirBeB$room_type)
table(AirBeB$room_type)

#amenities
str(AirBeB$amenities)
AirBeB$amenities[1]
#amenities contiene delle tag praticamente si possono estrarre delle parole e usare come dummy
#la mettero char più avanti


#accomodates numero di posti
str(AirBeB$accommodates)
summary(AirBeB$accommodates)
sum(is.na(AirBeB$accommodates))
(plot(AirBeB$price~AirBeB$accommodates))
cor(AirBeB$price,AirBeB$accommodates)

#bathrooms
str(AirBeB$bathrooms)
summary(AirBeB$bathrooms)
#install.packages("VIM")
library(VIM)
aggr(AirBeB, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)
table(AirBeB$accommodates[is.na(AirBeB$bathrooms)==T])

AirBeB$description[is.na(AirBeB$bathrooms)==T]




aggregate(bathrooms~as.factor(accommodates),data=AirBeB,mean)



AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==1]<- 1
AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==2]<- 1
AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==3]<- 1
AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==4]<- 1
AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==5]<- 1
AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==6]<- 2
AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==8]<- 2
AirBeB$bathrooms[is.na(AirBeB$bathrooms)==T & AirBeB$accommodates==16]<- 3
  

# bed type
str(AirBeB$bed_type)
levels(AirBeB$bed_type)  
table(AirBeB$bed_type)  
  

#canellation policy
str(AirBeB$cancellation_policy)
table(AirBeB$cancellation_policy)  


#cleaning fee
str(AirBeB$cleaning_fee)
table(AirBeB$cleaning_fee)



# city

str(AirBeB$city)
table(AirBeB$city)


# description
str(AirBeB$description)
AirBeB$description[1]

#firt review
str(AirBeB$first_review)
table(AirBeB$first_review)
#posso trasformale ain date e ceracre di capire da quanti anni è disponibile


#host has a profile pic
str(AirBeB$host_has_profile_pic)
table(AirBeB$host_has_profile_pic)
summary(AirBeB$host_has_profile_pic)
AirBeB$description[is.na(AirBeB$host_has_profile_pic)==T]


#host identify verified
levels(AirBeB$host_identity_verified)
summary(AirBeB$host_identity_verified)
table(AirBeB$host_identity_verified)

colnames(AirBeB)
#host since  anche quetsa potrbebe essere una data
str(AirBeB$host_since)
summary(AirBeB$host_since)

#host response rate
str(AirBeB$host_response_rate)
summary(AirBeB$host_response_rate)


#instatan bookable
str(AirBeB$instant_bookable)
table(AirBeB$instant_bookable)


#last reviw
str(AirBeB$last_review)
#data anche questa
summary(AirBeB$last_review)

#latitude
str(AirBeB$latitude)
summary(AirBeB$latitude)

#longitude
summary(AirBeB$longitude)

#name
str(AirBeB$name)

#Neighbourhoud
str(AirBeB$neighbourhood)
levels(AirBeB$neighbourhood)

#number of reviews
str(AirBeB$number_of_reviews)
aggregate(price~neighbourhood,data=AirBeB,mean)
summary(AirBeB$number_of_reviews)
m1<-aggregate(longitude~neighbourhood,data=AirBeB,mean)
m2<-aggregate(latitude~neighbourhood,data=AirBeB,mean)
m1<-cbind(m1,m2$latitude)
ass1<-AirBeB$longitude[is.na(AirBeB$neighbourhood)]
ass2<-AirBeB$latitude[is.na(AirBeB$neighbourhood)]
ass1<-cbind(ass1,ass2,NA)

#Review score rating
str(AirBeB$review_scores_rating)
summary(AirBeB$review_scores_rating)

#zipcode
str(AirBeB$zipcode)


#bedrooms
str(AirBeB$bedrooms)
summary(AirBeB$bedrooms)
sum(is.na(AirBeB$bedrooms))
aggregate(bedrooms~as.factor(accommodates),data=AirBeB,mean)
aggregate(bedrooms~property_type,data=AirBeB,median)
table(AirBeB$accommodates[is.na(AirBeB$bedrooms)==T])
(AirBeB$property_type[is.na(AirBeB$bedrooms)==T])
AirBeB$bed_type[is.na(AirBeB$bedrooms)==T]
AirBeB$bedrooms[is.na(AirBeB$bedrooms)==T & AirBeB$bed_type =="Pull-out Sofa"]<-0
AirBeB$bedrooms[is.na(AirBeB$bedrooms)==T & AirBeB$bed_type =="Futon"]<-0
AirBeB$bedrooms[is.na(AirBeB$bedrooms)==T & (AirBeB$accommodates==1 | AirBeB$accommodates==2| AirBeB$accommodates==3| AirBeB$accommodates==4)]<- 1
AirBeB$bedrooms[is.na(AirBeB$bedrooms)==T & AirBeB$accommodates==6]<-2



#beds
summary(AirBeB$beds)
AirBeB$bed_type[is.na(AirBeB$beds)==T]
AirBeB$beds[is.na(AirBeB$beds)==T & AirBeB$bed_type =="Pull-out Sofa"]<-0
aggregate(beds~as.factor(accommodates),data=AirBeB,mean)
table(AirBeB$accommodates[is.na(AirBeB$beds)==T])
AirBeB$beds[is.na(AirBeB$beds)==T & (AirBeB$accommodates==1 | AirBeB$accommodates==2 | AirBeB$accommodates==3)]<-1
AirBeB$description[is.na(AirBeB$beds)==T & AirBeB$accommodates==3]
AirBeB$description[is.na(AirBeB$beds)==T & AirBeB$accommodates==5]
AirBeB$beds[is.na(AirBeB$beds)==T & (AirBeB$accommodates==4 | AirBeB$accommodates==5) ]<-2
AirBeB$beds[is.na(AirBeB$beds)==T & AirBeB$accommodates==6]<-3
AirBeB$beds[is.na(AirBeB$beds)==T & AirBeB$accommodates==8]<-4
AirBeB$beds[is.na(AirBeB$beds)==T & AirBeB$accommodates==16]<-7

AirBeB<- AirBeB[is.na(AirBeB$neighbourhood)==F,]
colnames(AirBeB)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
AirBeB$amenities<-as.character(AirBeB$amenities)
docs <- Corpus(VectorSource(AirBeB$amenities[1:30000]))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x) )
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "#")
docs<- tm_map(docs, toSpace, ",")

# Convert the text to lower case
#docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
#docs <- tm_map(docs, stripWhitespace)
#creo la matrice di parole che poi nalaizza
dtm <- TermDocumentMatrix(docs,control = list(tolower =F))
m <- as.matrix(dtm)
dim(m2)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
A<-rep(NA,411)
A<-d[1:205,1]
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#per i secondi 30000

docs2 <- Corpus(VectorSource(AirBeB$amenities[30000:7000]))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x) )
docs2 <- tm_map(docs2, toSpace, "/")
docs2 <- tm_map(docs2, toSpace, "@")
docs2 <- tm_map(docs2, toSpace, "\\|")
docs2 <- tm_map(docs2, toSpace, "#")
docs2 <- tm_map(docs2, toSpace, ",")

# Convert the text to lower case
#docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs2 <- tm_map(docs2, removeNumbers)
# Remove english common stopwords
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
# Remove punctuations
docs2 <- tm_map(docs2, removePunctuation)
# Eliminate extra white spaces
#docs <- tm_map(docs, stripWhitespace)
#creo la matrice di parole che poi nalaizza
dtm2 <- TermDocumentMatrix(docs2,control = list(tolower =F))
#?TermDocumentMatrix
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 20)
A[206:411]<-d2[1:205,1]
A<-unique(A)
new<-matrix(0,ncol=length(A),nrow = nrow(AirBeB))
colnames(new)<-A
for (i in 1:length(A)){
  new[,i]<-as.factor(new[,i])
  levels(new[,i])<-c("0","1")
  new[,i]<-"0"
  ind<-grep(A[i],AirBeB$amenities)
  new[ind,i]<-"1"
}
new<-as.data.frame(new)


#per description
AirBeB$description<-as.character(AirBeB$description)

vect<-seq(1,70001,by=1000)
B<-matrix(NA,nrow=50,ncol=(length(vect)-1))

for(i in 1:(length(vect)-1) ){
  
  docs1 <- Corpus(VectorSource(AirBeB$description[vect[i]:vect[i+1]]))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs1 <- tm_map(docs1, toSpace, "/")
  docs1 <- tm_map(docs1, toSpace, "@")
  docs1 <- tm_map(docs1, toSpace, "\\|")
  docs1 <- tm_map(docs1, toSpace, "#")
  docs1 <- tm_map(docs1, toSpace, ",")
  # Convert the text to lower case
  #docs1 <- tm_map(docs1, content_transformer(tolower))
  # Remove numbers
  docs1 <- tm_map(docs1, removeNumbers)
  # Remove english common stopwords
  docs1 <- tm_map(docs1, removeWords, stopwords("english"))
  # Remove punctuations
  docs1 <- tm_map(docs1, removePunctuation)
  # Eliminate extra white spaces
  #docs1 <- tm_map(docs1, stripWhitespace)
  #creo la matrice di parole che poi nalaizza
  dtm1 <- TermDocumentMatrix(docs1,control = list(tolower =F,stopwords=T))
  m1<- as.matrix(dtm1)
  v1 <- sort(rowSums(m1),decreasing=TRUE)
  d1 <- data.frame(word = names(v1),freq=v1)
  
  set.seed(1234)
  #wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
  #max.words=200, random.order=FALSE, rot.per=0.35, 
  #colors=brewer.pal(8, "Dark2"))
  
  B[,i]<-as.character( d1[1:50,1])
}

c<-c()
vect2<-seq(1, (70*50)+1,by=50)
for(i in 1:(length(vect2)-1)){
  c[vect2[i]:(vect2[i+1]-1)]<-B[,i]
}

c<-unique(c)
new2<-matrix(NA,ncol=length(c),nrow = nrow(AirBeB))
colnames(new2)<-c
for (i in 1:length(c)){
  new2[,i]<-as.factor(new2[,i])
  levels(new2[,i])<-c("0","1")
  new2[,i]<-"0"
  ind<-grep(c[i],AirBeB$description)
  new2[ind,i]<-"1"
}
new2<-as.data.frame(new2)
sum(new2$apartment=="1")
dummy<-cbind(new,new2)

summary(AirBeB$host_since)
sum(is.na(AirBeB$host_since))

summary(AirBeB$first_review)
sum(is.na(AirBeB$first_review))

summary(AirBeB$last_review)
sum(is.na(AirBeB$last_review))



date<-matrix(NA,ncol=2018-2009,nrow = nrow(AirBeB))
h<-paste("last_review_year",seq(2009,2017,by=1),sep = "_")
colnames(date)<-h
vet<-seq(2009,2017,by=1)
for (i in 1:ncol(date)){
  ici<-grep(vet[i],AirBeB$last_review)
  date[,i]<-as.factor(date[,i])
  levels(date[,i])<-c("0","1")
  date[,i]<-"0"
  date[ici,i]<-"1"
}
date<-as.data.frame(date)
date$last_review_year_na<-NA
date$last_review_year_na<-as.factor(date$last_review_year_na)
levels(date$last_review_year_na)<-c("0","1")
date$last_review_year_na<-"0"
date$last_review_year_na[is.na(AirBeB$last_review==T)]<-"1"
table(date$last_review_year_na)



AirBeB$host_since<-as.Date(AirBeB$host_since)


date1<-matrix(NA,ncol=2018-2008,nrow = nrow(AirBeB))
h<-paste("host_since",seq(2008,2017,by=1),sep = "_")
colnames(date1)<-h
vet<-seq(2008,2017,by=1)
for (i in 1:ncol(date1)){
  ici<-grep(vet[i],AirBeB$host_since)
  date1[,i]<-as.factor(date1[,i])
  levels(date1[,i])<-c("0","1")
  date1[,i]<-"0"
  date1[ici,i]<-"1"
}
date1<-as.data.frame(date1)
sum(is.na(AirBeB$host_since))
date1$host_since_na<-NA
date1$host_since_na<-as.factor(date1$host_since_na)
levels(date1$host_since_na)<-c("0","1")
date1$host_since_na<-"0"
date1$host_since_na[is.na(AirBeB$host_since)==T]<-"1"
table(date1$host_since_na)




AirBeB$first_review<-as.Date(AirBeB$first_review)
plot(AirBeB$first_review,seq(1:length(AirBeB$first_review)))
grep(2006,AirBeB$first_review)

date2<-matrix(NA,ncol=2018-2008,nrow = nrow(AirBeB))
h<-paste("first_review",seq(2008,2017,by=1),sep = "_")
colnames(date2)<-h
vet<-seq(2008,2017,by=1)
for (i in 1:ncol(date2)){
  ici<-grep(vet[i],AirBeB$first_review)
  date2[,i]<-as.factor(date2[,i])
  levels(date2[,i])<-c("0","1")
  date2[,i]<-"0"
  date2[ici,i]<-"1"
}
date2<-as.data.frame(date2)
sum(is.na(AirBeB$first_review))
date2$first_review_na<-NA
date2$first_review_na<-as.factor(date2$first_review_na)
levels(date2$first_review_na)<-c("0","1")
date2$first_review_na<-"0"
date2$first_review_na[is.na(AirBeB$first_review)==T]<-"1"
table(date2$first_review_na)

Date<-cbind(date,date1,date2)

AirBeB<-AirBeB[,-c(1,5,12,13,14,15,16,17,19,22,25,26,27,30)]
sum(is.na(AirBeB))

AirBeB<-cbind(AirBeB,dummy,Date)


colnames(AirBeB)

table(AirBeB$Dogs)
library(caret)
vars_vz<-nearZeroVar(AirBeB, 
            freqCut = 95/5, 
            uniqueCut = 10, 
            saveMetrics = TRUE)
ind<-colnames(AirBeB)[vars_vz$nzv ==F]
AirBeB<-AirBeB[,ind]

#HDI



#sample splitting 

install.packages("hdi")
library(hdi)
set.seed(123)
X<-model.matrix(lm(log_price~0+.,data = AirBeB) )
x<-as.data.frame(X)
colnames(X)
y<-AirBeB$log_price
dim(X)
length(y)

fit <- multi.split(x=X[1:10000,], y=y[1:10000], B=10, fraction=0.5, ci=TRUE, ci.level = 0.95)
fit
# 'names' attribute [552] must be the same length as the vector [527]



#stability selection
#install.packages("stabs")
library(stabs)

fit <- stabsel(x = X, y = y, fitfun = lars.lasso, cutoff = 0.75,
               PFER = 6, assumption ="none") 
length(fit$selected)
predict(fit, type="coefficients")

variabless<-fit$selected
length(variabless)
sum
l.model<-lm(log_price~0+.,data = AirBeB)
(sum[,4] <=(0.05/length(sum[,1])))
sum<-summary(l.model)$coefficients

length(sum[,4]<=0.05)
variablelmbon<-which((sum[,4]<=(0.05/length(sum[,1]))))

#knockoff
result = knockoff.filter(X, y, fdr = 0.1)
print(result)
fdp(result$selected)
