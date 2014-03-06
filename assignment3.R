################## Load libs
library(randomForest)
library(e1071)

################### read data 
d = read.csv('~/Downloads/train-sample.csv')

################### fix feature types 
d$OwnerUserId = as.factor(d$OwnerUserId)
d$BodyMarkdown = as.character(d$BodyMarkdown)
################### Feature Creation
# manually inspect closed posts to see what characteristics they seem to have:
d[d$Closed=='closed',][872,]

# first, just try to predict closed vs. open
d$Closed = as.factor(ifelse(d$OpenStatus=='open','open','closed'))

# nchar in title
d$TitleLength = nchar(as.character(d$Title))

# nchar in body
d$BodyLength = nchar(as.character(d$BodyMarkdown))

# has 'u' or 'i' or 'lol' - sloppy language elements 
d$sloppyLanguage = as.factor(ifelse(grepl(' u | i | lol',as.character(d$BodyMarkdown)),'yes','no'))
table(d$sloppyLanguage,d$Closed)

# hour posted:
hp = strsplit(as.character(d$PostCreationDate),' ')
hp2 = strsplit(sapply(hp,'[',2),':')
hp3 = sapply(hp2,'[',1)
d$hourPosted = as.factor(hp3)

# how long account has been open: owner creation date minus post creation date
acc1 = strsplit(as.character(d$OwnerCreationDate),' ')
acc2 = strsplit(sapply(acc1,'[',1),'/')
acc_year = as.integer(sapply(acc2,'[',3))
post1 = strsplit(as.character(d$PostCreationDate),' ')
post2 = strsplit(sapply(post1,'[',1),'/')
post_year = as.integer(sapply(post2,'[',3))
d$YrsAcctOpen = post_year-acc_year

# number of tags
d$numTags = sapply(1:nrow(d),function(i){
	tagged = sum(!d[i,9:13]=='')
})


# number of words and length of each word 
sp = strsplit(d$BodyMarkdown,' ')
d$numWords = sapply(sp,length)
d$wordLength = sapply(sp,function(i){
	mean(nchar(i))
})

# did the post contain a swear word?  
d$swear = grepl('damn|fuck|shit',as.character(d$BodyMarkdown))

# nchar in Tag1
d$Tag1=as.character(d$Tag1)
d$tagLength = sapply(1:nrow(d),function(i){
	nchar(d$Tag1[i])
})

# is Tag1 in the body? May be off topic if not:
d$tagInBody = sapply(1:nrow(d),function(i){
	grepl(d$Tag1[i],d$BodyMarkdown[i])
})

d$isPolite = grepl('please',as.character(d$BodyMarkdown))

d$propCaps = sapply(1:nrow(d),function(i){
	a = d$BodyMarkdown[i]
	g_up = gregexpr('[A-Z]',a)
	g_low = gregexpr('[a-z]',a)
	l_up =length(regmatches(a,g_up)[[1]])
	l_low =length(regmatches(a,g_low)[[1]])
	l_up/(l_up+l_low)
})


# create a test set since the supposed one on the Kaggle website doesn't exist! 
test_indices = sample(1:nrow(d),15000)
test = d[test_indices,]
dfull = d
d = d[-test_indices,]
dim(d)
dim(test)

# the following features need to be created just within the training set, because they depend on the labels of other instances  

# idea: see number of posts by that user that were closed (note: we really should only look at post *before* the current post, since that's the realistic situation we'd be in for predicting for a new post, but I'll come back to this if I have time to mess around with the dates later.)
tt = table(d$OwnerUserId,d$Closed)
head(tt)
tt[c('9','9'),]
d$numClosed = tt[as.character(d$OwnerUserId),1]

d$otherClosed = sapply(1:nrow(d),function(i){
	max(0,d$numClosed[i]-1)
})

# posting frequency: 
num_post_tab = table(d$OwnerUserId)
d$numPosts = num_post_tab[as.character(d$OwnerUserId)]-1

# prior knowledge about the poster...either 
d$prior = sapply(1:nrow(d),function(i){
	if(d$numClosed[i]>=2){return('bad')}
	if(d$numPosts[i]==1){return('unknown')}else{
		return('good')
	}
})
d$prior=as.factor(d$prior)



######################### Fitting Models
# have to subsample the training set in order to fit this is a reasonable time on my slow machine: 
dd = d[sample(1:nrow(d),10000),]
# method 1: random forests
r = randomForest(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,dd,na.action=na.omit)
r
i = importance(r)
d1 = data.frame(variable = rev(rownames(i)[order(i)]),importance=rev(sort(i)))
library(xtable)
xtable(d1)
xtable(r$confusion)
p = predict(r,dd)
actual = dd[names(p),'Closed']
length(actual)
conf = table(p,actual)
conf
(conf[1,1]+conf[2,2])/sum(conf) # 35% error, even withouyt cv 


# method 2: SVM
s = svm(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,dd,cross=10)
s
names(s)
s$tot.accuracy

table(dd$Closed) # balanced 

p = predict(s,dd)
length(p)
actual = dd[names(p),'Closed']
length(actual)
conf = table(p,actual)
conf
(conf[1,1]+conf[2,2])/sum(conf) 


# method 3: LDA
l = lda(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,dd)

p = predict(l,dd)
confusion = table(p[[1]],dd$Closed)
1-(confusion[1,1]+confusion[2,2])/sum(confusion) 

# method 4: QDA
q = qda(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,dd)

p = predict(q)

confusion = table(p[[1]],dd$Closed)
1-(confusion[1,1]+confusion[2,2])/sum(confusion) 


p1 =predict(q,dd) 
length(p1[[1]])

table(p1[[1]],dd$Closed)
sum(p1[[1]]==dd$Closed,na.rm=T)

# method 5: knn
install.packages('kknn')
library(kknn)
k = train.kknn(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,data=dd,na.action=na.omit)
k
k = kknn(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,dd,dd,k=11)

names(k)
f = fitted.values(k)
f1 = fitted.values(k)
length(f)
length(f1)
all.equal(f,f1)
tab = table(f,dd$Closed)
sum(tab[1,1],tab[2,2])/sum(tab)
p = predict(k,dd)
p1 = predict(k,dd)
length(p)
length(p1)
all.equal(p,p1) # predict doesn't work for kknn
all.equal(p,f)# true 

# method 6: classification tree 
library(rpart)
r = rpart(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,dd)
p = predict(r,dd)
s = sapply(1:10000,function(i){which.max(p[i,])})
p = names(s)
conf = table(dd$Closed,p)
1-(conf[1,1]+conf[2,2])/sum(conf) 
############ measure predictions on test set: 
# the following variables need to be created using the test set, since they depend on info from other instances and, for the training set, they needed to be created within the training set without using info from the test set.  
tt = table(dfull$OwnerUserId,dfull$Closed)
test$numClosed = tt[as.character(test$OwnerUserId),1]

test$otherClosed = sapply(1:nrow(test),function(i){
	max(0,test$numClosed[i]-1)
})

# posting frequency: 
num_post_tab = table(dfull$OwnerUserId)
test$numPosts = num_post_tab[as.character(test$OwnerUserId)]-1

# prior knowledge about the poster...either 
test$prior = sapply(1:nrow(test),function(i){
	if(test$numClosed[i]>=2){return('bad')}
	if(test$numPosts[i]==1){return('unknown')}else{
		return('good')
	}
})
test$prior=as.factor(test$prior)

# RF
pred = predict(r,test)
actual = test$Closed
conf = table(actual,pred)
1-(conf[1,1]+conf[2,2])/sum(conf)

# SVM
pred = predict(s,test)
actual = test[names(pred),'Closed']
conf = table(actual,pred)
1-(conf[1,1]+conf[2,2])/sum(conf)

# LDA
p = predict(l,test)
conf = table(test$Closed,p[[1]])
1-(conf[1,1]+conf[2,2])/sum(conf) 

# QDA
p = predict(q,test)
conf = table(test$Closed,p[[1]])
1-(conf[1,1]+conf[2,2])/sum(conf) 

# KNN 
k = kknn(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+sloppyLanguage+hourPosted+YrsAcctOpen+numTags+swear+numWords+tagLength+isPolite+wordLength+tagFreq+otherClosed+numPosts+prior+propCaps,dd,test,k=11)
pred = fitted.values(k)
c = complete.cases(test)
actual = test$Closed[c]
conf = table(actual,pred)
1-(conf[1,1]+conf[2,2])/sum(conf)

# CART
p = predict(r,test)
s = sapply(1:15000,function(i){which.max(p[i,])})
p = names(s)
conf = table(test$Closed,p)
1-(conf[1,1]+conf[2,2])/sum(conf) 

# make a graph?  
errs = c(27,29.81,32.42,36.38,36.1,32.9)
methods = c('RF','SVM','LDA','QDA','KNN','CART')
plot(1:6 ,sort(errs),xaxt='n',xlab='',pch=19,ylab='Test Set Percentage Error',main='Test Set Errors of 6 Methods')
axis(1,at=1:6,labels=methods[order(errs)])
