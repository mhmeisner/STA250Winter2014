################## Load libs
library(randomForest)
library(e1071)

d = read.csv('~/Downloads/train-sample.csv')
dim(d)
###################
d$OwnerUserId = as.factor(d$OwnerUserId)
d$BodyMarkdown = as.character(d$BodyMarkdown)
################### Feature Creation
# first, just try to predict closed vs. open
d$Closed = as.factor(ifelse(d$OpenStatus=='open','open','closed'))

# nchar in title
d$TitleLength = nchar(as.character(d$Title))

# nchar in body
d$BodyLength = nchar(as.character(d$BodyMarkdown))

# has 'u' 
d$cannotSpellYou = as.factor(ifelse(grepl(' u ',as.character(d$BodyMarkdown)),'yes','no'))
table(d$cannotSpellYou,d$Closed)

# consider other sloppy language elements

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
	tagged = sum(!d[i,c('Tag1','Tag2','Tag3','Tag4','Tag5')]=='')
})


# number of words and length of each word 
sp = strsplit(d$BodyMarkdown,' ')
d$numWords = sapply(sp,length)


# may want to bin some of the variables 

# manually inspect closed posts to see what characteristics they seem to have:
d[d$Closed=='closed',][8922,]




w = grepl(' i | u ',as.character(d$BodyMarkdown))
table(d$Closed[w])

w = grepl('!',as.character(d$BodyMarkdown))
w = grepl('?!',as.character(d$BodyMarkdown))
table(d$Closed[w])



d$swear = grepl('damn|fuck|shit',as.character(d$BodyMarkdown))


######################### Fitting Models
dd = d[sample(1:nrow(d),10000),]
r = randomForest(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+cannotSpellYou+hourPosted+YrsAcctOpen+numTags+swear+numWords,dd)
r
importance(r) # hourPosted helped a lot 
s = svm(Closed~ReputationAtPostCreation+OwnerUndeletedAnswerCountAtPostTime+TitleLength+BodyLength+cannotSpellYou+hourPosted+YrsAcctOpen+numTags,dd,cross=10)
s
names(s)
s$tot.accuracy

table(dd$Closed) # balanced 