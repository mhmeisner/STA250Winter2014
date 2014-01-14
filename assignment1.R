d = read.csv('~/Downloads/1990.csv.bz2')


# takes about 20 seconds 
system.time({
del = system('bzip2 -dc /Users/matthewmeisner/Downloads/1990.csv.bz2|cut -f 15 -d,',intern=TRUE) 
ta = table(del[-1]) # the -1 removes teh "ArrDelay" column header -- we don't want that in the table
})
head(ta)
length(del)
length(ta) # 740 
del1 = as.numeric(del[-1])
class(del1)
sum(is.na(del1))
mean(as.numeric(del[-1]),na.rm=T)
head(del)
head(del[-1])

# need fxn to get mean, median, and sd from the table 
ta[1:10]
sort(as.numeric(names(ta)))
head(ta)
n = unlist(sapply(1:length(ta),function(i){
	rep(as.numeric(names(ta)[i]),ta[i])
}))
> length(n)
[1] 5270893 # this is right -- one less than line count which also included column names 
mean(n,na.rm=T)
median(n,na.rm=T)
sd(n,na.rm=T) # sd looks about right given the graph 
plot(density(n,na.rm=T),xlim=c(-100,100))

# will need to loop over files, but can just find the mean of each, and then take weighted average of those weighted by the 


# but, the median and sd will probably need to all be done as one merged list of all of the delay values 

# takes about 85 seconds 
system.time({t = system('bzip2 -dc /Users/matthewmeisner/Downloads/1990.csv.bz2|cut -f 15 -d,|sort|uniq -c',intern=TRUE) })
t = system('bzip2 -dc /Users/matthewmeisner/Downloads/1990.csv.bz2|cut -f 15 -d,|sort|uniq -c',intern=TRUE) 
class(t)
head(t)
length(t) # 740
tail(t)
grepl('[0-9]',strsplit(t[736],' ')[[1]])
# will need to convert this to a table

del = unlist(sapply(1:length(t),function(i){
	split = strsplit(t[i],' ')[[1]]
	is_num = grepl('[0-9]',split)
	if(sum(is_num)==2){
		delay = as.numeric(split[is_num][2])
		n = split[is_num][1]
		return(rep(delay,n))
	}else{
		return(NULL)
	}
}))
length(del) # NAs removed...

mean(del) # identical answer 
median(del) # same 
sd(del) # same


# now try all files! 
del = system('bzip2 -dc /Users/matthewmeisner/Downloads/Delays1987_2013.tar.bz2|cut -f 15 -d,',intern=TRUE) 


# or, could loop through the .csv files:
del = system('cut -f 15 -d, /Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv',intern=TRUE) 
head(del)
length(del)