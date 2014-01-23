###### METHOD 1: Loop over files, use shell to extract column of interest and then R to make frequency table for each 
files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)
runtime1 = system.time({
tables1 = sapply(1:length(files),function(i){
	filename = files[i]
	cat('currently working on file',filename,'\n')
	filepath = paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',filename)
	# need to find what column we want, since it's annoyingly not the same in each file 
	if(i<=21){
		col_number = 15
	}else{
		col_number = 45 # this gets the "ARR_DEL15" column; the ARR_DELAY column values make no sense! but the ARR_DEL15 values *seem* reasonable...
	}
	shell_command = paste('export LANG=C; cut -f',col_number,'-d,',filepath)
	delays = system(shell_command,intern=TRUE)
	table(delays[-1]) # -1 removes the column header 
})

# need to change names of the tables to all be integers (they are in 3.00 form for the later years)
# also need to makes sure NA and 'NA' are called the same thing (some were characters and some were actually NAs; this was causing problems when merging tables)
for(i in 22:length(tables1)){
	names(tables1[[i]]) = as.character(as.integer(names(tables1[[i]])))
	names(tables1[[i]])[is.na(names(tables1[[i]]))]='NA'
}

merged_table1 = mergeFreqTable(tables1,na.rm=T)

mean1 = meanFreqTable(merged_table1)
median1 = medianFreqTable(merged_table1)
sd1 = sdFreqTable(merged_table1,mean1)

})


###### METHOD 2: Loop over files, use shell to make frequency table for each: 

runtime2 = system.time({
tables2 = sapply(1:length(files),function(i){
	filename = files[i]
	cat('currently working on file',filename,'\n')
	filepath = paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',filename)
	# need to find what column we want, since it's annoyingly not the same in each file 
	if(i<=21){
		col_number = 15
	}else{
		col_number = 45 # this gets the "ARR_DEL15" column; the ARR_DELAY column values make no sense! but the ARR_DEL15 values *seem* reasonable...
	}
	shell_command = paste('export LANG=C; cut -f',col_number,'-d,',filepath,'|sort|uniq -c')
	t = system(shell_command,intern=TRUE)
	
	# now, convert this to a table (currently just has strings with both delay and count)
	del = unlist(sapply(1:length(t),function(i){
		split = strsplit(t[i],' ')[[1]]
			is_num = grepl('^-|^[0-9]',split) # regular expression weeds out the blank/NA column and the ARR-DELAY header 

		if(sum(is_num)==2){ # this is 2 if and only if this line of the frequency table actually had a delay on it (not NA or a column header)
			delay = as.numeric(split[is_num][2])
			n = split[is_num][1]
			return(c(delay,n))
		}else{
			return(NULL)
		}
	}))
	
	# we have vector of length (2*number of unique delay times); every other entry is the delay and the count
	delays = del[seq(from=1,to=length(del)-1,by=2)]
	counts = as.integer(del[seq(from=2,to=length(del),by=2)])
	names(counts)=delays
	counts
})

merged_table2 = mergeFreqTable(tables2)
mean2 = meanFreqTable(merged_table2) 
median2 = medianFreqTable(merged_table2) 
sd2 = sdFreqTable(merged_table2,mean2) 


})


##### Method 3: PostgreSQL Database 
# create table for first format of data (1987-2007)
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays1987to2007(carrier CHARACTER(10), arrdelay FLOAT,origin CHARACTER(3),dest CHARACTER(3));"')

# run this in the shell. Can't use system (at least I don't know how we could) since we need both single and double quotes in the shell command
cat 19*.csv 2000.csv 2001.csv 2002.csv 2003.csv 2004.csv 2005.csv 2006.csv 2007.csv | cut -f 9,15,17,18 -d,| grep -v ArrDelay | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays1987to2007 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';" # took about 12 minutes 

# create table for second format of data (2008-2012) (had to use second table since order of columns differs in later dataset, and cut can't rearrange columns)
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays2008to2012(carrier CHARACTER(10),origin CHARACTER(3),dest CHARACTER(3), arrdelay FLOAT);"')

# run this in the shell: 
cat 2008*.csv 2009*.csv 2010*.csv 2011*.csv 2012*.csv  | cut -f 9,15,25,45 -d,| grep -v ARR_DEL15 | sed 's/,$/,NA/g' | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays2008to2012 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';" # took 10 minutes 

runtime = 22*60+system.time({
# mean - need to do weighted average of the two groups
n1 = dbGetQuery(con, "select count(*) from delays1987to2007")
m1 = dbGetQuery(con, "select avg(arrdelay) from delays1987to2007")
n2 = dbGetQuery(con, "select count(*) from delays2008to2012")
m2 = dbGetQuery(con, "select avg(arrdelay) from delays2008to2012")
mean3 =(n1*m1+n2*m2)/(n2+n1)

# will need to get a frequency table for sd and median -- no built in command for median, and no easy way to merge sd calculated separately on the two databases.  
u1 = dbGetQuery(con, "SELECT DISTINCT arrdelay FROM delays1987to2007")
u2 = dbGetQuery(con, "SELECT DISTINCT arrdelay FROM delays2008to2012")
t1 = dbGetQuery(con, "SELECT count(*) FROM delays1987to2007 GROUP BY arrdelay")
t2 = dbGetQuery(con, "SELECT count(*) FROM delays2008to2012 GROUP BY arrdelay")

tab1 = t1[,1]
names(tab1) = u1[,1]
tab2 = t2[,1]
names(tab2) = u2[,1]

merged_table3 = mergeFreqTable(list(tab1,tab2),na.rm=T)

median3 = medianFreqTable(merged_table3)
sd3 = sdFreqTable(merged_table3,mean3)
})








dbGetQuery(con, "select count(*) from delays1987to2007")
dbGetQuery(con, "select avg(arrdelay) from delays1987to2007 GROUP BY carrier")
dbGetQuery(con, "SELECT DISTINCT carrier FROM delays1987to2007")
dbGetQuery(con, "select count(*) from delays2008to2012")
dbGetQuery(con, "select avg(arrdelay) from delays2008to2012 GROUP BY carrier")
dbGetQuery(con, "SELECT DISTINCT carrier FROM delays2008to2012")
dbGetQuery(con, "select count(*) from delays2008to2012 GROUP BY carrier")
dbGetQuery(con, "select count(*) from delays2008to2012 WHERE origin = 'LAX'")
dbGetQuery(con, "select count(*) from delays2008to2012 WHERE origin = 'ATL' GROUP BY carrier")
dbGetQuery(con, "SELECT DISTINCT carrier FROM delays2008to2012 WHERE origin = 'ATL'")

dbGetQuery(con, "select avg(arrdelay) from delays2008to2012 WHERE dest = 'SFO' GROUP BY carrier")
dbGetQuery(con, "SELECT DISTINCT carrier FROM delays2008to2012 WHERE dest = 'SFO'")
dbGetQuery(con, "select avg(arrdelay) from delays2008to2012 GROUP BY carrier")


dbGetQuery(con, "select avg(arrdelay) from delays2008to2012 WHERE dest = 'SFO'")
dbGetQuery(con, "select avg(arrdelay) from delays2008to2012 WHERE dest = 'OAK'")
dbGetQuery(con, "select avg(arrdelay) from delays2008to2012 WHERE dest = 'SJC'")

##### make plots of summary statistics and times

par(mfrow=c(1,3))
plot(c(mean1,mean2,mean3),pch=19,cex=1.2,xaxt='n',xlab='',main='Mean Arrival Delay',ylab='Mean Arrival Delay (minutes)')
axis(1,c(1,2,3),c('Method 1','Method 2','Method 3'))
plot(c(median1,median2,median3),pch=19,cex=1.2,xaxt='n',xlab='',main='Median Arrival Delay',ylab='Median Arrival Delay (minutes)')
axis(1,c(1,2,3),c('Method 1','Method 2','Method 3'))
plot(c(sd1,sd2,sd3),pch=19,cex=1.2,xaxt='n',xlab='',main='Std Dev of Arrival Delay',ylab='Std Dev of Arrival Delay (minutes)')
axis(1,c(1,2,3),c('Method 1','Method 2','Method 3'))


##### functions for calculating summmary statistics from tables 

# function to merge tables (we have a table from each year and want a combined one with values summed across all years)
mergeFreqTable = function(tt,na.rm=FALSE){
	# tt needs to be a list of tables to be merged
	# returns a named integer vector; names are delay times and value are counts 
	# na.rm deterines in NAs are included in the final table
	
	# first, find all the unique values in all the tables combined
	all_names = unlist(lapply(tt,function(t){names(t)}))
	unique_names = unique(all_names)
	
	# loop over all possible values; within that loop over all years and extract corresponding counts. Then sum them.  
	merged = sapply(unique_names,function(delay){
		sum(sapply(tt,function(t){t[delay]}),na.rm=T)
	})
	# remove NAs if desired
	if(na.rm){
		w = which(is.na(names(merged)))
		merged = merged[-w]
	}
	merged
}

# next, need functions for mean, median, and sd from freq table
meanFreqTable = function(t){
	# takes a table (or named vector) and finds the mean of all the names, assuming each name is replicated the number of times corresponding to the entry for that name 
	sum(as.integer(names(t))*t)/sum(t)
}

medianFreqTable = function(t,debug=F){
	# takes a table (or named vector) and finds the median of all the names, assuming each name is replicated the number of times corresponding to the entry for that name 
	# this won't get the right answer if the median need to be an average of two values. (This fxn will return the lower of those 2 numbers). However, given the strong tendency for delays to be near 0, it's exceptionally unlikely that for this application this averaging will be needed.  I also checked with the debug option in my function, and the cumulative sum at the value before the median is reached is much less than half, and the cumulative sum right after the median is reached is much more than half. So, it's not an issue for these data.   
	n = sum(t)
	half = ceiling(n/2)
	# sort the names; we will start from the lowest and keep a cumulative sum of counts until we reach the midway point.  
	sorted_names = sort(as.integer(names(t)))
	cumul_sum = 0
	i = 1
	while(cumul_sum<half){
		if(debug){cat('cumulative sum so far is:',cumul_sum,'\n')}
		current_number = sorted_names[i]
		cumul_sum = cumul_sum + t[as.character(current_number)]
		i = i+1
	}
	if(debug){cat('cumulative sum after final bin is:',cumul_sum,'\n')}
	current_number
}

sdFreqTable = function(t,mean){
		# takes a table (or named vector) and its mean, and finds the sample SD (MLE, biased estimate) of all the names, assuming each name is replicated the number of times corresponding to the entry for that name 
	var_mle = sum(t*(as.integer(names(t))-mean)**2)/sum(t)
	sd_mle = sqrt(var_mle)
	sd_mle
}


########### save results 
info = list(sessionInfo(),Sys.info())
names(info) = c('sessionInfo','systemInfo')

runtimes = list(runtime1,runtime2,runtime3)
means = list(mean1,mean2,mean3)
medians = list(median1,median2,median3)
sds = list(sd1,sd2,sd3)

save(info,means,medians,sds,file='~/Documents/STA250Winter2014/results_final.rda')