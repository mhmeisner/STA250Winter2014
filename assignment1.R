
files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)
runtime = system.time({
tables = sapply(1:length(files),function(i){
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
for(i in 22:length(tables)){
	names(tables[[i]]) = as.character(as.integer(names(tables[[i]])))
	names(tables[[i]])[is.na(names(tables[[i]]))]='NA'
}

merged_table = mergeFreqTable(tables,na.rm=T)

mean = meanFreqTable(merged_table)
median = medianFreqTable(merged_table)
sd = sdFreqTable(merged_table,mean)

})

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
		w = which(names(merged)=='NA')
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
	# this won't get the right answer if the median need to be an average of two values. (This fxn will return the lower of those 2 numbers). However, given the strong tendency for delays to be near 0, it's exceptionally unlikely that for this application this averaging will be needed.  I also checked with the debug option in my function, and the cumulative sum at the value before the median is reached is much less than half, and the cumulative sum right after the median is reached is much more than half. So, it's not an issue for these data. For the next submission, I'll try to have a better general function that works on any table, even if this averaging were needed.  
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

# get means by year! 
means_1987_2007 = sapply(1:21,function(year){
	meanFreqTable(mergeFreqTable(tables[year],na.rm=T))
})
monthly_files_2008_2012 = sapply(2008:2012,function(y){grepl(y,files)})
means_2008_2012 = sapply(1:5,function(y){
	w = which(monthly_files_2008_2012[,y])
	meanFreqTable(mergeFreqTable(tables[w],na.rm=T))
})
yearly_mns = c(means_1987_2007,means_2008_2012)
par(mar=c(4,4,4,5))
plot(yearly_mns,type='b',lwd=2,pch=19,xaxt='n',xlab='',ylab='Mean Arrival Delay (min)',main='Arrival Delays and Numbers of Domestic Flights, 1987-2012')
axis(1,at=1:length(yearly_mns),labels=c(1987:2012),las=2)

# also find number of flights in each year and add that to the graph 
nflights_1987_2007 = sapply(1:21,function(year){
	sum(mergeFreqTable(tables[year],na.rm=T))
})
monthly_files_2008_2012 = sapply(2008:2012,function(y){grepl(y,files)})
nflights_2008_2012 = sapply(1:5,function(y){
	w = which(monthly_files_2008_2012[,y])
	sum(mergeFreqTable(tables[w],na.rm=T))
})
yearly_n = c(nflights_1987_2007,nflights_2008_2012)
par(new=T)
plot(yearly_n,type='b',lwd=2,lty=2,xaxt='n',xlab='',ylab='Mean Arrival Delay (min)',yaxt='n')
axis(4)
mtext('Number of Flights',side=4,outer=T)
text(30,4.5e6,'Number of Flights',xpd=T,srt=90)
legend('topleft',legend=c('Mean Delay','Number of Flights'),pch=c(19,1),lty=c(1,2),cex=.9)

########### save results 
info = list(sessionInfo(),Sys.info())
names(info) = c('sessionInfo','systemInfo')
save(runtime,info,mean,median,sd,file='~/Documents/STA250Winter2014/results_and_info.rda')