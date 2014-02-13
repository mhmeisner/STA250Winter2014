####################### parallel package in R
library('parallel')

# need to create file list first, then create cluster, so all nodes can access this
files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)

cl = makeCluster(4,'FORK')

# get tables for each year, in parallel
time4 = system.time({
tables = clusterApply(cl,1:length(files),getFreqTable)
})

# fix NA discrepancy between two data file formats:
for(i in 22:length(tables)){
	names(tables[[i]]) = as.character(as.integer(names(tables[[i]])))
	names(tables[[i]])[is.na(names(tables[[i]]))]='NA'
}

# merge tables
merged_table = mergeFreqTable(tables,na.rm=T)

# find stats
mean = meanFreqTable(merged_table)
median = medianFreqTable(merged_table)
sd = sdFreqTable(merged_table,mean)

# save results
#info = list(sessionInfo(),Sys.info())
#names(info) = c('sessionInfo','systemInfo')
#save(time4,info,mean,median,sd,file='~/Documents/STA250Winter2014/results_parallel.rda')
load('~/Documents/STA250Winter2014/results_parallel.rda')
time4 


################## parallel shell commands
# set up 3 shell processes extracting desired column; run them at the same time 
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/198[7-9].csv ~/Downloads/Delays1987_2013/199[0-8].csv| cut -f 15 -d, > /Users/matthewmeisner/Documents/grp1.txt &')
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/199[7-9].csv ~/Downloads/Delays1987_2013/200[0-7].csv| cut -f 15 -d, > /Users/matthewmeisner/Documents/grp2.txt &')
system('export LANG=C; grep -v ARR_DEL15 ~/Downloads/Delays1987_2013/200[8-9]*.csv ~/Downloads/Delays1987_2013/201[0-2]*.csv | cut -f 45 -d, > /Users/matthewmeisner/Documents/grp3.txt &')

# to time this: every minute, check and see if each process is done, by seeing if no new lines have been added to the output files
parallel_shell_time = system.time({
ngrps = 3
current_counts = numeric(ngrps)
converged=logical(ngrps)
while(sum(converged)<ngrps){
	print(current_counts)
	print(converged)
	for(i in 1:ngrps){
		if(!converged[i]&current_counts[i]!=''){
			command = paste0('wc -l ~/Documents/grp',i,'.txt')
			new_count = strsplit(system(command,intern=TRUE),' ')[[1]][2]
			if(new_count==current_counts[i]){
				converged[i]=TRUE
			}
			current_counts[i] = new_count
		}
	}
	Sys.sleep(60)
}

})

parallel_shell_time

t1 = read.csv('~/Documents/grp1.txt')
t2 = read.csv('~/Documents/grp2.txt')
t3 = read.csv('~/Documents/grp3.txt')

merged_table = mergeFreqTable(list(table(t1),table(t2),table(t3)),na.rm=T)

# find stats
mean = meanFreqTable(merged_table)
median = medianFreqTable(merged_table)
sd = sdFreqTable(merged_table,mean)



#######################  pthreads method 
install.packages('~/Documents/stat250/AirlineDelays',repos=NULL, type='source')
library(AirlineDelays)


files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)
expanded_files = paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',files)

no_thread_AirlineDelays_time = system.time({
tables = sapply(files, function(file){
	cat('currently working on file',file,'\n')
	getDelayTable(paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',file))
})
}) # elapsed way way higher than user +system 
no_thread_AirlineDelays_time

merged_table = mergeFreqTable(tables,na.rm=T)

# find stats
mean = meanFreqTable(merged_table)
median = medianFreqTable(merged_table)
sd = sdFreqTable(merged_table,mean)



pthreads_time = system.time({
tt = getDelayTable_thread(files=list(expanded_files[1:10],expanded_files[11:21],expanded_files[22:52],expanded_files[52:81]),fieldNum=c(15,15,45,45),numThreads=4L)
})
pthreads_time

# not exactly right: 
meanFreqTable(tt)
medianFreqTable(tt)
sdFreqTable(tt,meanFreqTable(tt))

###################### method 4: commands ran on hadoop: 
# load data to HDFS:
hadoop fs -copyFromLocal /home/mmeisner/Delays1987_2013/* /user/mmeisner/Data5
# run MapReduce job:
hadoop jar eg.jar DelaysFrequencyTable Data5 output5
# copy results from HDFS to local:
hadoop fs -copyToLocal /user/mmeisner/output5/part-r-00000 /home/mmeisner/results/out.txt
# copy results from hadoop to lipschitz
scp out.txt mmeisner@lipschitz.ucdavis.edu:~
# copy results form lipschitz to my computer: 
scp mmeisner@lipschitz.ucdavis.edu:~/out.txt ~/Documents/hadoop_out.txt


d = read.table("~/Documents/hadoop_out.txt")
t = as.numeric(d[,2])
names(t) = d[,1]
meanFreqTable(t)
medianFreqTable(t)
sdFreqTable(t,meanFreqTable(t))

###################### Needed Functions for all methods 
getFreqTable = function(i){
	# takes in index of object 'files' (should be a character vector of filenames), returnes freq table of appropriate columsn 
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
}

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
