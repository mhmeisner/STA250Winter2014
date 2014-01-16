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
files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)
del = system('cut -f 15 -d, /Users/matthewmeisner/Downloads/Delays1987_2013/2012_March.csv',intern=TRUE) 

head(del)
length(del)
colnames = tolower(strsplit(readLines('/Users/matthewmeisner/Downloads/Delays1987_2013/1990.csv',1),',')[[1]])
grepl('arr',colnames)&grepl('delay',colnames)
readLines('/Users/matthewmeisner/Downloads/Delays1987_2013/2010_May.csv',4)

col_numbers = sapply(files,function(filename){
	filepath = paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',filename)
	# need to find what column we want, since it's annoyingly not the same in each file 
	colnames = tolower(strsplit(readLines(filepath,1),',')[[1]])	
	col_number = which(grepl('arr',colnames)&grepl('delay',colnames))[1] # get first column that has "arr" and "delay" in name (manual inspection of the files )	
	col_number
})
col_numbers


some_files = files
system.time({
tables = sapply(1:length(some_files),function(i){
	filename = some_files[i]
	cat('currently working on file',filename,'\n')
	filepath = paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',filename)
	# need to find what column we want, since it's annoyingly not the same in each file 
	if(i<=21){
		col_number = 15
	}else{
		col_number = 45 # this gets the "ARR_DEL15" column; the ARR_DELAY column values make no sense! but the ARR_DEL15 values *seem* reasonable...
	}
	shell_command = paste('cut -f',col_number,'-d,',filepath)
	delays = system(shell_command,intern=TRUE)
	table(delays[-1]) # -1 removes the column name
})
})
  user   system  elapsed 
 546.233   25.236 1236.730 
tables
class(tables)
length(tables)
length(files)

# would rm(tables) within the sapply loop matter?

# need to change names of the tables to all be integers (they are in 3.00 form for the later years)
for(i in 22:length(tables)){
	names(tables[[i]]) = as.character(as.integer(names(tables[[i]])))
	names(tables[[i]])[is.na(names(tables[[i]]))]='NA'
}
head(names(tables[[22]]))

# now need a function to merge the tables
mergeFreqTable = function(tt,na.rm=FALSE){
	# tt needs to be a list of tables to be merged
	# returns a named integer vector; names are delay times and value are counts 
	# na.rm deterines in NAs are included in the final table
	
	# first, find all the unique values in the table
	all_names = unlist(lapply(tt,function(t){names(t)}))
	unique_names = unique(all_names)
	
	merged = sapply(unique_names,function(delay){
		sum(sapply(tt,function(t){t[delay]}),na.rm=T)
	})
	# need to add the NA remover
	if(na.rm){
		w = which(names(merged)=='NA')
		merged = merged[-w]
	}
	merged
}
# 15 and 16 are 2001 and 2002 
names(tt[[17]])
class(tt[[15]])
length(tt[[15]])

class(names(tables[[22]]))

m = mergeFreqTable(tables[1:22],na.rm=T)
meanFreqTable(m)

NA %in% names(tt[[1]])
'NA' %in% names(tt[[1]])
NA %in% names(tt[[22]])
'NA' %in% names(tt[[22]])
names(tt[[15]])

#####
m = mergeFreqTable(tables,na.rm=T)
meanFreqTable(m)
medianFreqTable(m)
sdFreqTable(m,meanFreqTable(m))

length(unique_names)
NA %in% merged
NA %in% names(merged)

which(names(merged)=='NA')
names(tables[[20]])

which(names(m)=='NA')
a = 
length(tables)
m = mergeFreqTable(tables,na.rm=T)
class(m)
sum(m)
names(m)

#  check that this works
sum(m) ==sum(tables[[1]],tables[[2]])
i = 'NA'
sum(tables[[1]][i],tables[[2]][i],tables[[3]][i],tables[[4]][i])
m[i]

head(m)
head(names(m))
as.integer(names(m))
names(m)

# next, need functions for mean, median, and sd from freq table
meanFreqTable = function(t){
	sum(as.integer(names(t))*t)/sum(t)
}
meanFreqTable(m)
mn = meanFreqTable(m)

medianFreqTable = function(t,debug=F){
	n = sum(t)
	half = floor(n/2)
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

sum(m)
medianFreqTable(m,debug=T)

t = m
sort(as.integer(names(t)))

# test median function
a = c(1,1,2,2,2,3,3,4)
t1 = table(a)
median(a)
medianFreqTable(t1,debug=T)


sdFreqTable = function(t,mean){
	var_mle = sum(t*(as.integer(names(t))-mean)**2)/sum(t)
	sd_mle = sqrt(var_mle)
	sd_mle
}

sdFreqTable(t,mn)
plot(density(as.numeric(del),na.rm=T))

# nextmethods to try
2. freq table in shell (either looping over files  in R, or all at once in shell)
3. read.csv in blocks in R
4. should also try the current method (just using the shell to get the right column), but use pipe instead of system. could then update the freq table more often, perhaps? Not sure this would help...


del = system('cut -f 15 -d, /Users/matthewmeisner/Downloads/2003.csv',intern=TRUE) 
del
del[1:50]
mean(as.numeric(del),na.rm=T)
readLines('/Users/matthewmeisner/Downloads/Delays1987_2013/2001.csv',4)

del = system('cut -f 45 -d, /Users/matthewmeisner/Downloads/2008_March.csv',intern=TRUE) 
head(del)
colnames = tolower(strsplit(readLines('/Users/matthewmeisner/Downloads/Delays1987_2013/2008_March.csv',1),',')[[1]])
colnames[46]
grepl('arr',colnames)&grepl('delay',colnames)

lapply(strsplit(del[1:1000],'\"'),function(i){i[2]})
del1

substr(del[1],1,1)
nchar(del[2])
del[2]
substr(de,6,6)

# another way to check that mean, median, sd seem to be workign correctly:
n = unlist(sapply(1:length(t),function(i){
	rep(as.numeric(names(t)[i]),t[i])
}))
sum(t)
length(n)
mean(n)
median(n)
medianFreqTable(t)
sd(n)



# 
del = system('cut -f 15 -d, /Users/matthewmeisner/Downloads/2001.csv',intern=TRUE) 
del
del[1:50]
mean(as.numeric(del),na.rm=T)
readLines('/Users/matthewmeisner/Downloads/Delays1987_2013/2001.csv',4)
