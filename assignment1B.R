system.time({t = system('bzip2 -dc /Users/matthewmeisner/Downloads/1990.csv.bz2|cut -f 15 -d,|sort|uniq -c',intern=TRUE) })
t = system('bzip2 -dc /Users/matthewmeisner/Downloads/1990.csv.bz2|cut -f 15 -d,|sort|uniq -c',intern=TRUE) 
t = system('cut -f 15 -d, /Users/matthewmeisner/Downloads/1990.csv.bz2|sort|uniq -c',intern=TRUE) 
class(t)
head(t)
length(t) # 740
tail(t)
grepl('[0-9]',strsplit(t[736],' ')[[1]])



system.time({
col_number=45
sh = paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/2012_May.csv  |sort|uniq -c')
t = system(sh,intern=T)

# will need to convert this to a table

del = unlist(sapply(1:length(t),function(i){
	split = strsplit(t[i],' ')[[1]]
	is_num = grepl('[0-9]',split)
	if(sum(is_num)==2){
		delay = as.numeric(split[is_num][2])
		n = split[is_num][1]
		return(c(delay,n))
	}else{
		return(NULL)
	}
}))

delays = del[seq(from=1,to=length(del)-1,by=2)]
counts = as.integer(del[seq(from=2,to=length(del),by=2)])
names(counts)=delays
})

sum(counts)
length(counts)

system.time({
t1 = table(system(paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv'),intern=TRUE))
t1 = t1[-((length(t1)-1):length(t1))]
})


length(t1) # same as length(counts)
t1
sum(t1) # same as sum(counts)


# check that these are all the same for both: yes! 
meanFreqTable(t1)
meanFreqTable(counts)

medianFreqTable(t1)
medianFreqTable(counts)

sdFreqTable(t1,meanFreqTable(t1))
sdFreqTable(counts,meanFreqTable(counts))

# loop over files for method 2 (makign talbe in shell)

files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)
#runtime = system.time({
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
	shell_command = paste('export LANG=C; cut -f',col_number,'-d,',filepath,'|sort|uniq -c')
	t = system(shell_command,intern=TRUE)
	
	# now, convert this to a table
	del = unlist(sapply(1:length(t),function(i){
		split = strsplit(t[i],' ')[[1]]
			is_num = grepl('^-|^[0-9]',split) # regular expression weeds out the blank/NA column and the ARR-DELAY header 

		if(sum(is_num)==2){
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

})


# testing for new format:
# method 1:
col_number=15
t1 = table(system(paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/2002.csv'),intern=TRUE))
t1 = t1[-((length(t1)-1):length(t1))] # only works for first format - will need to remove the ARR_DELAY and NA differently for later 


sh = paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/2002.csv  |sort|uniq -c')
t = system(sh,intern=T)

# will need to convert this to a table

del = unlist(sapply(1:length(t),function(i){
	split = strsplit(t[i],' ')[[1]]
	is_num = grepl('^-|^[0-9]',split) # regular expression weeds out the blank/NA column and the ARR-DELAY header 
	if(sum(is_num)==2){
		delay = as.numeric(split[is_num][2])
		n = split[is_num][1]
		return(c(delay,n))
	}else{
		return(NULL)
	}
}))

delays = del[seq(from=1,to=length(del)-1,by=2)]
counts = as.integer(del[seq(from=2,to=length(del),by=2)])
names(counts)=delays
# these are the same for 2002.csv
meanFreqTable(t1)
meanFreqTable(counts)
