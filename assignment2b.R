install.packages('~/Documents/stat250/AirlineDelays',repos=NULL, type='source')
library(AirlineDelays)


files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)
expanded_files = paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',files)
t = getDelayTable('~/Downloads/Delays1987_2013/1987.csv') # really fast 
system.time({
tables = sapply(files, function(file){
	cat('currently working on file',file,'\n')
	getDelayTable(paste0('/Users/matthewmeisner/Downloads/Delays1987_2013/',file))
})
}) # elapsed way way higher than user +system 


tt = getDelayTable_thread(files=list('/Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv','/Users/matthewmeisner/Downloads/Delays1987_2013/1988.csv'),fieldNum=c(15,15),numThreads=2L)
class(tt)
sum(tt)

system.time({
tt = getDelayTable_thread(files=as.list(expanded_files)[1:4],fieldNum=c(rep(15,21),rep(45,60)),numThreads=4L)
})
class(tt)
sum(tt)


tt = getDelayTable_thread(files=list(expanded_files[1],expanded_files[2]),fieldNum=c(15,15),numThreads=2L)
sum(tt)
meanFreqTable(tt)


# real # of rows 
a = sapply(1:2,function(i){
	system(paste0('wc -l ',expanded_files[i]),intern=TRUE)
}) 
sum(as.integer(sapply(strsplit(a,' '),'[[',2)))

# old method 
tables = sapply(1:2,function(i){
	filename = files[i]
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
t = mergeFreqTable(tables)
sum(t) # exactly number of rows -2 
t = mergeFreqTable(tables,na.rm=T)
sum(t)
meanFreqTable(t)

head(t)
names(t)
names(tt)
tt['3']
t['4']








################## parallel shell commands


system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/198[7-9].csv | cut -f 15 -d, > /Users/matthewmeisner/Documents/grp1.txt &')
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/199[0-4].csv | cut -f 15 -d, > /Users/matthewmeisner/Documents/grp2.txt &')
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/199[5-9].csv | cut -f 15 -d, > /Users/matthewmeisner/Documents/grp3.txt &')
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/200[0-3].csv | cut -f 15 -d, > /Users/matthewmeisner/Documents/grp4.txt &')
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/200[4-7].csv | cut -f 15 -d, > /Users/matthewmeisner/Documents/grp5.txt &')
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/200[8-9]*.csv | cut -f 45 -d, > /Users/matthewmeisner/Documents/grp6.txt &')
system('export LANG=C; grep -v ArrDelay ~/Downloads/Delays1987_2013/201[0-2]*.csv | cut -f 45 -d, > /Users/matthewmeisner/Documents/grp7.txt &')


system.time({
ngrps = 7
current_counts = numeric(ngrps)
converged=logical(ngrps)
while(sum(converged)<ngrps){
	print(current_counts)
	print(converged)
	for(i in 1:ngrps){
		if(!converged[i]){
			command = paste0('wc -l ~/Documents/grp',i,'.txt')
			new_count = strsplit(system(command,intern=TRUE),' ')[[1]][2]
			if(new_count==current_counts[i]){
				converged[i]=TRUE
			}
			current_counts[i] = new_count
		}
	}
	Sys.sleep(10)
}

})




strsplit(system('wc -l ~/Documents/grp1.txt',intern=TRUE),' ')[[1]][2]
strsplit(system('wc -l ~/Documents/grp2.txt',intern=TRUE),' ')[[1]][2]
strsplit(system('wc -l ~/Documents/grp3.txt',intern=TRUE),' ')[[1]][2]

