library('parallel')
cl = makeCluster(4,'FORK')
a = c(1,2,3)
clusterApply(cl,a,function(x){print(x)})

files = system('ls /Users/matthewmeisner/Downloads/Delays1987_2013',intern=TRUE)
files1 = files[1:21]


getFreqTable = function(i){
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

system.time({
tables = clusterApply(cl,files1[1],getFreqTable)
})

system.time({
tables = clusterApply(cl,files1[1:4],getFreqTable)
})


clusterEvalQ(cl,ls())
clusterEvalQ(cl,Sys.getpid)
clusterEvalQ(cl,getFreqTable)


system.time({
tables = clusterApply(cl,1:2,getFreqTable)
})
