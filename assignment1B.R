system.time({t = system('bzip2 -dc /Users/matthewmeisner/Downloads/1990.csv.bz2|cut -f 15 -d,|sort|uniq -c',intern=TRUE) })
t = system('bzip2 -dc /Users/matthewmeisner/Downloads/1990.csv.bz2|cut -f 15 -d,|sort|uniq -c',intern=TRUE) 
t = system('cut -f 15 -d, /Users/matthewmeisner/Downloads/1990.csv.bz2|sort|uniq -c',intern=TRUE) 
class(t)
head(t)
length(t) # 740
tail(t)
grepl('[0-9]',strsplit(t[736],' ')[[1]])



system.time({
col_number=15
sh = paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv  |sort|uniq -c')
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

