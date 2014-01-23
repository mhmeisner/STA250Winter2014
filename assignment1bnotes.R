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
	counts
})


# testing for new format:
# method 1:
col_number=45
t1 = system(paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/2009_April.csv'),intern=TRUE)
t1 = table(t1[-1])
t1 = t1[-1]  #remove the NAs

# method 2:
sh = paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/2009_April.csv  |sort|uniq -c')
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
# these are the same for 2002.csv and 2009_April.csv
meanFreqTable(t1)
meanFreqTable(counts)

# testing on all
merged_table = mergeFreqTable(tables)
meanFreqTable(merged_table) # 6.56
medianFreqTable(merged_table) # 0
sdFreqTable(merged_table,meanFreqTable(merged_table)) # 31.55

# identical results to method 1!!!
print(load('~/Documents/STA250Winter2014/results_and_info.rda'))
mean
median
sd



# method 3
1. remove later columns from files in shell
2. read just as many columsn as necessary into R (in blocks), and make a tablew ithout ever storing the samples 


readLines('~/Downloads/Delays1987_2013/1987.csv',2)


CREATE TABLE delays2(year INT, month INT);

CREATE TABLE delays3(year INT, month INT, day INT,dayofweek INT,deptime INT,crsdeptime INT,arrtime INT, crsarrtime INT,carrier character(10), flightnum INT,tailnum character(10),eltime float,crseltime float,airtime float, arrdelay INT,depdelay float,origin character(10), dest character(10),distance float,taxiin character(10),taxiout character(10),cancelled character(10),canccode character(10),diverted character(10),carrierdel character(10),weatherdel character(10), nasdelay character(10), secdelay character(10), ladelay character(10));


\copy delays3 FROM '/Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv'  DELIMITER ',' CSV HEADER null 'NA';


select count(*) from delays3; 

select avg(arrdelay) from delays3; # got: 9.4466990497746688

# 
col_number=15
t1 = system(paste('export LANG=C; cut -f',col_number,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv'),intern=TRUE)
t1 = table(t1[-1])
t1 = t1[-which(names(t1)=='NA')]  #remove the NAs
meanFreqTable(t1) # 9.446699 same result! 
head(t1)

library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="postgres")
dbGetQuery(con, "select count(*) from delays3")
dbGetQuery(con, "select avg(arrdelay) from delays3")

u = dbGetQuery(con, "SELECT DISTINCT arrdelay FROM delays3")



sqrt(dbGetQuery(con, "select var_samp(arrdelay) from delays3"))

# will need to get a frequency table for sd and median -- no built in commands 

t = dbGetQuery(con, "SELECT count(*) FROM delays3 GROUP BY arrdelay")
head(t)

272 %in% t1

# this matches for all the values I tried -- so the format for the frequency table created using GROUP BY has been deciphered. 
head(u)
head(t)
i = sample(1:500,10)
u[i,]
t[i,]
t1[as.character(u[i,])]


system('~/local/bin/psql -U matthewmeisner postgres; CREATE TABLE delays2(year INT, month INT);')


CREATE TABLE delays4(year INT, month INT, day INT,dayofweek INT,deptime INT,crsdeptime INT,arrtime INT, crsarrtime INT,carrier character(10), flightnum INT,tailnum character(10),eltime float,crseltime float,airtime float, arrdelay INT,depdelay float,origin character(10), dest character(10),distance float,taxiin character(10),taxiout character(10),cancelled character(10),canccode character(10),diverted character(10),carrierdel character(10),weatherdel character(10), nasdelay character(10), secdelay character(10), ladelay character(10));



 postgresqlBuildTableDefinition(con,'test', d,row.names=F)
 dbGetQuery(con, "select count(*) from test")
 
dbWriteTable(con,'test',d)


dbWriteTable(con,'test',file('/Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv','r'))
 
 
 
d1 = readLines('/Users/matthewmeisner/Downloads/Delays1987_2013/1987.csv')
 postgresqlBuildTableDefinition(con,'test', d,row.names=F) 
d = read.csv('~/Downloads/Metamorphosis.csv')
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays4(year INT, month INT, day INT,dayofweek INT,deptime INT,crsdeptime INT,arrtime INT, crsarrtime INT,carrier character(10), flightnum INT,tailnum character(10),eltime float,crseltime float,airtime float, arrdelay INT,depdelay float,origin character(10), dest character(10),distance float,taxiin character(10),taxiout character(10),cancelled character(10),canccode character(10),diverted character(10),carrierdel character(10),weatherdel character(10), nasdelay character(10), secdelay character(10), ladelay character(10));"',intern=TRUE)
system('~/local/bin/psql -U matthewmeisner postgres -c "\copy delays4 FROM /Users/matthewmeisner/Downloads/Delays1987_2013/1988.csv  DELIMITER , CSV HEADER null NA;"',intern=TRUE)



u = dbGetQuery(con, "SELECT DISTINCT origin FROM delays3")


cut -f 15 -d, 198*.csv| grep -v ArrDelay | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays5 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';"

CREATE TABLE delays5(arrdelay INT);


dbGetQuery(con, "select count(*) from delays6")
dbGetQuery(con, "select avg(arrdelay) from delays5")
 
 
 
system('wc -l /Users/matthewmeisner/Downloads/Delays1987_2013/200*.csv') # same number of rows as the above command!

CREATE TABLE delays5(arrdelay INT);
cat 19*.csv 2000.csv 2001.csv 2002.csv 2003.csv 2004.csv 2005.csv 2006.csv 2007.csv | cut -f 15 -d,| grep -v ArrDelay | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays5 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';"

cat 1987.csv 1988.csv | cut -f 15 -d,| grep -v ArrDelay | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays6 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';"


# final database creation:
CREATE TABLE delays1987to2007(arrdelay INT);
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays1987to2007(arrdelay INT);"')
cat 19*.csv 2000.csv 2001.csv 2002.csv 2003.csv 2004.csv 2005.csv 2006.csv 2007.csv | cut -f 15 -d,| grep -v ArrDelay | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays1987to2007 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';" # took 11 minutes 
dbGetQuery(con, "select count(*) from delays1987to2007")
dbGetQuery(con, "select avg(arrdelay) from delays1987to2007")

CREATE TABLE delays2008to2012(arrdelay INT);
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays2008to2012(arrdelay FLOAT);"')
dbRemoveTable(con,'delays2008to2012')
cat 2008*.csv 2009*.csv 2010*.csv 2011*.csv 2012*.csv  | cut -f 45 -d,| grep -v ARR_DEL15 | sed 's/^$/NA/g' | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays2008to2012 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';" # took 12 minutes 

cat 2008_April.csv | cut -f 45 -d,| grep -v ARR_DEL15 | sed 's/^$/NA/g' | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays2008to2012 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';"

r = readLines('~/Downloads/Delays1987_2013/2008_April.csv', n=5)

r = readLines('~/Downloads/Delays1987_2013/1988.csv', n=5)

strsplit(r[1],',')[[1]][45]

# different things about later format
1. column header is different
2. need different column
3. NAs are blanks 

t1 = system(paste('export LANG=C; cut -f',45,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/2008_April.csv'),intern=TRUE)
unique(t1)

# try with character
dbRemoveTable(con,'delays2008to2012')
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays2008to2012(arrdelay float);"')
dbGetQuery(con, "select count(*) from delays2008to2012")
dbGetQuery(con, "select avg(arrdelay) from delays2008to2012")

cat 2008_April.csv | cut -f 45 -d,| grep -v ARR_DEL15 | sed 's/^$/NA/g' |head

# try also getting carrier column and then grouping by that: 
# or maybe all of the columns, at least from the first batch?



dbRemoveTable(con,'delays1987to2007')
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays1987to2007(carrier CHARACTER(10), arrdelay FLOAT,origin CHARACTER(3),dest CHARACTER(3));"')
cat 19*.csv 2000.csv 2001.csv 2002.csv 2003.csv 2004.csv 2005.csv 2006.csv 2007.csv | cut -f 9,15,17,18 -d,| grep -v ArrDelay | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays1987to2007 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';" # took about 12 minutes 

dbRemoveTable(con,'delays2008to2012')
system('~/local/bin/psql -U matthewmeisner postgres -c "CREATE TABLE delays2008to2012(carrier CHARACTER(10),origin CHARACTER(3),dest CHARACTER(3), arrdelay FLOAT);"')
cat 2008*.csv 2009*.csv 2010*.csv 2011*.csv 2012*.csv  | cut -f 9,15,25,45 -d,| grep -v ARR_DEL15 | sed 's/,$/,NA/g' | ~/local/bin/psql -U matthewmeisner postgres -c "COPY delays2008to2012 FROM STDIN DELIMITER ',' CSV HEADER null 'NA';" # took 10 minutes 

head 2008_April.csv | cut -f 9,15,25,45 -d,|sed 's/^$/NA/g'

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

t1 = system(paste('export LANG=C; cut -f',25,'-d,','/Users/matthewmeisner/Downloads/Delays1987_2013/2008_April.csv'),intern=TRUE)
# 9: carrier, 15: origin IATA code, 25: dest IATA code 