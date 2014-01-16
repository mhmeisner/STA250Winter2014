STA250Winter2014 Assignment 1
Matt Meisner
16Jan2014
==========================
The goal is to find the mean, median, and standard deviation of arrival delays for domestic commercial aviation flights in the US from 1987-2012.  

The method I implemented was to use the shell (called from R using system), to extract the desired column from each of the .csv files.  I first unzipped the tar.bz2 file into the component .csv files, since I couldn't figure out how to extract the column without unzipping, and I couldn't figure out how to loop through the files if I just piped the result of the unzipping step to 'cut' to extract the correct column (and getting the values from all the files at once is unwieldily -- actually, maybe I could have used pipe() to deal with this in blocks..will try for next week).

A basic outline of the steps is as follows: 
1. Unzip the tar.bz2 into .csv files
2. Loop over filenames in R
3. For each file, call the shell from R to extract the column of interest using cut
4. Use R to create a frequency table of delay times for each year (in my experiments, this was much faster than creating this table using sort uniq in the shell, but I'll explore this in more depth for next week).
5. Merge those tables together (it surprised me how fast it was to do this)
6. Calculate quantities of interest using the complete frequency table.

The results, runtime, and session info are attached in the results_and_info.rda file.  I found a mean delay time of about 6.7 minutes, with a median of 0 (wish that happened when I fly!) and sd of 31.6 minutes.  The mean being higher than the median suggests to me that it was more common for there to be flights that were extremely late than it was for flights to be extremely early.  In other words, while about the same number of flights arrived early and late, the flights that were late tended to be later than the flights that were early tended to be early.  

The total computation only took about 14 minutes, which seems pretty reasonable given that we're working with more than 100 million flight data points.  Unzipping also took 20 minutes or so, but I don't know a good way around this.  

I also was curious to see if there have been trends for flight delays over time, so I found the mean arrival delay by year.  This computation was practically free given that the frequency tables of delay times were already made for each year (took less than 15 seconds).  I've attached a plot in yearly_means.pdf.  There seems to be a peak in delays in the late 1980s, the late 1990s, and mid 2000s.  However, in recent years, the airlines' behavior seems to be improving!  It's my cynical suspicion that they're just getting better at padding flight schedules with extra times to appear to be more on time, though.  (Note: 2001 and 2002 are omitted due to the problem reading the files mentioned on Piazza).