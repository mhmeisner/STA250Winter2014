STA250Winter2014 Assignment 1
Matt Meisner
23Jan2014
==========================
The goal is to find the mean, median, and standard deviation of arrival delays for domestic commercial aviation flights in the US from 1987-2012.  

The first method I implemented was to use the shell (called from R using system), to extract the desired column from each of the .csv files.  I first unzipped the tar.bz2 file into the component .csv files, since I couldn't figure out how to extract the column without unzipping, and I couldn't figure out how to loop through the files if I just piped the result of the unzipping step to 'cut' to extract the correct column (and getting the values from all the files at once is unwieldily -- actually, maybe I could have used pipe() to deal with this in blocks..will try for next week).

A basic outline of the steps is as follows: 
1. Unzip the tar.bz2 into .csv files
2. Loop over filenames in R
3. For each file, call the shell from R to extract the column of interest using cut
4. Use R to create a frequency table of delay times for each year (in my experiments, this was much faster than creating this table using sort uniq in the shell, but I'll explore this in more depth for next week).
5. Merge those tables together (it surprised me how fast it was to do this)
6. Calculate quantities of interest using the complete frequency table.

The second method is very similar, but uses the shell instead of R to create the frequency table.  The third method I implemented was using a Postgres database.  See the sta250hw1_mhm.pdf for detailed explanations of the three methods. 

The results, runtime, and session info are attached in the results_final.rda file.  I found a mean delay time of about 6.6 minutes, with a median of 0 (wish that happened when I fly!) and sd of 31.6 minutes.  The mean being higher than the median suggests to me that it was more common for there to be flights that were extremely late than it was for flights to be extremely early.  In other words, while about the same number of flights arrived early and late, the flights that were late tended to be later than the flights that were early tended to be early.  
  
I also was curious to see if there have been trends for flight delays over time, so I found the mean arrival delay by year (and the number of total flights with recorded delay times in each year).  This computation was practically free given that the frequency tables of delay times were already made for each year (took less than 10 seconds).  I've attached a plot called yearly_means.pdf.  There seems to be a peak in delays in the late 1980s, the late 1990s, and mid 2000s.  However, in recent years, the airlines' behavior seems to be improving!  It's my cynical suspicion that they're just getting better at padding flight schedules with extra times to appear to be more on time, though.  

The dip in delay times in 2002 interestingly corresponds with the dramatic reduction in flight schedules precipitated by the reduced demand for air travel in the wake of the 9/11 terrorist attacks.  We can see from the dashed line, which displays the total number of domestic flights each year (for which a delay time was recorded), that there was indeed a noticeable drop in air travel in 2002.  Also, at least for 1997-2012, there appears to be a correlation wherein years with more flights had greater mean delay times (a result that makes complete sense given the congested US airspace and lack of extra air traffic control capacity, especially at major airports).  However, we can't definitively conclude from eyeballing the plot that this correlation is actually significant or that it indicates a causal relationship.  This plot also suggests that we don't have complete data from 1987, since the number of flights in the dataset from that year is quite small. It's also interesting that the observed decline in flight numbers the last few years, in contrast to 2005-2006, corresponds with the global financial disaster which wreaked economic havoc on the airline industry and I know led the airlines to reduce flight capacity to help increase fares and profits.  