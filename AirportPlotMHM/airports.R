# load files
d = read.csv('~/Downloads/Delays1987_2013/2012_August.csv')
a = read.csv('http://eeyore.ucdavis.edu/stat250/Data/Airlines/airports.csv')
names(d)
head(a)

# find 30 biggest airports
t = table(d$ORIGIN)
big_airports = names(t)[order(t,decreasing=T)[1:30]]

big_airport_locs = sapply(big_airports,function(apt){
	a[a$iata==apt,c('lat','long')]	
})

aa = data.frame(apt = colnames(big_airport_locs),long=unlist(big_airport_locs[2,]),lat=unlist(big_airport_locs[1,]))

library(maps)

### modify a few points that are overlappping where airports are really close (exact location is lost, but readability is enchanced! )
aa_orig = aa
aa = aa_orig
aa['MDW','lat'] = aa['MDW','lat']-.5
aa['MIA','lat'] = aa['MIA','lat']-.5
aa['DCA','lat'] = aa['DCA','lat']-1
aa['IAD','lat'] = aa['IAD','lat']-.4
aa['EWR','lat'] = aa['EWR','lat']-.3
aa['JFK','lat'] = aa['JFK','lat']+.3
aa['LGA','lat'] = aa['LGA','lat']+.8

svg('~/Documents/STA250Winter2014/AnimatedPlot/airport.svg')
map('state',mar=c(0,0,0,0))
# add airport points to the plot:
points(aa[,2],aa[,3],pch=19,cex=1.3,col='red')
text(aa[,2]-1.7,aa[,3],aa[,1],cex=.7,font=2)
dev.off()
library(SVGAnnotation)

doc = xmlParse('~/Documents/STA250Winter2014/AnimatedPlot/airport.svg')
n = getNodeSet(doc,'//x:path',c(x = "http://www.w3.org/2000/svg")) # nodes 193 through 222 are the red ones that we want to add the onclick attr to 

# add onclick attrs to appropriate nodes
mapply(function(node, county){xmlAttrs(node, append = TRUE) = c(onclick = sprintf("parent.iata ='%s'; parent.showPlot('%s',parent.plottype);", county, county))},n[193:222], aa[,1])
saveXML(doc, "airport1.svg")




############### make the .pngs to display for each airport
# first, plot distribution of airlines for flights from that airport:
# subset 11 largest airlines to look at:
interesting_airlines = c('AA','AS','B6','DL','F9','FL','HA','UA','US','VX','WN')
d$ORIGIN = as.character(d$ORIGIN)
airlines_by_apt = sapply(aa[,1],function(apt){
	table(d$UNIQUE_CARRIER[d$ORIGIN==as.character(apt)])[interesting_airlines]
})/31

# make png files of the mean daily departures by airlines
for(i in 1:30){ 
	filepath = paste0('~/Documents/STA250Winter2014/AnimatedPlot/',aa[i,1],'dep.png')
	png(filepath)
	o = order(airlines_by_apt[,i],decreasing=T)
	plot(1:11,airlines_by_apt[,i][o],xaxt='n',xlab='Airline',ylab='Mean Number of Daily Departures',pch=19,main=paste('Daily Departures by Airline at',aa[i,1]),col='blue',cex=1.3,type='b',lty=2)
	axis(1,at=1:11,labels=interesting_airlines[o])
	abline(h=0,lty=2)
	legend('topright',legend=c('AA: American','AS: Alaska','B6: JetBlue','DL: Delta','F9: Frontier','FL: AirTran','HA: Hawaiian','UA: United','US: US Airways','VX: Virgin America','WN: Southwest'),cex=1)
	dev.off()	
}

delays_by_apt = sapply(aa[,1],function(apt){
	table(d$UNIQUE_CARRIER[d$ORIGIN==as.character(apt)])[interesting_airlines]
})

# make pngs of boxplots of arrival delays by airlines:
for(i in 1:30){ 
	filepath = paste0('~/Documents/STA250Winter2014/AnimatedPlot/',aa[i,1],'del.png')
	png(filepath)
	dd = d[d$ORIGIN==aa[i,1]&d$UNIQUE_CARRIER %in% interesting_airlines,]
	# acrobatics to plot boxplots in order of least to greatest delay
	dd$UNIQUE_CARRIER = droplevels(dd$UNIQUE_CARRIER)
	meds = aggregate(dd$ARR_DELAY,list(dd$UNIQUE_CARRIER),median,na.rm=T)
	o = order(meds[,2])
	colors = rep('red',nrow(meds))
	colors[meds[,2]<=0]='green'
	dd$UNIQUE_CARRIER=as.factor(dd$UNIQUE_CARRIER)
	m= data.frame(l=levels(dd$UNIQUE_CARRIER)[o],o=1:nrow(meds)) # order we need the new factors to be in 
	dd$carr_ordered = as.factor(m$o[match(dd$UNIQUE_CARRIER,m$l)])
	table(dd$carr_ordered,dd$UNIQUE_CARRIER)
	boxplot(ARR_DELAY~carr_ordered,data = dd,ylim=c(-50,50),xlab='Airline',ylab="Delays (Minutes)",col=colors[o],xaxt='n',main=paste('Arrival Delays by Airline at',aa[i,1]))
	abline(h=0,lty=2)
	axis(1,at=1:nrow(meds),labels=m[,1])

	#legend('topright',legend=c('AA: American','AS: Alaska','B6: JetBlue','DL: Delta','F9: Frontier','FL: AirTran','HA: Hawaiian','UA: United','US: US Airways','VX: Virgin America','WN: Southwest'),cex=1)
	dev.off()	
}


### unsuccessful attempts to use Google Maps and Open Maps 
# try RgoogleMpas instead
library(RgoogleMaps)
lat = range(aa[,3])
long = range(aa[,2])
center = c(mean(lat),mean(long))
zoom = min(MaxZoom(lat,long))
gm = GetMap(center=center,zoom=zoom,size=c(640,450),destfile='terrain1.png',maptype='mobile',verbose=0)
svg('~/Documents/STA250Winter2014/AnimatedPlot/google_airport.svg')
GetMap(center=center,zoom=zoom,size=c(640,450),destfile='terrain.png',maptype='terrain')
dev.off()


doc = xmlParse('~/Documents/STA250Winter2014/AnimatedPlot/google_airport.svg')
n = getNodeSet(doc,'//x:path',c(x = "http://www.w3.org/2000/svg")) # nodes 194 through 223 

#################### 
library(OpenStreetMap)
map = openmap(c(43,119),c(33,133),minNumTiles=3)

map <- openmap(c(43.46886761482925,119.94873046875),c(33.22949814144951,133.9892578125),minNumTiles=3,type='osm')