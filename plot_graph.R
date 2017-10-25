# new start
library(igraph)

# functions
# data flter: Select only large: ones with more than n connections in the edgelist.
create_edgelist <- function(trade){
  edgelist <- data.frame(from=character(),to=character(),t=integer(),trade=double())
  test=(1:length(trade$r))
  for (i in test){
    print(i)
    edgelist<-rbind(edgelist,data.frame("from"=trade$r[i],"to"=trade$p[i],"t"=trade$t[i],"trade"=trade$m.X[i]))
    edgelist<-rbind(edgelist,data.frame("from"=trade$p[i],"to"=trade$r[i],"t"=trade$t[i],"trade"=trade$m.M[i]))
  }
  edgelist[edgelist==0] <- NA
  edgelist[edgelist=="SUN"] <- NA
  edgelist=edgelist[complete.cases(edgelist),]
  return(edgelist)
}

filter_edgelist <- function(edgelist,n){
  years <- c(min(edgelist$t):max(edgelist$t))
  newedgelist<-data.frame(from=character(),to=character(),t=integer(),trade=double())
  for (year in years){
    print(year)
    temp<-edgelist[edgelist$t==year,]
    tab <- table(temp$from)
    big.id <- names(tab)[tab>n]
    temp  <- temp[temp$from %in% big.id & 
                    temp$to %in% big.id, ]
    newedgelist<-rbind(newedgelist,temp)
  }
  return(newedgelist) 
}

nodelist_from_edgelist <- function(edgelist){
  from<-data.frame("id"=edgelist$from) 
  to<-data.frame("id"=edgelist$to)
  temp<- rbind(from,to)
  temp<-temp[!duplicated(temp$id),]
  nodelist<-data.frame("abbres"=temp)
  return(nodelist)
}

find_full_countryname <- function(nodelist){
  library("countrycode")
  for (i in 1:length(nodelist$abbres)){
    nodelist$country_name[i]<-countrycode(nodelist$abbres[i],'iso3c','country.name')
  }
  nodelist<-na.omit(nodelist)  # get rid of NAs
  return(nodelist)
}

add_location <- function(nodelist){
  library(ggmap)
  for (i in 1:length(nodelist$abbres)){
    country_name<-sapply(nodelist$country_name[i],as.character)
    nodelist$lon[i]<-geocode(country_name)[1]
    nodelist$lat[i]<-geocode(country_name)[2]
  }
  return(nodelist)
}


add_degree<-function(nodelist){
  for (i in 1:length(nodelist$abbres)){
    nodename<-sapply(nodelist$abbres[i],as.character)
    from_sum<-sum(edgelist[edgelist$from==nodename,]$trade)
    to_sum<-sum(edgelist[edgelist$to==nodename,]$trade)
    nodelist$outdegree[i]<-from_sum
    nodelist$indegree[i]<-to_sum
    nodelist$degree[i]<- (from_sum+to_sum)
  }
  return(nodelist)
}


line_plot = function(temp_edgelist,from, to){
  # get longitude/latitude at origin/destination
  lat_o <- unlist(nodelist[nodelist==from,][4])
  long_o <-unlist( nodelist[nodelist==from,][3])
  lat_d <- unlist(nodelist[nodelist==to,][4])
  long_d <- unlist(nodelist[nodelist==to,][3])
  #inter<-list()
  # create map
  inter <- gcIntermediate(c(long_o, lat_o), c(long_d, lat_d), n=100, addStartEnd=TRUE, breakAtDateLine = TRUE)
  #inter <- greatCircle(c(long_o, lat_o), c(long_d, lat_d), n=360,sp=FALSE)
  # edge.ind <- round(log((edgelist$trade[i]) / (max(edgelist$trade))))
  edge.ind <- (temp_edgelist$trade[i]) / (max(temp_edgelist$trade))
  ind <- round((long_o+180)/60)+1
  if (ind==7){print(ind)}
  #lines(inter, col=edge.col[edge.ind], lwd=edge.ind/30)
  #lines(inter, col=mycolor[edge.ind], lwd=edge.ind/30)
  if (length(inter)==2){
    #print("lines into 2")
    # print(inter[1])
    lines(as.data.frame(inter[1])$lon,as.data.frame(inter[1])$lat, col=linecolor[ind], lwd=log(edge.ind*900))
    lines(as.data.frame(inter[2])$lon,as.data.frame(inter[2])$lat, col=linecolor[ind], lwd=log(edge.ind*900))
  } else{
    #print("lines 1")
    lines(inter, col=linecolor[ind], lwd=log(edge.ind*900))
  }
  
  
  #lines(as.data.frame(inter[2])$lon,as.data.frame(inter[2])$lat, col=linecolor[ind], lwd=edge.ind/30)
}

node_plot=function(nodename,edgelist_year,tab){
  lat_o <- unlist(nodelist[nodelist==nodename,][4])
  long_o <-unlist( nodelist[nodelist==nodename,][3])
  # text(long_o, lat_o, nodename, col = 'red', adj = c(-0.1, 1.25),cex=0.5)
  # points(long_o, lat_o, cex =0.6,col="orange" )
  # node.ind <-(tab[names(tab)==nodename]/max(tab))
  interval = round(max(sapply(temp_nodelist$lon,as.double))-min(sapply(temp_nodelist$lon,as.double)))/7
  ind <- round((long_o-min(sapply(temp_nodelist$lon,as.double)))/interval)+1
  wid <- nodelist[nodelist$abbres==nodename,]$degree/max(nodelist$degree)
  points(long_o, lat_o, cex = wid*3,col=nodecolor[ind],pch=19 )
}

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

add.threshold <- function(font_size){
  abline(a=2, b=0, col = 2)
  text(600,2, "Very Stong", col=2, adj=c(-.1,-.1),xpd = TRUE,cex = font_size)
  
  abline(a=1.5, b=0, col = 2)
  text(600,1.5, "Strong", col=2, adj=c(-.1,-.1),xpd = TRUE,cex = font_size)
  
  abline(a=1, b=0, col = 2)
  text(600,1, "Moderate", col=2, adj=c(-.1,-.1),xpd = TRUE,cex = font_size)
  
  
  abline(a=-0.5, b=0, col = 'blue')
  #text(450,0.5, col=2, adj=c(-.1,-.1))
  
  abline(a=-1, b=0, col = 'blue')
  text(600,-1, "Weak", col='blue', adj=c(-.1,-.1),xpd = TRUE,cex = font_size)
  
  abline(a=-1.5, b=0, col = 'blue')
  text(600,-1.5, "Moderate", col='blue', adj=c(-.1,-.1),xpd = TRUE,cex = font_size)
  
  abline(a=-1.5, b=0, col = 'blue')
  text(600,-1.5, "Moderate", col='blue', adj=c(-.1,-.1),xpd = TRUE,cex = font_size)
  
  
  abline(a=-2, b=0, col = 'blue')
  text(600,-2, "Strong", col='blue', adj=c(-.1,-.1),xpd = TRUE,cex = font_size)
}
# Step 1:  data preparation-edgelist (from data trade to edgelist)
# input file: maize.trade.rdata ; output file: final_edgelist.RData)

# 	edgelist <- data.frame(from=character(),to=character(),t=integer(),trade=double())
# 	edgelist <- create_edgelist(trade)
# Select only large: ones with more than 10 connections in the data.
# 	edgelist<-filter_edgelist(edgelist,1)



# Step 2: data preparation-nodelist (generate nodelist from edgelist)
# input: final_edgelist.RData output: nodelist.Rdata

nodelist<-data.frame("abbres"=character())
nodelist<-nodelist_from_edgelist(edgelist)
# add full country name and location to nodelistno
nodelist<-find_full_countryname(nodelist)
# add location to the nodelist
nodelist<-add_location(nodelist)
# add indegree and outrigger
nodelist <- add_degree(nodelist)


# Step 3: plot with location
# http://kateto.net/network-visualization
# http://blog.qiubio.com:8080/archives/2395
# https://wklchris.github.io/R-plotting-basic.html
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
library(maps)
library(geosphere)
library(RColorBrewer)

# save enso_data as enso.Rdata
# enso_data <- read.csv(file="C:\\Users\\jieadmin\\Google Drive\\Research\\Climate Network\\enso.csv", header=TRUE, sep=",")
enso_data <- read.csv(file="/Users/liu1729/Google Drive/Research/Climate Network/clean up/enso.csv", header=TRUE, sep=",")
library(reshape)
enso_data.reshape <- melt(enso_data, id="Year")
enso_data.sort<-enso_data.reshape[order(enso_data.reshape$Year),]



linecolor<-brewer.pal(n = 8, name = 'Spectral')
nodecolor<-brewer.pal(n = 8, name = 'Spectral')
linecolor <-add.alpha(linecolor, alpha=0.2)
#linecolor<-c("#FF0000","#FF601A","#FFF801","#00FF0D","#00FAFF","#003DFF","#E200FF")
#nodecolor<-c("#FF0000","#FF601A","#FFF801","#00FF0D","#00FAFF","#003DFF","#E200FF")

# tab <- table(edgelist$from)


# color
# col.1 <- adjustcolor("orange red", alpha=0.4)
# col.2 <- adjustcolor("orange", alpha=0.1)
# edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
# edge.col <- edge.pal(100)
# color
# library(RColorBrewer)
# display.brewer.all()
# display.brewer.pal(n = 8, name = 'spectrum')

# First plot: plot year by year 
for (year in c(1962:2011)){
	print(year)
	temp_edgelist<-edgelist[edgelist$t==year,]
	# create nodelist for each year
	temp_nodelist<-data.frame("abbres"=character())
	temp_nodelist<-nodelist_from_edgelist(temp_edgelist)
	#temp_nodelist<-find_full_countryname(temp_nodelist)
	#temp_nodelist<-add_location(temp_nodelist)
	temp_nodelist<-nodelist[nodelist$abbres %in% temp_nodelist$abbres,c(1,2,3,4)]
	temp_nodelist <- add_degree(temp_nodelist)
	
	#dev.new()
	#filename= paste("C:\\Users\\jieadmin\\Google Drive\\Research\\Climate Network\\trade_",year,".png")
	filename= paste("/Users/liu1729/Google Drive/Research/Climate Network",year,".png")
	png(file=filename,width=1000,height=1000)
	
	#dev.new()
	# first Plot -world map
	par(fig=c(0,1,0.2,1), new=TRUE,mar=c(5.1,4.1,4.1,5.1))
	first_title<- paste("Trade Network",year)
	
	# Plot a map of world:
	map("world")
	#map("world", fill = TRUE, col = rainbow(200), ylim = c(-60,90), mar = c(0, 0, 0, 0),title("world map"))
	map<-map('world', col = 'grey20', fill = T,ylim = c(-60,90),bg="black", boundary = T, lty = 0)
	# map<-map('world', col = 'grey20', fill = T,ylim = c(-60,90), boundary = T, lty = 0)
	title(first_title,cex=4,col.main="white")
#	title(first_title, outer=TRUE)
	for(i in 1:length(temp_edgelist$from)){
		from<-sapply(temp_edgelist$from[i], as.character)
		to<-sapply(temp_edgelist$to[i], as.character)
		line_plot(temp_edgelist,from,to)
	}

	
	for (i in 1:length(temp_nodelist$abbres)){
		nodename<-sapply(temp_nodelist$abbres[i],as.character)
		node_plot(nodename,temp_edgelist,tab)
	}
	
	
	# second plot 
	par(fig=c(0,1,0,0.3), new=TRUE,fg="white",mar=c(5.1,4.1,4.1,5.1))
	#second_title<- "Oceanic Nino Index(ONI)"
	#title(second_title, outer=TRUE)
	
	#axis_labels<- c("very strong","strong","moderate","weak")
	#axis_posi<-c(2,1.5,1,0.5)
	#axis(4,axis_posi,axis_labels,col='red',hadj=1,srt = 45)

	plot(enso_data.sort$value,axes=FALSE,type="l",xlab=NA,ylab="3-Month Nino Region Average",cex.lab=0.8,col="white",col.lab="white")#, xlab= "time index",)# xlim = c(0, 600), ylim = c(-2, 2))
	# axis(1,cex.axis=1,col="blue")
	axis(2,cex.axis=1,col="white",col.ticks="white",col.axis="white")
	#mtext("y2", side = 4, line = 3,col="blue") #New Y axis label

	
	temp_point_x<-which(enso_data.sort$Year==year)
	temp_point_y<-enso_data.sort[enso_data.sort$Year==year,]
	points(temp_point_x,temp_point_y$value,col='red')
	
	add.threshold(1.1)
	
	legend(600,0.7, # places a legend at the appropriate place 
			c("El Nino","La Nina"), # puts text in the legend
			lty=c(1,1), # gives the legend appropriate symbols (lines)
			lwd=c(2.5,2.5),col=c("red","blue"),cex = 0.9,xpd = TRUE, bg="black",text.col="white") # gives the legend lines the correct color and width
	dev.off();
}






# second analysis and plot

# functions
find_countinues_edges<-function (from, to,interval,mylist,mylist_year){
  # find edges countinues for interval years
  df1<-data.frame(from=character(),
                 to=character(),
                 start=integer(),
                 end=integer()
  )
  for( i in seq(1,length(mylist)-interval+1) ){
    temp=mylist[i:(i+interval-1)]
    check_year=mylist_year[i:(i+interval-1)]
    check_year=check_year[temp!=0]
    #print(check_year)
    if( length(check_year)  == interval && (check_year[(interval)]-check_year[1] == (interval-1))){
      # list years
      # data frame: years[]=i
      df1<-rbind(df1,data.frame(from=from, to=to, start=mylist_year[i],end=mylist_year[(i+interval-1)]))
    }
  }
  return (df1);
}


# find continues edges : return df
## countinues_data<-data.frame()
## colSums: from, to, year_start, year_end
df<-data.frame(from=character(),
               to=character(),
               start=integer(),
               end=integer()
)
interval=40;
for (i in seq(1,nrow(nodelist))){
#  print(i)
  for (j in seq(1,nrow(nodelist))){
    from=as.character(nodelist$abbres[i])
    to=as.character(nodelist$abbres[j])
    aa=edgelist[edgelist$from == from,]
    bb=aa[aa$to == to,]
    #print(bb)
    mylist=bb$trade
    mylist_year=bb$t
    if (length(mylist)>interval) {
      df1<-find_countinues_edges(from, to,interval,mylist,mylist_year)
      #print(df1)
      df<-rbind(df,df1)      
    }
  }
}


# remove duplicated
aaa<-df[!duplicated(df[,1:2]), ]





# visulize the links
# Add the label into edgelist # visualize_label 
# for example 1: perserve for many years color:green 2:  others:0
edgelist['visualize_label']=0

# find the links only appear during /before /after extreme weather




# label the links (for each links/years) 
# ->give a label (presist for over ten years) / new emerge / only emerge before extrem weather/

for (line in df){
  start = line['start']
  end_s = line['end']
  for (i in seq(start,end_s)){
    from= line['from']
    to= line['to']
  }
  
}



















# Turning networks into igraph objects
library(igraph)
for (year in trade$t){
	edgelist_year=edgelist[edgelist$t==year,]
	net <- graph_from_data_frame(d=edgelist_year[edgelist_year$trade>0,], vertices=nodelist$abbres, directed=T)
	dev.new()
	plot(net,vertex.size=.4, edge.arrow.size=.4,edge.curved=0.1,layout=layout_in_circle)
}

# E(net) # The edges of the "net" objectyear
# V(net) # The vertices of the "net" object
# E(net)$type # Edge attribute "type"
# V(net)$media # Vertex attribute "media"


V(net)$size <- 8
V(net)$frame.color <- "white"
V(net)$color <- "orange"
V(net)$label <- ""
E(net)$arrow.mode <- 0
plot(net,layout=layout_in_circle)







sPDF <- joinCountryData2Map(nodelist,joinCode = "ISO3",nameJoinColumn = "abbres")
mapBubbles(sPDF, nameZSize="POP_EST",nameZColour="BIODIVERSITY",colourPalette='topo',numCats=5,catMethod="quantiles")






# Step 3: plot ---------------------------------------start here
for (year in trade$t){
	edgelist_year=edgelist[edgelist$t==year,]
	net <- graph_from_data_frame(d=edgelist_year, vertices=nodelist, directed=T)
	dev.new()
	plot(net,vertex.size=.4, edge.arrow.size=.4,edge.curved=0.1,layout=layout_in_circle)
}
