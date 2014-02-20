---
title: Rebalancing Mystery
lead: Still a Mystery But New Research Makes It a Little Clearer
framework: pure
mode     : selfcontained # {standalone, draft}
highlighter: prettify
hitheme: twitter-bootstrap
assets:
  css:
    - http://yui.yahooapis.com/pure/0.4.2/pure-min.css
    - http://purecss.io/combo/1.11.2?/css/main-grid.css&/css/main.css&/css/home.css&/css/rainbow/baby-blue.css
---

Throughout my career as a portfolio manager, I have been taught that rebalancing is good.  However, it seemed few understood why rebalancing is good or even explored if rebalancing is as universally positive as believed.  A research paper published last year is the first that led me down this path and resulted in some enlightenment.

<blockquote>
Kiskiras, John and Nardon, Andrea<br>
<strong>Portfolio Rebalancing: A Stable Source of Alpha</strong><br> <em>January 18, 2013</em><br>
Available at SSRN: <a href="http://ssrn.com/abstract=2202736">http://ssrn.com/abstract=2202736</a>
<br><br>
<strong>Abstract: </strong>In this work we verify that portfolio rebalancing can generate an excess return under certain market conditions. In line with existing measures, developed specifically to capture that alpha (Rebalancing Bonus), we show that high volatility as well as low correlation maximize the magnitude of the excess return. However, in contrast to previous works, we demonstrate that the actual driver and therefore sufficient condition for a Rebalancing Bonus is the presence of relative mean-reversion.
</blockquote>


```r
#get very helpful Ken French data
#for this project we will look at Industry Portfolios
#http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/48_Industry_Portfolios_daily.zip

require(PerformanceAnalytics)
require(quantmod)
require(xtsExtra)

#my.url will be the location of the zip file with the data
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/48_Industry_Portfolios_daily.zip"
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/10_Industry_Portfolios_daily.zip"
#this will be the temp file set up for the zip file
my.tempfile<-paste(tempdir(),"\\frenchindustry.zip",sep="")
#my.usefile is the name of the txt file with the data
#my.usefile<-paste(tempdir(),"\\48_Industry_Portfolios_daily.txt",sep="")
my.usefile<-paste(tempdir(),"\\10_Industry_Portfolios_daily.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_industry <- read.table(file=my.usefile,
                              header = TRUE, sep = "",
                              as.is = TRUE,
                              skip = 9, nrows=22881)

#get dates ready for xts index
datestoformat <- rownames(french_industry)
datestoformat <- paste(substr(datestoformat,1,4),
                       substr(datestoformat,5,6),substr(datestoformat,7,8),sep="-")

#get xts for analysis
french_industry_xts <- as.xts(french_industry[,1:NCOL(french_industry)],
                              order.by=as.Date(datestoformat))

#divide by 100 to get percent
french_industry_xts <- french_industry_xts/100

#delete missing data which is denoted by -0.9999
french_industry_xts[which(french_industry_xts < -0.99,arr.ind=TRUE)[,1],
                    unique(which(french_industry_xts < -0.99,arr.ind=TRUE)[,2])] <- 0





data.monthly <- french_industry_xts 

#get cumulative of individual components as price
data.monthly.cumul <- data.frame(
  lapply(
    cumprod(1+data.monthly),
    FUN=function(x){
      x[endpoints(x,on="months"),]
    }
  ))

data.monthly.cumul <- as.xts(data.monthly.cumul,orderBy=as.Date(index(data.monthly[endpoints(data.monthly,on="months"),])))
colnames(data.monthly.cumul) <- colnames(data.monthly)


#get the returns for a non rebalanced portfolio
#starting point will be cumulative return for each by themselves
#so just divide all the monthly values by the beginning value
bh.cumul <-
  data.monthly.cumul /
  matrix(
    data.monthly.cumul[1,],
    nrow=NROW(data.monthly.cumul),
    ncol=NCOL(data.monthly.cumul),
    byrow=TRUE
  )
#test our calculation graphically
#should look exactly the same except scale
#plot.zoo(merge(bh.cumul,data.monthly.cumul),nc=2)
#now let's calculate cumulative at the portfolio level
#multiple all by 1/N or 1/ncol
#then sum by row
portfolio <- list()
portfolio$bh <- as.xts(
  apply(
    bh.cumul * 1/NCOL(bh.cumul),
    MARGIN = 1,
    FUN = sum
  ),
  orderBy = as.Date(index(bh.cumul))
)
#get the returns for a monthly rebalanced portfolio
#since we are looking at monthly, we get monthly returns
#then multiply each by 1/NCOL then sum returns by row
#get monthly returns
rebal.cumul <- data.monthly.cumul/lag(data.monthly.cumul,k=1)-1
#make first 0 instead of NA to start at 1
rebal.cumul[1,] <- 0
portfolio$rebal <- as.xts(
  cumprod(apply(
    rebal.cumul * 1/NCOL(rebal.cumul),
    MARGIN = 1,
    FUN = sum) + 1
  ),
  orderBy = as.Date(index(rebal.cumul))
)

#get all indexes in same format, same class, etc. so merge will be proper
index(portfolio$bh) <- index(portfolio$rebal) <- index(data.monthly.cumul)


xtsExtra::plot.xts(  
  merge(
    #data.monthly.cumul[,1],
    portfolio$bh,
    portfolio$rebal#,
    #apply(data.monthly.cumul,MARGIN=1,FUN=max)-portfolio$rebal, #best component - rebal portfolio
    #portfolio$rebal-portfolio$bh  #rebalancing bonus(Rb)
  ),
  screens=1,
  #screens=c(rep(1,NCOL(data.monthly.cumul)),2,2,3,4),
  #layout.screens=matrix(c(1,2,1,2,3,4),ncol=2,byrow=TRUE),
  #ylim = matrix(c(-1,5,-1,5,-1,50,-15,15),ncol=2,byrow=TRUE),
  auto.legend=TRUE,
  main = "Rebalancing Bonus (Nardon/Kiskiras)\nFrench Industry"
)


data.monthly.cumul2 <- merge(data.monthly.cumul$NoDur,data.monthly.cumul)
data.monthly.cumul2

outperform <- xts()
#test all combinations with 1st Agriculture for fun
for (i in 2:NCOL(data.monthly.cumul2)){
  temp <- data.monthly.cumul2[,c(1,i)]
  #get the returns for a non rebalanced portfolio
  #starting point will be cumulative return for each by themselves
  #so just divide all the monthly values by the beginning value
  bh.cumul <-
    temp /
    matrix(
      temp[1,],
      nrow=NROW(temp),
      ncol=NCOL(temp),
      byrow=TRUE
    )
  #test our calculation graphically
  #should look exactly the same except scale
  #plot.zoo(merge(bh.cumul,temp),nc=2)
  #now let's calculate cumulative at the portfolio level
  #multiple all by 1/N or 1/ncol
  #then sum by row
  portfolio <- list()
  portfolio$bh <- as.xts(
    apply(
      bh.cumul * 1/NCOL(bh.cumul),
      MARGIN = 1,
      FUN = sum
    ),
    orderBy = as.Date(index(bh.cumul))
  )
  #get the returns for a monthly rebalanced portfolio
  #since we are looking at monthly, we get monthly returns
  #then multiply each by 1/NCOL then sum returns by row
  #get monthly returns
  rebal.cumul <- temp/lag(temp,k=1)-1
  #make first 0 instead of NA to start at 1
  rebal.cumul[1,] <- 0
  portfolio$rebal <- as.xts(
    cumprod(apply(
      rebal.cumul * 1/NCOL(rebal.cumul),
      MARGIN = 1,
      FUN = sum) + 1
    ),
    orderBy = as.Date(index(rebal.cumul))
  )
  
  #get all indexes in same format, same class, etc. so merge will be proper
  index(portfolio$bh) <- index(portfolio$rebal) <- index(temp)
  outperform <- merge(outperform, portfolio$rebal-portfolio$bh)
}
colnames(outperform) <- colnames(data.monthly.cumul2)[-1]

t(tail(outperform,1))
#hm <- table.HigherMoments(french_industry_xts[,colnames(data.monthly.cumul2)],french_industry_xts[,colnames(data.monthly.cumul2)[1]])
#corr <- table.Correlation(french_industry_xts,french_industry_xts$Agric)[,1]
#capm <- table.CAPM(french_industry_xts[,colnames(data.monthly.cumul2)],french_industry_xts[,colnames(data.monthly.cumul2)[1]])


statsTable <- cbind(
  t(tail(outperform[,-which(colnames(outperform)=="NoDur.1")],1)),
  (t(tail(data.monthly.cumul2,1)) - t(tail(data.monthly.cumul2,1))[1])[-c(1,which(colnames(data.monthly.cumul2)=="NoDur.1")),],
#  t(hm)[-1,],
#  t(capm)[-1,]
)[,-(3:4)]
colnames(statsTable)[1:2] <- c("PerfDiff","CumulLess")

#get colors to use for heat map style coloring by out/under performance
require(RColorBrewer)
require(latticeExtra)
dotplot(t(tail(outperform,1)))
brew <- brewer.pal(name="RdBu",n=5)
#get color ramp
cc.brew <- colorRampPalette(brew)
#apply color ramp
cc <- cc.brew(10)
#do colors based on out/under performance but with gray so visible when labelling
cc.palette <- colorRampPalette(c(cc[1],"gray60",cc[length(cc)]))
cc.levpalette <- cc.palette(10)
cc.levels <- level.colors(statsTable[,1], at = do.breaks(c(-max(abs(range(statsTable[,1]))),max(abs(range(statsTable[,1])))),10),
                          col.regions = cc.levpalette)


require(rCharts)
p1 <- rCharts$new()
p1$setLib(system.file('parcoords', package = 'rCharts'))
p1$set(
  padding = list(top = 24, left = 100, bottom = 12, right = 200),
  height = "600",
  width = "1000"
)
#get range of data
#for colors to be right need min and max to be same so 0 is center
maxabs <- max(abs(range(statsTable[,1])))
p1$set(data = toJSONArray(statsTable, json = F), 
       colorby = 'PerfDiff', 
       range = c( -maxabs, 0, maxabs ),
       colors = c( 
         paste0( max( cc.levels) ),
         'gray',
         paste0( min( cc.levels) )
       )
)
p1


#chart.RollingPerformance(ROC(data.monthly.cumul[,c("Aero","NoDur")],type="discrete",n=1),width=36)
#chart.RollingPerformance(ROC(data.monthly.cumul[,c("Oil","NoDur")],type="discrete",n=1),width=36)
xyplot(log(outperform[,rev(tail(order(t(tail(outperform,1))),20))]),scales=list(y=list(relation="same")))
xyplot(outperform[,rev(head(order(t(tail(outperform,1))),20))],scales=list(y=list(relation="same")))
```
