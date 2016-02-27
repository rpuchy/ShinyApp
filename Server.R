#This is part of shiny project this is the Server.R Code
library(shiny)
library(xts)
library(dygraphs)
library(quantmod)
library(tseries)
library(ggplot2)

yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,5)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

#GetData(c("MCO"),as.Date("01/05/1965", "%m/%d/%Y") ,as.Date("01/05/2010", "%m/%d/%Y") )
GetData <- function(stocks,date1,date2){
  SMonth<-as.numeric(format(date1,'%m'))-1
  SDay<-format(date1,'%d')
  SYear<-format(date1,'%Y')
  EMonth<-as.numeric(format(date2,'%m'))-1
  EDay<-format(date2,'%d')
  EYear<-format(date2,'%Y')
  address<-sprintf("http://real-chart.finance.yahoo.com/table.csv?s=%s&a=%s&b=%s&c=%s&d=%s&e=%s&f=%s&g=d&ignore=.csv",stocks,SMonth,SDay,SYear,EMonth,EDay,EYear)
  results<-NULL
  for (ad in address)
  {
    c<-NULL
    try(c  <- yahoo.read(ad),silent=T)
    if (!is.null(c))
    {
       c_xts <- xts(c$Close,order.by=c$Date,frequency=365)
       results<-cbind(results,c_xts)
    }
  }
  names(results)<-stocks
  results
}

ConvertReturns <-function(stocks,period)
{
  result<-NULL
  for (i in 1:ncol(stocks))
  {
    result<-cbind(result, periodReturn(stocks[,i],period=period))
  }
  names(result)<-names(stocks)
  result
}

effFrontier = function (averet, rcov, nports = 20, shorts=T, wmax=1) 
{
  mxret = max(abs(averet))
  mnret = -mxret
  n.assets = ncol(averet)
  reshigh = rep(wmax,n.assets)
  if( shorts )
  {
    reslow = rep(-wmax,n.assets)    
  } else {
    reslow = rep(0,n.assets)    
  }
  min.rets = seq(mnret, mxret, len = nports)
  vol = rep(NA, nports)
  ret = rep(NA, nports)
  sharpe = rep(NA, nports)
  for (k in 1:nports)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets
                                    [k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow,shorts=shorts),silent=T)
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[k] = averet %*% port.sol$pw
      sharpe[k] = ret[k]/vol[k]
    }
  }
  return(list(vol = vol, ret = ret, sharpe = sharpe))
}

plotEff <- function(stockdata,period)
{
  stockdata.ret<-ConvertReturns(stockdata,period)
  averet <- matrix(colMeans(stockdata.ret, na.rm = TRUE),nrow=1)
  rcov <- cov(stockdata.ret, use = "na.or.complete")
  eff_list<-effFrontier(averet, rcov,nports=200)
  eff<-NULL
  eff$Std.Dev<-eff_list$vol
  eff$Exp.Return <-eff_list$ret
  eff$Sharpe <-eff_list$sharpe
  eff<-data.frame(eff)
  eff<-eff[complete.cases(eff),]
  eff.optimal.point <- eff[eff$Sharpe==max(eff$Sharpe, na.rm=T),]
  
  ealred  <- "#7D110C"
  ealtan  <- "#CDC4B6"
  eallighttan <- "#F7F6F0"
  ealdark  <- "#423C30"
  ggplot(data.frame(eff), aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
    geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=Sharpe), color=ealred, size=5) +
    annotate(geom="text", x=eff.optimal.point$Std.Dev, y=eff.optimal.point$Exp.Return,
             label=paste("Risk: ", round(eff.optimal.point$Std.Dev*100, digits=3),"%\nReturn: ",
                         round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                         round(eff.optimal.point$Sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=1.2) +
    ggtitle("Efficient Frontier\nand Optimal Portfolio") + labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
    theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
          plot.title=element_text(size=24, color=ealred))
}



shinyServer(
  function(input, output,sesssion) {
    

    stockdata <- eventReactive(input$goButton, {
      GetData(input$inStocks,as.Date(as.character(input$StartDate), "%Y-%m-%d"),as.Date(as.character(input$EndDate), "%Y-%m-%d"))
      
    })
    
    chart <- eventReactive(input$goButton, {
  
      dygraph(stockdata(),ylab="Close", 
              main="Stock Closing Stock Prices") %>%
        #        dyOptions(colors = c("blue","brown")) %>%
        dyRangeSelector()
    })
    
    chart2 <- eventReactive(input$goButton, {
      
      plotEff(stockdata(),'monthly')
    })
    
    output$dygraph <- renderDygraph({
      chart()
      })
    output$EffFrontier <- renderPlot({
      chart2() 
    })
  }
)