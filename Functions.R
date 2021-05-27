
#####   All functions created

# The function cash_flow retrieves financial statements from the wall street journal. It is called  
# cash   flow since thats the value I am interested in to calculate the dcf

cash_flow<- function(tick) {
  
  web  = paste0('https://www.wsj.com/market-data/quotes/',tick,'/financials/annual/cash-flow')
  
  table =web %>% read_html() %>% html_nodes('.cr_dataTable') 
  
  table =table %>% html_table(fill = TRUE)
  
  cash_fl = table[[1]] 
  
  #cash_fl = as.data.frame(na.omit(cash_flow[c(1:6)]))
  
  return(cash_fl)
  
  
}





# Valoracion calculates the dcf according to a single cash flow value and constant short term growth
# 
# cf = cash flow, n = number of periods(years) forward, wacc = weighted average cost of capital, 
# 
# t_crecimiento = short term growth, g = growth in perpetuity


valoracion <- function(cf, n, wacc, t_crecimiento, g) {
  
  x = list()
  
  for (i in (1:n)) {
    
    y = (cf*(1+t_crecimiento)**i)/(1 +  wacc)**i
    
    x[[i]]<-y
    
    
    
    
  }
  
  vp_suma = sum(unlist(x))
  
  TV = (((cf*(1+t_crecimiento)**n)/(1 +  wacc)**n)*(1+g))/(wacc-g)
  
  return(sum(vp_suma,TV,cf))
}


#The following lines scrape the tickers of the s&p 500 from wikipedia

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>% html_table()
SP500 <- SP500[[1]]
Tix <- SP500$`Symbol`



#Get closing price gets the closing price for each ticker in the list 


GetClosingPrice <- function(tickers) {
  
  PortfolioPrices = NULL
  
  for (ticker in tickers) {
    
    PortfolioPrices = cbind(PortfolioPrices,
                            getSymbols(ticker, from = "2021-01-01", to = Sys.Date(), auto.assign = FALSE)[,4]
                            
    )}
  
  colnames(PortfolioPrices) = tickers
  
  return(PortfolioPrices)
  
}



#Pesos optimos gets the optimal weigths for the portfolio. The variable portfolio prices must be a df with 
#the returns of each stock.


PesosOptimos <- function(PortfolioPrices) {
  
  
  
  PortfolioPriceReturn = na.omit(ROC(PortfolioPrices))
  
  
  
  
  portf <- portfolio.spec(colnames(PortfolioPrices))
  
  portf = add.constraint(portf, type = 'weight_sum', min_sum =1,max_sum=1)
  portf = add.constraint(portf, type = 'box', min = .10, max = .6)
  portf = add.objective(portf, type = 'return', name = 'mean')
  portf = add.objective(portf, type = 'risk', name = 'StdDev')
  
  optPort = optimize.portfolio(PortfolioPriceReturn, portf, optimize_method = 'ROI')
  
  return(optPort)
  
} 

#Pleasee calulates the returns of the portfolio 

pleasee <- function(tickers,w) {
  
  
  stock_returns_monthly <- tickers %>%
    tq_get(get  = "stock.prices",
           from = "2021-01-01",
           to   = Sys.Date()) %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "daily", 
                 col_rename = "Ra") %>% tq_portfolio(assets_col  = symbol, 
                                                     returns_col = Ra, 
                                                     weights     = w,
                                                     col_rename  = "Ra")
  
  
  return(stock_returns_monthly)
  
}





