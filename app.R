
#Markowitz Optimization

library(ROI) 
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)


#Get financial data 
library(tidyquant)
library(rvest)
library(quantmod)

#Transform and plot data

library(ggplot2)
library(reshape2)
library(GGally)

#Shiny dashboard

library(recipes) 
library(shiny)
library(shinydashboard)
library(shinyWidgets)



cash_flow<- function(tick) {
    
    web  = paste0('https://www.wsj.com/market-data/quotes/',tick,'/financials/annual/cash-flow')
    
    table =web %>% read_html() %>% html_nodes('.cr_dataTable') 
    
    table =table %>% html_table(fill = TRUE)
    
    cash_fl = table[[1]] 
    
    #cash_fl = as.data.frame(na.omit(cash_flow[c(1:6)]))
    
    return(cash_fl)
    
    
}








valoracion <- function(cf, n, wacc, t_crecimiento, g) {
    
    x = list()
    
    for (i in (1:n)) { y = (cf*(1+t_crecimiento)**i)/(1 +  wacc)**i
        
                       x[[i]]<-y}
    
    vp_suma = sum(unlist(x))
    
    TV = (((cf*(1+t_crecimiento)**n)/(1 +  wacc)**n)*(1+g))/(wacc-g)
    
    return(sum(vp_suma,TV,cf))
}

#####app

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 <- url %>%
         read_html() %>%
         html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>% html_table()

SP500 <- SP500[[1]]

Tix <- SP500$`Symbol`

######################


GetClosingPrice <- function(tickers) {
    
    PortfolioPrices = NULL
    
    for (ticker in tickers) {
        
        PortfolioPrices = cbind(PortfolioPrices,
                          getSymbols(ticker, from = "2021-01-01", to = Sys.Date(), auto.assign = FALSE)[,4]
                                
        )}
    
    colnames(PortfolioPrices) = tickers
    
    return(PortfolioPrices)
    
}






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
    
     return(stock_returns_monthly)}



portfolioReturns <- function(df) {
    
    dframe = data.frame(date=index(df), coredata(df))
    
    returns = ROC(dframe[-1]) %>% mutate(suma = rowSums(across(where(is.numeric))))
   
    returns = cbind(dframe[1], returns)
    
    return(returns)
    
}



#################################################################################################################################


######User Interface

#Dashboard page initiates a dashboard with the shinydashboards package, 

ui <-   dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(sidebarMenu(
        menuItem("Stock", tabName = "Stock", icon = icon('chart-line')),
        menuItem("Financials", tabName = "Financials", icon = icon("money-bill-alt")),
        menuItem("Portfolio", tabName = "Portfolio", icon = icon("piggy-bank")) 
    )),
    dashboardBody( tabItems( tabItem
                   
 ########### First tab - historic prices
                            
         (tabName = "Stock",  fluidRow(
       
        # First box with the graph of historic prices
        box(width = 10,
            title = "Historico de Precios", 
           status = "success", #color of header
           solidHeader = TRUE,
           collapsible = TRUE, #option to make the box collapsible
           dropdownButton( icon = icon("sliders"),
                           radioButtons("tipo_grafica", 'Tipo de Grafica', choices = c('Line', 'Candle-stick'))),
          
            selectInput("download", "Select a stock (SPY 500 only)", choices = Tix), 
            
            plotOutput("distPlot")), 
        
        # Box with stock info from last week
        
        box(width = 10,
            title = "Precios",  
            status = 'success',
            collapsible = TRUE, 
            dataTableOutput('table')))), 
        
 ############### Second tab - Financials and DCF calculator
                       
                            
       tabItem(tabName = "Financials",
                fluidRow(
                    
                   
                    box(title = "Financials", 
                        status = "danger", 
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        dataTableOutput('financials')),
                    
                    
                    ### Simple DCF calculator
                    box(title = " Calculate Discounted Cash Flow (DCF)", status = "danger", solidHeader = TRUE,
                        collapsible = TRUE, 
                        sliderInput("wacc", "WACC:", min = 0, max = 0.5, value = 0.1, step = 0.01),
                        sliderInput("g", "Perpetuity Growth:",  min = 0, max = 0.05, value = 0.02, step = 0.001),
                        sliderInput("growth", "Growth Short Term:", min = 0, max = 1, value = 0.1, step = 0.01), 
                        numericInput("cashflow", "Cash Flow:", 10, min = 1, max = 1000000),
                         numericInput("periodos", "Periods:", 10, min = 1, max = 1000000)), 
                    
                    ### Result of DCF - Value of company not price per share
                    infoBox('Company Value (mm)', value = textOutput('valor_empresa'), width = 6, color = 'red')
                    
                  
                
                
        ) 
        )
        ,
        
        #### Markowitz Portfolio                    
        
        tabItem(tabName = 'Portfolio', 
                fluidRow( 
                    box(width=4,status = 'info',pickerInput(
                        inputId = "w",
                        label = "Pick stocks", 
                        choices = Tix,
                        multiple = TRUE,
                        selected = c("AAPL",'TSLA','GOOGL')),
                        tags$p('In finance, the Markowitz model - put forward by Harry Markowitz in 1952 - is a portfolio optimization model; it assists in the selection of the most efficient portfolio by analyzing various possible portfolios of the given securities. Here, by choosing securities that do not move exactly together, the HM model shows investors how to reduce their risk. The HM model is also called mean-variance model due to the fact that it is based on expected returns (mean) and the standard deviation (variance) of the various portfolios. It is foundational to Modern portfolio theory.')),
                    
                    box(plotOutput('portfolioreturn'),
                        width = 8),
                    
                    box(title = 'Optima Weights', 
                        solidHeader = TRUE, 
                        status = 'info',
                        width = 4, 
                        verbatimTextOutput('pesos'),
                        verbatimTextOutput('mean')),
                    
                    box(plotOutput('correlacion'),
                        width = 8)
                 
        ))
        
        )))



######### server ###############

 server <- function(input, output) {
             
             
 tickers  <- reactive(input$w)
             
 wts <- reactive(rep(1/length(tickers()),length(tickers())))
             
 Portfolio <- reactive(GetClosingPrice(tickers()))
 
 Returns <- reactive(portfolioReturns(Portfolio()))
 
Correlacion <- reactive(cor(data.frame(Portfolio(), row.names = NULL)))

Correlacion1 <- reactive(data.frame(Portfolio(), row.names = NULL))

pesosopt <- reactive(PesosOptimos(Portfolio()))


graficareturns <- reactive(pleasee(tickers(),w = rep(1/length(input$w), length(input$w))))


             
 output$distPlot <- renderPlot({
                 
                 if (input$tipo_grafica == 'Line'){        
                     
                     prices  <- tq_get(input$download, get = "stock.prices", from = "2021-01-01")
            
                               ggplot(prices, aes(x = date, y = close)) +
                               geom_line(color = '#0a8212' ) +  
                               ggtitle(input$download) +
                               xlab("") 
                     
                     
                     
                 } else {
                     
                     candle  <- tq_get(input$download, get = "stock.prices", from = "2021-01-01")
                                ggplot(candle, aes(x = date, y = close)) +
                                geom_candlestick(aes(open = open, high = high, low = low, close = close),colour_up = "green", colour_down = "red", 
                                fill_up  = "green", fill_down  = "red") +  
                                xlab("") +
                                theme_tq() + 
                                ggtitle(input$download)+ 
                                theme(panel.background = element_rect(fill = 'gray89'))
                     
                     
                     
                 }
                 
             })
             
output$table <- renderDataTable(tq_get(input$download, get = "stock.prices", from = (Sys.Date() - 7)))

             
output$financials <- renderDataTable(cash_flow,  options = list(pageLength = 10))
             
output$valor_empresa <- renderText(
                        valoracion(input$cashflow,input$periodos,input$wacc,input$growth,input$g))

      
 


output$correlacion <-renderPlot(ggcorr(Correlacion(),label = TRUE) + 
                                ggtitle('Correlation Matrix')+
                                theme(plot.title = element_text(size = 20)))
             

output$portfolioreturn = renderPlot(ggplot(graficareturns(), aes(y= cumsum(Ra),x=date))+
                                    geom_line(color ='#5e96cc', size =1)+
                                    ggtitle('Portfolio Return')+
                                    xlab('') +ylab('') +
                                    theme(plot.title = element_text(size = 20)))
             


output$pesos = renderPrint(pesosopt()[1])
output$mean = renderPrint(pesosopt()[2])

         }
         
         # Run the application 
 shinyApp(ui = ui, server = server)
         
