# server.R
library(shiny)
install.packages("data.table")
library(data.table)
source("helperfunction.R")

#add opportunity costs, costs from expired milk
#(overstocking costs), and costs compared to
#optimal ordering amount

shinyServer(
  function(input, output, clientData, session) {
    
    observe({
      sel=input$select
      rmean=runif(1,4,25)
      rsd=rmean/runif(1,2.5,5)
      c_nums = c(round(rnorm(21,rmean,rsd)))
      rcost=round(runif(1,.5,5), digits=2)
      rsell1=rcost*runif(1,1,3)
      rsell=round(rsell1, digits=2)
      rexpiration=round(runif(1,5,15))
      
      if (sel==2){
        updateNumericInput(session, "num1", value=c_nums[1])
        updateNumericInput(session, "num2", value=c_nums[2])
        updateNumericInput(session, "num3", value=c_nums[3])
        updateNumericInput(session, "num4", value=c_nums[4])
        updateNumericInput(session, "num5", value=c_nums[5])
        updateNumericInput(session, "num6", value=c_nums[6])
        updateNumericInput(session, "num7", value=c_nums[7])
        updateNumericInput(session, "num8", value=c_nums[8])
        updateNumericInput(session, "num9", value=c_nums[9])
        updateNumericInput(session, "num10", value=c_nums[10])
        updateNumericInput(session, "num11", value=c_nums[11])
        updateNumericInput(session, "num12", value=c_nums[12])
        updateNumericInput(session, "num13", value=c_nums[13])
        updateNumericInput(session, "num14", value=c_nums[14])
        updateNumericInput(session, "num15", value=c_nums[15])
        updateNumericInput(session, "num16", value=c_nums[16])
        updateNumericInput(session, "num17", value=c_nums[17])
        updateNumericInput(session, "num18", value=c_nums[18])
        updateNumericInput(session, "num19", value=c_nums[19])
        updateNumericInput(session, "num20", value=c_nums[20])
        updateNumericInput(session, "num21", value=c_nums[21])
        updateNumericInput(session, "exp", value=rexpiration)
        updateNumericInput(session, "cost", value=rcost)
        updateNumericInput(session, "sell", value=rsell)
      }
      
    })
    
    milk <- reactive({
      c(as.numeric(input$num1),
        as.numeric(input$num2),
        as.numeric(input$num3),
        as.numeric(input$num4),
        as.numeric(input$num5),
        as.numeric(input$num6),
        as.numeric(input$num7),
        as.numeric(input$num8),
        as.numeric(input$num9),
        as.numeric(input$num10),
        as.numeric(input$num11),
        as.numeric(input$num12),
        as.numeric(input$num13),
        as.numeric(input$num14),
        as.numeric(input$num15),
        as.numeric(input$num16),
        as.numeric(input$num17),
        as.numeric(input$num18),
        as.numeric(input$num19),
        as.numeric(input$num20),
        as.numeric(input$num21))  
    })
    
    mean <- reactive({
      sum(milk())/length(milk())
    })
    var <- reactive({
      sum((milk()-sum(milk())/length(milk()))^2)/(length(milk())-1) 
    })
    expi <- reactive({
      as.numeric(input$exp)
    })
    newmean <- reactive({
      expi()*mean()
    })
    newvar <- reactive({
      expi()^2*var()
    })
    cost <- reactive({
      as.numeric(input$cost)
    })
    sell <- reactive({
      as.numeric(input$sell)
    })
    overcost<- reactive({
      cost()
    })
    undercost<- reactive({
      sell() - cost()
    })
    q <- reactive({
      undercost()/(overcost()+undercost())
    })
    opt <- reactive({
      qnorm(q(), newmean(), sqrt(newvar()))
    })
    
    summary <- reactive({
      data.frame(Summary=c("Mean per day",
                           "Standard deviation per day",
                           "Mean per expiration date",
                           "Standard deviation per expieration date",
                           "Profit Margin",
                           "Optimal amount to order"),
                 Statistic=c(mean(),
                             sqrt(var()),
                             newmean(),
                             sqrt(newvar()),
                             sprintf("%.2f %%", 100*sell()/cost()),
                             opt()))
    })
    
    allcosts <- reactive({
      all.costs(meanexp = newmean(), sdexp = sqrt(newvar()),
                exp = expi(), opt = opt(), 
                overcost=overcost(), undercost=undercost())
    })
    
    costtable <- reactive({
      data.frame(Gallons.ordered=c(1:(2*opt())),
                 Yearly.revenue.loss=c(allcosts()))
    })
    
    output$opt <- renderText({
      opt()
    })
    output$mean <- renderText({
      mean()
    })
    output$sd <- renderText({
      sqrt(var())
    })
    output$newmean <- renderText({
      newmean()
    })
    output$newsd <- renderText({
      sqrt(newvar())
    })
    output$hist <- renderPlot({
      hist(milk())
    })
    output$summary <- renderDataTable({
      summary()
    })
    output$costs <- renderDataTable({
      allcosts()
    })
    output$appinfo <- renderUI({
      HTML('<p>In this version, you can input data of daily milk sales for the
           past 21 days or randomize it. You can also input your cost for
           each milk unit, how much you sell it for, and the expiration
           date.</p>
           <p>The summary tab shows the mean and standard deviation of
           milk sales per day and per each expiration period. It also
           shows your profit margin and the optimal amount of milk you
           should order on each expiration date.</p>
           <p>The plot tab shows a histogram of your daily milk sales.</p>
           <p>The costs tab shows your yearly costs depending on how many
           units of milk you order each expiration date. The first column
           shows how many gallons of milk were ordered per expiration date.
           The second column shows how much you will save ordering the optimal amount of 
           milk compared to other amounts. The third column shows the yearly
           costs of not having perfect information. The fourth column
           shows the costs from ordering too much milk [the costs of how
           much milk expires before it is sold] while the fifth column
           shows the opportunity costs related to not ordering enough
           milk.</p>
           <p>This version uses multiple assumptions.</p>
           <p>A user input related assumption is that the input sales data
           occurs with an unlimited supply of milk. In other words, if a
           customer wants to buy milk from the vendor, they are always
           able to.</p>
           <p>One optimization related assumption is that daily sales are
           normally distributed. Another is that milk sales are 
           independent of the day of the week and of time in general.</p>
           <p>An ordering assumption is that a new order of milk always
           arrives the day after the previous order expires. In future
           versions that account for this assumption, there will be an
           assumption that milk is always sold FIFO unless it expires.</p>
           <p>There are several business and cost related assumptions. One is that there
           are no storage costs. Another is the assumption that there
           is no disparity between delivery costs with how much milk
           is ordered, and that milk can be ordered in single units
           rather than multiples of 4, 6, 12, etc. A third is that milk
           being in stock does not encourage nor discourage the sale
           of other items, such as soy milk or cereal. Also, this app
           does not take into account the idea that some people may only
           come into the store because of the milk and end up buying other
           things while they are there. It also does not take into account
           the appearance of the vendor "what kind of store runs out of
                                      milk on a sunday morning?!". There are many other factors
           that depend on the vendor and clientelle that require
           qualitative decisions rather than quantitative ones.</p>')
    })
  }
)