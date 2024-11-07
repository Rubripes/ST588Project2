#
# This is a Shiny web application designed for ST588
# This was authored by Matt Bray
# This is Project 2
#
#

library(shiny)
library(tidyverse)
library(readxl)
library(questionr)
library(vtable)
library(hardhat)
library(geofacet)
library(bslib)
library(shinycssloaders)


#data are stored in same folder as app.  Read dataset for app.
data<-read_excel("./US Superstore data.xls")

#data$names are reformatted for programmatic consistency, and are coerced to Factors as appropriate
factors<-c("shipMode", "segment", "country", "state", "postalCode", "region", "category", "subCategory")
data3 <- data |>
  rename("rowID" = "Row ID",
         "orderID" = "Order ID",
         "orderDate" = "Order Date",
         "shipDate" = "Ship Date",
         "shipMode" = "Ship Mode",
         "customerID" = "Customer ID",
         "customerName" = "Customer Name",
         "segment" = "Segment",
         "country" = "Country",
         "city" = "City",
         "state" = "State",
         "postalCode" = "Postal Code",
         "region" = "Region",
         "productID" = "Product ID",
         "category" = "Category",
         "subCategory" = "Sub-Category",
         "productName" = "Product Name",
         "sales" = "Sales",
         "quantity" = "Quantity",
         "discount" = "Discount",
         "profit" = "Profit"
  ) |>
  mutate_at(factors, factor)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Store Sales Data"),

    # create Sidebar layout
sidebarLayout(
  sidebarPanel(
    #slider input to select low end of profit for subsetting
    sliderInput("profitRangeLow", 
                "Profit Range to Subset From (Lower Bound)",
                min = -6600,
                max = 8400,
                value = -6600,
                step = 100),
    #slider input to select high end of profit for subsetting
    sliderInput("profitRangeHigh", 
                "Profit Range to Subset (Upper Bound",
                min = -6600,
                max = 8400,
                value = 8400,
                step = 100),
    #create action button that subsets data
    actionButton("subset",
                 "Display Data in Profit Range"),
    #create download button that downloads subset data
    downloadLink("downloadData",
                 "Download Data from Data Download Tab"),
    #input selector for factor variable 1
    selectInput("variable",
                "Choose Category to Visualize Sales Volume by Category:",
                c("Market Segment" = "segment",
                  "Ship Mode" = "shipMode",
                  "Category" = "category")),
    #input selector for factor variable 2
    selectInput("geographicRegion",
                "Choose Region for 2-way Frequency Table",
                c("Country" = "country",
                  "City" = "city",
                  "State" = "state",
                  "Region" = "region")),
                  
    #input selector for numeric variable for summaries
    selectInput("numericVar",
                "Choose Numeric Variable for Numeric Summary by Category",
                c("Profit" = "profit",
                  "Discount" = "discount")),
    #input selector for sales vs. profit
    selectInput("salesVProfit",
                "Explore Sales vs. Profits",
                c("Profit" = "profit",
                  "Sales" = "sales")),
    #select input for date coloring for sales vs profit plot
    selectInput("yearVMonth",
                "Color Sales vs. Profits by Month or Year:",
                c("Year" = "year",
                  "Month" = "month")),
    selectInput("yearProfit",
                "Year to Explore Profit by:",
                 year(data3$orderDate))
    ),
  
    mainPanel(
      navset_card_underline(
        nav_panel("About", markdown(
          glue::glue(
            "These data are sales data from a large company and were downloaded from [Kaggle](https://www.kaggle.com/datasets/juhi1994/superstore/data).  This app allows the user to explore sales volumes, profits, discounts, etc. across 4 years (2014-2017).  The sidebar functions are used to subset categorical and numeric variables to allow the user to visualize and quantify different values of interest to the business.  The Data tab displays the data.  The data in the Data tab can be subset by creating a range of profits using the sliders in the sidebar and pressing the 'Subset Profit Range' button."
          )
          )),
        
        nav_panel("Data Exploration",
         tableOutput("contingency"),
         plotOutput("contingencyPlot"),
         tableOutput("regionPlot"),
         tableOutput("categoricalProfits"),
         plotOutput("salesVProfit"),
         plotOutput("stateSales"),
         plotOutput("pie"),
         plotOutput("profitQuant"),
         plotOutput("profitYear")),
        
        nav_panel("Data Download", DT::dataTableOutput("data1"))
      
    )
  )
))



# Define server logic:
server <- function(input, output, session) {
  #1 way contingency table.  Subset (select) data using selectInput
  observeEvent(input$subset,{
    output$data1 <- renderDataTable({
    #isolate action data subsetting so that sliders don't have effect unless action button is pushed.
    isolate(data3 |> 
      filter(between(data3$profit, input$profitRangeLow, input$profitRangeHigh)))
    })
  })
  
  output$contingency <- renderTable({
    xfactor <- data3 |> 
      select(c(input$variable,
               "quantity"))
    wtd.table(xfactor[,1], weights=xfactor[,2])
  })
  #display bar graph for selected variables in 1 way contingency table above
  output$contingencyPlot <- renderPlot({
    xfactor <- data3 |> 
      select(c(input$variable,
               "quantity"))
    ggplot(data = xfactor, mapping = aes_string(x=input$variable, y=xfactor$quantity, fill=input$variable)) +
      geom_col() +
      labs(x = input$variable,
           y = "Items Sold (#)",
           fill = input$variable,
           title = "# Items Sold by Category")
  })
  #2 way contingcy table by user selected geography for same categories as 2way table.
  output$regionPlot <- renderTable({
    xfactor <- data3 |> 
      select(c(input$variable,
               input$geographicRegion,
               "quantity"))
    wtd.table(xfactor[,1], xfactor[,2], weights=xfactor[,3], na.rm=TRUE)
  })
  #Generate numeric summaries of the profits by selected factor
  output$categoricalProfits <- renderTable({
    sumTable<-sumtable(data=data1, vars=input$numericVar, group=input$variable, out="return")
    sumTable
  })
  #explore how discounts may impact sales and profits
  output$salesVProfit <- renderPlot({
    data2 <- data3 |>
      mutate(month = month(orderDate),
               year = year(orderDate))
  ggplot(data = data2, mapping = aes_string(x=data2$discount, y=input$salesVProfit, color=input$yearVMonth)) +
    geom_point() +
    labs(title=paste0("Does Discount affect ", input$salesVProfit, "?"),
         x = input$data1$discount,
         y = input$salesVProfit,
         color=input$yearVMonth)
  })
  #visualize sales volume between categories across the states:
  output$stateSales <- renderPlot({
    ggplot(data = data3, aes_string(x=input$variable, y=data3$quantity, fill = input$variable)) +
      geom_col() +
      coord_flip() +
      facet_geo(~ state) +
      theme_bw() +
      labs(title = paste0("Geographic Representation of quantity by ", input$variable))
  })
  #create pie chart for sales by selected variable:
  output$pie <- renderPlot({
    ggplot(data3,aes_string(x="1", y=data1$quantity, fill=input$variable))+
      geom_bar(width=1, stat="identity") +
      coord_polar("y", start=0) +
      theme_void() +
      labs(title = paste0("Pie Chart of ", input$variable))
  })
  #explore profit by volume:
  output$profitQuant <- renderPlot({
    ggplot(data3, aes_string(x=data3$quantity, y=data3$profit, color=input$variable))+
      geom_point() +
      labs(title = paste0("Is Profit Impacted by Order Size, Categorized by ", input$variable),
           x = "Purchase Item Quantity",
           y = "Profit")
  })
  output$profitYear <- renderPlot({
    data2 <- data3 |>
      mutate(year = year(orderDate)) |>
      filter(year == input$yearProfit)
  ggplot(data = data2, mapping = aes_string(x=input$variable, y=data2$profit)) +
    geom_violin() +
    labs(title = paste0("Profit across ", input$variable, " by ", input$yearProfit),
         x = input$variable,
         y = "Profit")
  })
output$downloadData <- downloadHandler(
 filename = function() {
  paste('data-', Sys.Date(), '.csv', sep='')
},
  content = function(con) {
     write.csv(data1, con)
   }
 )
}
 
# Run the application 
shinyApp(ui = ui, server = server)
