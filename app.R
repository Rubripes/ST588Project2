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
library(ggpie)


#data are stored in same folder as app.  Read dataset for app.
data<-read_excel("./US Superstore data.xls")

#data$names are reformatted for programmatic consistency, and are coerced to Factors as appropriate
factors<-c("shipMode", "segment", "country", "state", "postalCode", "region", "category", "subCategory")
data1 <- data |>
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

    # Sidebar with a select dropdown to select Factor to explore frequencies for different factors
sidebarLayout(
  sidebarPanel(
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
                  "Month" = "month"))
    ),
  
    mainPanel(
      tableOutput("contingency"),
      plotOutput("contingencyPlot"),
      tableOutput("regionPlot"),
      tableOutput("categoricalProfits"),
      plotOutput("salesVProfit"),
      plotOutput("stateSales"),
      plotOutput("pie")
    )
  )
)


# Define server logic:
server <- function(input, output, session) {
  #1 way contingency table.  Subset (select) data using selectInput
  output$contingency <- renderTable({
    xfactor <- data1 |> 
      select(c(input$variable,
               "quantity"))
    wtd.table(xfactor[,1], weights=xfactor[,2])
  })
  #display bar graph for selected variables in 1 way contingency table above
  output$contingencyPlot <- renderPlot({
    xfactor <- data1 |> 
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
    xfactor <- data1 |> 
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
    data2 <- data1 |>
      mutate(month = month(orderDate),
               year = year(orderDate))
  ggplot(data = data2, mapping = aes_string(x=data1$discount, y=input$salesVProfit, color=input$yearVMonth)) +
    geom_point() +
    labs(x = input$data1$discount,
         y = input$salesVProfit,
         color=input$yearVMonth)
  })
  #visualize sales volume between categories across the states:
  output$stateSales <- renderPlot({
    ggplot(data = data1, aes_string(x=input$variable, y=data1$quantity, fill = input$variable)) +
      geom_col() +
      coord_flip() +
      facet_geo(~ state) +
      theme_bw()
  })
  #animate a visual of profit or sales data, as selected by user:
  output$pie <- renderPlot({
    ggplot(data1,aes_string(x="1", y=data1$quantity, fill=input$variable))+
      geom_bar(width=1, stat="identity") +
      coord_polar("y", start=0) +
      theme_void() +
      labs(title = paste0("Pie Chart of ", input$variable))
  })
}
 
# Run the application 
shinyApp(ui = ui, server = server)
