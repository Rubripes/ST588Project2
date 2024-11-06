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
    selectInput("variable",
                "Choose Category to Visualize Sales Volume by Category:",
                c("Market Segment" = "segment",
                  "Ship Mode" = "shipMode",
                  "Category" = "category")
)),
    mainPanel(
      tableOutput("contingency"),
      plotOutput("contingencyPlot")
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
  output$contingencyPlot <-renderPlot({
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
}

# Run the application 
shinyApp(ui = ui, server = server)
