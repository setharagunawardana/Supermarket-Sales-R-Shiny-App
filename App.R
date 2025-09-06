library(shiny)
library(dplyr)
library(ggplot2)

# Load the dataset
sales_data <- read.csv("supermarket_sales.csv")


# Convert Date column to Date type
sales_data$Date <- as.Date(sales_data$Date, format = "%m/%d/%Y")


# Define UI for the application
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .title-panel {
          background-color: #00008B;
          color: white;
          padding: 10px;
          text-align: center;
          font-size: 30px;
          font-weight: bold;
        }
        .description {
          background-color: #F0F0F0;
          padding: 10px;
          border-radius: 10px;
          margin: 10px 0;
          font-size: 20px;
          font-style: italic;
        }
        .image-container {
          text-align: center;
          margin: 20px;
          padding: 10px;
        }
        .attributes-container {
          background-color: #F8F8F8;
          padding: 10px;
          border-radius: 10px;
          margin: 10px;
          font-size: 16px;
        }
        .plot-container {
          background-color: #FFFFFF;
          padding: 20px;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }
      ")
    )
  ),
  
  titlePanel(
    div("Supermarket Sales Analysis", class = "title-panel")
  ),
  
  tabsetPanel(
    tabPanel("Overview",
             fluidRow(
               div(
                 class = "description",
                 h3("Description", style = "color:#000080;"),
                 "The growth of supermarkets in most populated cities is increasing and market competitions are also high. This dataset is one of the historical sales records of a supermarket company, which has been recorded in 3 different branches over 3 months. Predictive data analytics methods are easy to apply with this dataset.",
               
                 h4("Dataset Source: Kaggle – Supermarket Sales Dataset (2019)."),
                 tags$a(href = "https://www.kaggle.com/code/aryantiwari123/supermarket-sales-prediction", 
                        "View on Kaggle", target = "_blank"),
                 h4("This dataset contains historical sales transactions from 3 branches in Myanmar.")
                 
               )
             ),
             fluidRow(
               column(6,
                      div(
                        class = "image-container",
                        img(src = "financial_graph.png", alt = "Financial Graph Image", width = "100%")
                      )
               ),
               column(6,
                      div(
                        class = "attributes-container",
                        h4("Attribute Information" , style = "color:#000080") ,
                        tags$ul(
                          tags$li("Invoice id: Computer generated sales slip invoice identification number"),
                          tags$li("Branch: Branch of supercenter (3 branches are available identified by A, B and C)"),
                          tags$li("City: Location of supercenters"),
                          tags$li("Customer type: Type of customers, recorded by Members for customers using member card and Normal for without member card"),
                          tags$li("Gender: Gender type of customer"),
                          tags$li("Product line: General item categorization groups - Electronic accessories, Fashion accessories, Food and beverages, Health and beauty, Home and lifestyle, Sports and travel"),
                          tags$li("Unit price: Price of each product in $"),
                          tags$li("Quantity: Number of products purchased by customer"),
                          tags$li("Tax: 5% tax fee for customer buying"),
                          tags$li("Total: Total price including tax"),
                          tags$li("Date: Date of purchase (Record available from January 2019 to March 2019)"),
                          tags$li("Time: Purchase time (10am to 9pm)"),
                          tags$li("Payment: Payment used by customer for purchase (3 methods are available – Cash, Credit card and Ewallet)"),
                          tags$li("COGS: Cost of goods sold"),
                          tags$li("Gross margin percentage: Gross margin percentage"),
                          tags$li("Gross income: Gross income"),
                          tags$li("Rating: Customer stratification rating on their overall shopping experience (On a scale of 1 to 10)")
                        )
                      )
               )
             )
    ),
    tabPanel("Data Quality",
             h3("Data Quality Checks", style = "color:#000080;"),
             fluidRow(
               column(4,
                      h4("Missing Values"),
                      tableOutput("missingValues"),
                      verbatimTextOutput("missingValues")),
               column(4,
                      h4("Duplicate Records"),
                      textOutput("duplicateRecords"),
                      verbatimTextOutput("duplicateRecords")),
               column(4,
                      h4("Outlier Counts"),
                      tableOutput("outlierCounts"),
                      verbatimTextOutput("outlierCounts"))
             )
    ),
    tabPanel("Time Plots",
             h4("This time series plot shows how sales metrics change across the selected date range."),
             sidebarLayout(
               sidebarPanel(
                 helpText("Select a date range to filter the sales data."),
                 dateRangeInput("dateRange", "Select Date Range:",
                                start = min(sales_data$Date),
                                end = max(sales_data$Date),
                                min = min(sales_data$Date),
                                max = max(sales_data$Date)),
                 helpText("Choose a variable to plot on the Y-axis for time series analysis."),
                 selectInput("yvar", "Select Y-axis Variable:",
                             choices = c("Unit price" = "Unit.price", "Quantity", "Tax 5%" = "Tax.5.", "Total", "cogs", "gross income" = "gross.income", "Rating"))
               ),
               mainPanel(
                 div(
                   class = "plot-container",
                   plotOutput("timeSeriesPlot")
                 )
               )
             )
    ),
    tabPanel("Univariate Analysis",
             h3("Categorical Variables", style = "color:#000080;"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Choose a categorical variable to view its distribution."),
                 selectInput("cat_var", "Select Categorical Variable:",
                             choices = c("Branch", "City", "Customer type" = "Customer.type", "Gender", "Product line" = "Product.line", "Payment")) ,
                 tableOutput("catSummary")
                 ),
               mainPanel(
                 plotOutput("pieChart"),
                 p("This pie chart shows the percentage distribution of the selected categorical variable.")
               )
             ),
             h3("Continuous Variables", style = "color:#000080;"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Choose a continuous variable to explore its spread and outliers."),
                 selectInput("cont_var", "Select Continuous Variable:",
                             choices = c("Unit price" = "Unit.price", "Quantity", "Tax 5%" = "Tax.5.", "Total", "cogs", "gross income" = "gross.income", "Rating")) ,
                 verbatimTextOutput("summaryStats")
                 ),
               mainPanel(
                 plotOutput("boxPlot"),
                 p("This box plot illustrates the spread, median, and potential outliers for the selected continuous variable.")
               )
             )
    ),
    tabPanel("Bivariate Analysis",
             h3("Scatter Plots", style = "color:#000080;"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Select two continuous variables to explore their relationship."),
                 selectInput("xvar", "Select X-axis Variable:",
                             choices = c("Unit price" = "Unit.price", "Quantity", "Tax 5%" = "Tax.5.", "Total", "cogs", "gross income" = "gross.income", "Rating")),
                 selectInput("yvar_bi", "Select Y-axis Variable:",
                             choices = c("Unit price" = "Unit.price", "Quantity", "Tax 5%" = "Tax.5.", "Total", "cogs", "gross income" = "gross.income", "Rating"))
               ),
               mainPanel(
                 div(
                   class = "plot-container",
                   plotOutput("scatterPlot"),
                   p("This scatter plot helps to visualize the relationship between two continuous sales variables.")
                 )
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    sales_data %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })
  
  # Missing values
  output$missingValues <- renderPrint({
    colSums(is.na(sales_data))
  })
  
  # Duplicate records
  output$duplicateRecords <- renderPrint({
    paste("Total duplicate rows:", sum(duplicated(sales_data)))
  })
  
  # Outlier detection (IQR method, numeric variables)
  output$outlierCounts <- renderPrint({
    num_cols <- sapply(sales_data, is.numeric)
    outlier_counts <- sapply(sales_data[, num_cols], function(col) {
      Q1 <- quantile(col, 0.25, na.rm = TRUE)
      Q3 <- quantile(col, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      sum(col < lower | col > upper, na.rm = TRUE)
    })
    outlier_counts
  })
  
  
  output$timeSeriesPlot <- renderPlot({
    yvar <- input$yvar
    ggplot(filtered_data(), aes(x = Date, y = .data[[yvar]])) +
      geom_line(color="#000080") +
      theme_minimal() +
      labs(title = paste("Time Series Plot of", input$yvar),
           x = "Date",
           y = input$yvar) +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  output$pieChart <- renderPlot({
    cat_var <- input$cat_var
    data <- sales_data %>%
      count(.data[[cat_var]]) %>%
      mutate(percentage = n / sum(n) * 100)
    
    ggplot(data, aes(x = "", y = percentage, fill = .data[[cat_var]])) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_minimal() +
      labs(title = paste("Pie Chart of", cat_var),
           fill = cat_var,
           x = NULL,
           y = NULL) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5)) +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(size = 20, face = "bold"))
  })
  output$catSummary <- renderTable({
    cat_var <- input$cat_var
    data <- sales_data %>%
      count(.data[[cat_var]]) %>%
      mutate(percentage = n / sum(n) * 100)
    
    data
  })
  
  output$boxPlot <- renderPlot({
    cont_var <- input$cont_var
    ggplot(sales_data, aes(x = "", y = .data[[cont_var]])) +
      geom_boxplot(fill = "#000080", color = "black") +
      theme_minimal() +
      labs(title = paste("Box Plot of", cont_var),
           x = NULL,
           y = cont_var) +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  output$summaryStats <- renderPrint({
    cont_var <- input$cont_var
    summary(sales_data[[cont_var]])
  })
  output$scatterPlot <- renderPlot({
    xvar <- input$xvar
    yvar_bi <- input$yvar_bi
    ggplot(sales_data, aes(x = .data[[xvar]], y = .data[[yvar_bi]])) +
      geom_point(color = "#000080", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Scatter Plot of", xvar, "vs", yvar_bi),
           x = xvar,
           y = yvar_bi) +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)