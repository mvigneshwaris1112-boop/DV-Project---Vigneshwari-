```r
# ============================================================
# ONLINE RETAIL TRANSACTION DASHBOARD
# ============================================================

# ------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)
library(scales)


# ------------------------------------------------------------
# 2. LOAD CSV DATA
# ------------------------------------------------------------

retail <- read.csv(
  "C:/Users/91934/Downloads/archive (1)/Online_Retail_Transactions.csv",
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 3. CONVERT DATE COLUMNS
# ------------------------------------------------------------

retail$Order_Date <- as.Date(retail$Order_Date)

retail$Delivery_Date <- as.Date(retail$Delivery_Date)


# ------------------------------------------------------------
# 4. USER INTERFACE
# ------------------------------------------------------------

ui <- dashboardPage(
  
  # ==========================================================
  # HEADER
  # ==========================================================
  
  dashboardHeader(
    title = "Online Retail Dashboard"
  ),
  
  
  # ==========================================================
  # SIDEBAR
  # ==========================================================
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Product Analysis",
        tabName = "products",
        icon = icon("box")
      ),
      
      menuItem(
        "Transactions",
        tabName = "transactions",
        icon = icon("table")
      )
      
    ),
    
    br(),
    
    h4(
      "FILTERS",
      style = "padding-left: 15px;"
    ),
    
    dateRangeInput(
      inputId = "date",
      label = "Order Date",
      start = min(retail$Order_Date, na.rm = TRUE),
      end = max(retail$Order_Date, na.rm = TRUE)
    ),
    
    selectInput(
      inputId = "category",
      label = "Category",
      choices = c(
        "All",
        sort(unique(retail$Category))
      ),
      selected = "All"
    ),
    
    selectInput(
      inputId = "city",
      label = "City",
      choices = c(
        "All",
        sort(unique(retail$City))
      ),
      selected = "All"
    ),
    
    selectInput(
      inputId = "payment",
      label = "Payment Method",
      choices = c(
        "All",
        sort(unique(retail$Payment_Method))
      ),
      selected = "All"
    )
    
  ),
  
  
  # ==========================================================
  # DASHBOARD BODY
  # ==========================================================
  
  dashboardBody(
    
    # --------------------------------------------------------
    # CUSTOM CSS
    # --------------------------------------------------------
    
    tags$head(
      
      tags$style(
        
        HTML(
          
          "
          .content-wrapper {
            background-color: #f4f6f9;
          }

          .box {
            border-radius: 10px;
          }

          .small-box {
            border-radius: 10px;
          }

          .dashboard-title {
            font-size: 28px;
            font-weight: bold;
          }

          .dashboard-subtitle {
            color: #666666;
            margin-bottom: 20px;
          }
          "
          
        )
        
      )
      
    ),
    
    
    # ========================================================
    # DASHBOARD TABS
    # ========================================================
    
    tabItems(
      
      
      # ======================================================
      # TAB 1 - OVERVIEW
      # ======================================================
      
      tabItem(
        
        tabName = "overview",
        
        h2(
          class = "dashboard-title",
          "Online Retail Transaction Dashboard"
        ),
        
        p(
          class = "dashboard-subtitle",
          "Interactive Sales and Transaction Analytics"
        ),
        
        
        # ----------------------------------------------------
        # KPI CARDS
        # ----------------------------------------------------
        
        fluidRow(
          
          valueBoxOutput(
            outputId = "revenue",
            width = 3
          ),
          
          valueBoxOutput(
            outputId = "orders",
            width = 3
          ),
          
          valueBoxOutput(
            outputId = "units",
            width = 3
          ),
          
          valueBoxOutput(
            outputId = "avg_order",
            width = 3
          )
          
        ),
        
        
        # ----------------------------------------------------
        # REVENUE + CATEGORY
        # ----------------------------------------------------
        
        fluidRow(
          
          box(
            
            title = "Monthly Revenue Trend",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            width = 7,
            
            plotlyOutput(
              outputId = "revenue_trend",
              height = "350px"
            )
            
          ),
          
          box(
            
            title = "Revenue by Category",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            width = 5,
            
            plotlyOutput(
              outputId = "category_revenue",
              height = "350px"
            )
            
          )
          
        ),
        
        
        # ----------------------------------------------------
        # CITY + PAYMENT
        # ----------------------------------------------------
        
        fluidRow(
          
          box(
            
            title = "Revenue by City",
            
            status = "info",
            
            solidHeader = TRUE,
            
            width = 7,
            
            plotlyOutput(
              outputId = "city_revenue",
              height = "350px"
            )
            
          ),
          
          box(
            
            title = "Payment Method Distribution",
            
            status = "info",
            
            solidHeader = TRUE,
            
            width = 5,
            
            plotlyOutput(
              outputId = "payment_chart",
              height = "350px"
            )
            
          )
          
        )
        
      ),
      
      
      # ======================================================
      # TAB 2 - PRODUCT ANALYSIS
      # ======================================================
      
      tabItem(
        
        tabName = "products",
        
        h2(
          class = "dashboard-title",
          "Product Analysis"
        ),
        
        p(
          class = "dashboard-subtitle",
          "Explore product performance and sales patterns"
        ),
        
        
        fluidRow(
          
          box(
            
            title = "Top 10 Products by Revenue",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            width = 6,
            
            plotlyOutput(
              outputId = "top_products",
              height = "400px"
            )
            
          ),
          
          box(
            
            title = "Quantity vs Revenue",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            width = 6,
            
            plotlyOutput(
              outputId = "quantity_revenue",
              height = "400px"
            )
            
          )
          
        ),
        
        
        fluidRow(
          
          box(
            
            title = "Top 10 Products by Quantity Sold",
            
            status = "info",
            
            solidHeader = TRUE,
            
            width = 12,
            
            plotlyOutput(
              outputId = "quantity_products",
              height = "400px"
            )
            
          )
          
        )
        
      ),
      
      
      # ======================================================
      # TAB 3 - TRANSACTIONS
      # ======================================================
      
      tabItem(
        
        tabName = "transactions",
        
        h2(
          class = "dashboard-title",
          "Transaction Details"
        ),
        
        p(
          class = "dashboard-subtitle",
          "Search, filter and download transactions"
        ),
        
        
        fluidRow(
          
          box(
            
            title = "Retail Transactions",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            width = 12,
            
            downloadButton(
              outputId = "downloadData",
              label = "Download CSV"
            ),
            
            br(),
            br(),
            
            DTOutput(
              outputId = "transaction_table"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 5. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # FILTERED DATA
  # ==========================================================
  
  filtered_data <- reactive({
    
    data <- retail %>%
      
      filter(
        Order_Date >= input$date[1],
        Order_Date <= input$date[2]
      )
    
    
    # --------------------------------------------------------
    # CATEGORY FILTER
    # --------------------------------------------------------
    
    if (input$category != "All") {
      
      data <- data %>%
        filter(
          Category == input$category
        )
      
    }
    
    
    # --------------------------------------------------------
    # CITY FILTER
    # --------------------------------------------------------
    
    if (input$city != "All") {
      
      data <- data %>%
        filter(
          City == input$city
        )
      
    }
    
    
    # --------------------------------------------------------
    # PAYMENT FILTER
    # --------------------------------------------------------
    
    if (input$payment != "All") {
      
      data <- data %>%
        filter(
          Payment_Method == input$payment
        )
      
    }
    
    
    return(data)
    
  })
  
  
  # ==========================================================
  # KPI 1 - TOTAL REVENUE
  # ==========================================================
  
  output$revenue <- renderValueBox({
    
    total_revenue <- sum(
      filtered_data()$Total_Amount_INR,
      na.rm = TRUE
    )
    
    
    valueBox(
      
      value = paste0(
        "₹",
        comma(
          round(total_revenue, 0)
        )
      ),
      
      subtitle = "Total Revenue",
      
      icon = icon("money-bill"),
      
      color = "aqua"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI 2 - TOTAL ORDERS
  # ==========================================================
  
  output$orders <- renderValueBox({
    
    total_orders <- n_distinct(
      filtered_data()$Order_ID
    )
    
    
    valueBox(
      
      value = comma(
        total_orders
      ),
      
      subtitle = "Total Orders",
      
      icon = icon("shopping-cart"),
      
      color = "green"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI 3 - UNITS SOLD
  # ==========================================================
  
  output$units <- renderValueBox({
    
    total_units <- sum(
      filtered_data()$Quantity,
      na.rm = TRUE
    )
    
    
    valueBox(
      
      value = comma(
        total_units
      ),
      
      subtitle = "Units Sold",
      
      icon = icon("boxes-stacked"),
      
      color = "yellow"
      
    )
    
  })
  
  
  # ==========================================================
  # KPI 4 - AVERAGE ORDER VALUE
  # ==========================================================
  
  output$avg_order <- renderValueBox({
    
    data <- filtered_data()
    
    
    total_revenue <- sum(
      data$Total_Amount_INR,
      na.rm = TRUE
    )
    
    
    total_orders <- n_distinct(
      data$Order_ID
    )
    
    
    avg_value <- ifelse(
      total_orders > 0,
      total_revenue / total_orders,
      0
    )
    
    
    valueBox(
      
      value = paste0(
        "₹",
        comma(
          round(avg_value, 0
          )
        )
      ),
      
      subtitle = "Average Order Value",
      
      icon = icon("chart-line"),
      
      color = "purple"
      
    )
    
  })
  
  
  # ==========================================================
  # MONTHLY REVENUE TREND
  # ==========================================================
  
  output$revenue_trend <- renderPlotly({
    
    data <- filtered_data() %>%
      
      mutate(
        Month = floor_date(
          Order_Date,
          unit = "month"
        )
      ) %>%
      
      group_by(Month) %>%
      
      summarise(
        Revenue = sum(
          Total_Amount_INR,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    
    plot_ly(
      
      data = data,
      
      x = ~Month,
      
      y = ~Revenue,
      
      type = "scatter",
      
      mode = "lines+markers",
      
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Revenue: ₹%{y:,.0f}",
        "<extra></extra>"
      )
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Month"
        ),
        
        yaxis = list(
          title = "Revenue (₹)"
        ),
        
        hovermode = "x unified"
        
      )
    
  })
  
  
  # ==========================================================
  # REVENUE BY CATEGORY
  # ==========================================================
  
  output$category_revenue <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Category) %>%
      
      summarise(
        Revenue = sum(
          Total_Amount_INR,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      arrange(Revenue)
    
    
    plot_ly(
      
      data = data,
      
      x = ~Revenue,
      
      y = ~Category,
      
      type = "bar",
      
      orientation = "h",
      
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Revenue: ₹%{x:,.0f}",
        "<extra></extra>"
      )
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Revenue (₹)"
        ),
        
        yaxis = list(
          title = ""
        )
        
      )
    
  })
  
  
  # ==========================================================
  # REVENUE BY CITY
  # ==========================================================
  
  output$city_revenue <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(City) %>%
      
      summarise(
        Revenue = sum(
          Total_Amount_INR,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      arrange(Revenue)
    
    
    plot_ly(
      
      data = data,
      
      x = ~Revenue,
      
      y = ~reorder(City, Revenue),
      
      type = "bar",
      
      orientation = "h",
      
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Revenue: ₹%{x:,.0f}",
        "<extra></extra>"
      )
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Revenue (₹)"
        ),
        
        yaxis = list(
          title = ""
        )
        
      )
    
  })
  
  
  # ==========================================================
  # PAYMENT METHOD DISTRIBUTION
  # ==========================================================
  
  output$payment_chart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Payment_Method) %>%
      
      summarise(
        Orders = n(),
        .groups = "drop"
      )
    
    
    plot_ly(
      
      data = data,
      
      labels = ~Payment_Method,
      
      values = ~Orders,
      
      type = "pie",
      
      hole = 0.5,
      
      textinfo = "label+percent",
      
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Orders: %{value}",
        "<extra></extra>"
      )
      
    )
    
  })
  
  
  # ==========================================================
  # TOP 10 PRODUCTS BY REVENUE
  # ==========================================================
  
  output$top_products <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Product_Name) %>%
      
      summarise(
        Revenue = sum(
          Total_Amount_INR,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      arrange(desc(Revenue)) %>%
      
      slice_head(n = 10) %>%
      
      arrange(Revenue)
    
    
    plot_ly(
      
      data = data,
      
      x = ~Revenue,
      
      y = ~reorder(Product_Name, Revenue),
      
      type = "bar",
      
      orientation = "h",
      
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Revenue: ₹%{x:,.0f}",
        "<extra></extra>"
      )
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Revenue (₹)"
        ),
        
        yaxis = list(
          title = ""
        )
        
      )
    
  })
  
  
  # ==========================================================
  # QUANTITY VS REVENUE
  # ==========================================================
  
  output$quantity_revenue <- renderPlotly({
    
    data <- filtered_data()
    
    
    plot_ly(
      
      data = data,
      
      x = ~Quantity,
      
      y = ~Total_Amount_INR,
      
      type = "scatter",
      
      mode = "markers",
      
      color = ~Category,
      
      text = ~Product_Name,
      
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "Quantity: %{x}<br>",
        "Revenue: ₹%{y:,.0f}",
        "<extra></extra>"
      )
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Quantity"
        ),
        
        yaxis = list(
          title = "Revenue (₹)"
        )
        
      )
    
  })
  
  
  # ==========================================================
  # TOP 10 PRODUCTS BY QUANTITY
  # ==========================================================
  
  output$quantity_products <- renderPlotly({
    
    data <- filtered_data() %>%
      
      group_by(Product_Name) %>%
      
      summarise(
        Quantity = sum(
          Quantity,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      arrange(desc(Quantity)) %>%
      
      slice_head(n = 10) %>%
      
      arrange(Quantity)
    
    
    plot_ly(
      
      data = data,
      
      x = ~Quantity,
      
      y = ~reorder(Product_Name, Quantity),
      
      type = "bar",
      
      orientation = "h",
      
      hovertemplate = paste(
        "<b>%{y}</b><br>",
        "Quantity Sold: %{x}",
        "<extra></extra>"
      )
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Quantity Sold"
        ),
        
        yaxis = list(
          title = ""
        )
        
      )
    
  })
  
  
  # ==========================================================
  # TRANSACTION TABLE
  # ==========================================================
  
  output$transaction_table <- renderDT({
    
    filtered_data() %>%
      
      select(
        Order_ID,
        Product_Name,
        Category,
        City,
        Order_Date,
        Quantity,
        Unit_Price_INR,
        Total_Amount_INR,
        Payment_Method,
        Delivery_Date
      ) %>%
      
      datatable(
        
        extensions = "Buttons",
        
        options = list(
          
          pageLength = 10,
          
          scrollX = TRUE,
          
          dom = "Bfrtip",
          
          buttons = c(
            "copy",
            "csv",
            "excel"
          )
          
        ),
        
        filter = "top",
        
        rownames = FALSE
        
      )
    
  })
  
  
  # ==========================================================
  # DOWNLOAD FILTERED DATA
  # ==========================================================
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Online_Retail_Filtered_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}


# ============================================================
# 6. RUN THE SHINY APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
```
