# ============================================================
# Electric Vehicle Population Dashboard - R Shiny
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)

# ---- Load & Clean Data ----
df <- read.csv("C:/Users/Omsri/OneDrive/Documents/R-Files/ev_data.csv", stringsAsFactors = FALSE)

colnames(df) <- c("VIN", "County", "City", "State", "Postal_Code",
                  "Model_Year", "Brand", "Model", "EV_Type",
                  "CAFV_Eligibility", "Electric_Range", "Base_MSRP",
                  "Legislative_District", "DOL_Vehicle_ID",
                  "Vehicle_Location", "Electric_Utility", "Census_Tract")

df$Model_Year <- as.numeric(df$Model_Year)
df$Electric_Range <- as.numeric(df$Electric_Range)
df$Base_MSRP <- as.numeric(df$Base_MSRP)
df <- df %>% filter(!is.na(Model_Year), Model_Year >= 2000)

df$EV_Type_Short <- ifelse(grepl("Battery", df$EV_Type), "BEV", "PHEV")

# --- Calculated Fields ---
df$Price_Tier <- case_when(
  df$Base_MSRP <= 0 | is.na(df$Base_MSRP) ~ "Unknown",
  df$Base_MSRP < 25000 ~ "Budget (< $25K)",
  df$Base_MSRP < 45000 ~ "Mid-Range ($25K-$45K)",
  df$Base_MSRP < 70000 ~ "Premium ($45K-$70K)",
  TRUE ~ "Luxury ($70K+)"
)
df$Price_Tier <- factor(df$Price_Tier,
                        levels = c("Budget (< $25K)", "Mid-Range ($25K-$45K)",
                                   "Premium ($45K-$70K)", "Luxury ($70K+)", "Unknown"))

df$Range_Per_Dollar <- ifelse(
  df$Base_MSRP > 0 & df$Electric_Range > 0,
  round(df$Electric_Range / df$Base_MSRP * 1000, 2), NA)

# Colors
main_col <- "#2C7BB6"
second_col <- "#D7191C"
accent_col <- "#FDAE61"
bev_phev_pal <- c("BEV" = "#2C7BB6", "PHEV" = "#D7191C")
tier_pal <- c("Budget (< $25K)" = "#2C7BB6", "Mid-Range ($25K-$45K)" = "#ABD9E9",
              "Premium ($45K-$70K)" = "#FDAE61", "Luxury ($70K+)" = "#D7191C")

thm <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggp <- function(p, tip = "text") {
  ggplotly(p, tooltip = tip) %>%
    layout(margin = list(l = 10, r = 10, t = 30, b = 10))
}

min_yr <- min(df$Model_Year)
max_yr <- max(df$Model_Year)

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "EV Population Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "tab1", icon = icon("dashboard")),
      menuItem("Adoption Over Time", tabName = "tab2", icon = icon("chart-line")),
      menuItem("BEV vs PHEV", tabName = "tab3", icon = icon("balance-scale")),
      menuItem("Brand & Model", tabName = "tab4", icon = icon("car")),
      menuItem("Electric Range", tabName = "tab5", icon = icon("bolt")),
      menuItem("Pricing (MSRP)", tabName = "tab6", icon = icon("dollar-sign")),
      menuItem("County Analysis", tabName = "tab7", icon = icon("map")),
      menuItem("City Analysis", tabName = "tab8", icon = icon("city")),
      menuItem("CAFV Eligibility", tabName = "tab9", icon = icon("leaf")),
      menuItem("Model Year Deep Dive", tabName = "tab10", icon = icon("calendar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ============ TAB 1: OVERVIEW ============
      tabItem(tabName = "tab1",
              fluidRow(
                column(12, sliderInput("t1_year", "Model Year Range:",
                                       min = min_yr, max = max_yr,
                                       value = c(min_yr, max_yr), sep = ""))
              ),
              fluidRow(
                valueBoxOutput("t1_total", width = 3),
                valueBoxOutput("t1_brands", width = 3),
                valueBoxOutput("t1_models", width = 3),
                valueBoxOutput("t1_range", width = 3)
              ),
              fluidRow(
                box(title = "Top 10 Brands by Registrations", width = 12,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t1_plot1", height = "400px"))
              )
      ),
      
      # ============ TAB 2: ADOPTION OVER TIME ============
      tabItem(tabName = "tab2",
              fluidRow(
                column(4, selectInput("t2_type", "EV Type:",
                                      choices = c("Both", "BEV", "PHEV"), selected = "Both"))
              ),
              fluidRow(
                box(title = "EV Registrations by Model Year", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t2_plot1", height = "400px")),
                box(title = "BEV vs PHEV by Model Year", width = 6,
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("t2_plot2", height = "400px"))
              )
      ),
      
      # ============ TAB 3: BEV vs PHEV ============
      tabItem(tabName = "tab3",
              fluidRow(
                column(4, sliderInput("t3_year", "Model Year Range:",
                                      min = min_yr, max = max_yr,
                                      value = c(min_yr, max_yr), sep = ""))
              ),
              fluidRow(
                box(title = "BEV vs PHEV Proportion", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t3_plot1", height = "400px")),
                box(title = "Average Electric Range: BEV vs PHEV", width = 6,
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("t3_plot2", height = "400px"))
              )
      ),
      
      # ============ TAB 4: BRAND & MODEL ============
      tabItem(tabName = "tab4",
              fluidRow(
                column(4, selectInput("t4_brand", "Select Brand:",
                                      choices = sort(unique(df$Brand)), selected = "TESLA"))
              ),
              fluidRow(
                box(title = "Top 15 Brands by Registration Count", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t4_plot1", height = "400px")),
                box(title = "Top 10 Models for Selected Brand", width = 6,
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("t4_plot2", height = "400px"))
              )
      ),
      
      # ============ TAB 5: ELECTRIC RANGE ============
      tabItem(tabName = "tab5",
              fluidRow(
                column(4, selectInput("t5_type", "EV Type:",
                                      choices = c("Both", "BEV", "PHEV"), selected = "Both")),
                column(4, sliderInput("t5_year", "Model Year Range:",
                                      min = min_yr, max = max_yr,
                                      value = c(min_yr, max_yr), sep = ""))
              ),
              fluidRow(
                box(title = "Electric Range Distribution", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t5_plot1", height = "400px")),
                box(title = "Range Efficiency: Miles per $1,000 by Top 10 Brands",
                    width = 6, status = "success", solidHeader = TRUE,
                    plotlyOutput("t5_plot2", height = "400px"))
              )
      ),
      
      # ============ TAB 6: PRICING ============
      tabItem(tabName = "tab6",
              fluidRow(
                column(4, selectInput("t6_type", "EV Type:",
                                      choices = c("Both", "BEV", "PHEV"), selected = "Both")),
                column(4, selectizeInput("t6_brands", "Filter Brands:",
                                         choices = sort(unique(df$Brand)), multiple = TRUE,
                                         selected = NULL, options = list(placeholder = "All Brands")))
              ),
              fluidRow(
                box(title = "EV Count by Price Tier", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t6_plot1", height = "400px")),
                box(title = "Avg MSRP by Top 10 Brands", width = 6,
                    status = "danger", solidHeader = TRUE,
                    plotlyOutput("t6_plot2", height = "400px"))
              )
      ),
      
      # ============ TAB 7: COUNTY ============
      tabItem(tabName = "tab7",
              fluidRow(
                column(4, selectInput("t7_type", "EV Type:",
                                      choices = c("Both", "BEV", "PHEV"), selected = "Both"))
              ),
              fluidRow(
                box(title = "Top 15 Counties by EV Registrations", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t7_plot1", height = "400px")),
                box(title = "Top 15 Counties by Avg Electric Range", width = 6,
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("t7_plot2", height = "400px"))
              )
      ),
      
      # ============ TAB 8: CITY ============
      tabItem(tabName = "tab8",
              fluidRow(
                column(4, selectInput("t8_county", "Select County:",
                                      choices = c("All", sort(unique(df$County))), selected = "All"))
              ),
              fluidRow(
                box(title = "Top 20 Cities by EV Count", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t8_plot1", height = "450px")),
                box(title = "BEV vs PHEV Split - Top 10 Cities", width = 6,
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("t8_plot2", height = "450px"))
              )
      ),
      
      # ============ TAB 9: CAFV ============
      tabItem(tabName = "tab9",
              fluidRow(
                column(4, sliderInput("t9_year", "Model Year Range:",
                                      min = min_yr, max = max_yr,
                                      value = c(min_yr, max_yr), sep = ""))
              ),
              fluidRow(
                box(title = "CAFV Eligibility Distribution", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t9_plot1", height = "400px")),
                box(title = "CAFV Eligibility by Top 10 Brands", width = 6,
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("t9_plot2", height = "400px"))
              )
      ),
      
      # ============ TAB 10: MODEL YEAR DEEP DIVE ============
      tabItem(tabName = "tab10",
              fluidRow(
                column(4, selectInput("t10_type", "EV Type:",
                                      choices = c("Both", "BEV", "PHEV"), selected = "Both")),
                column(4, selectInput("t10_brand", "Filter by Brand:",
                                      choices = c("All", sort(unique(df$Brand))), selected = "All"))
              ),
              fluidRow(
                box(title = "Unique Models Available per Year", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("t10_plot1", height = "400px")),
                box(title = "Avg Range Improvement Over Years", width = 6,
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("t10_plot2", height = "400px"))
              )
      )
      
    )
  )
)


# ---- SERVER ----
server <- function(input, output, session) {
  
  # ========== TAB 1: OVERVIEW ==========
  t1_data <- reactive({
    df %>% filter(Model_Year >= input$t1_year[1], Model_Year <= input$t1_year[2])
  })
  
  output$t1_total <- renderValueBox({
    valueBox(format(nrow(t1_data()), big.mark = ","), "Total EVs",
             icon = icon("car"), color = "blue")
  })
  output$t1_brands <- renderValueBox({
    valueBox(n_distinct(t1_data()$Brand), "Unique Brands",
             icon = icon("industry"), color = "green")
  })
  output$t1_models <- renderValueBox({
    valueBox(n_distinct(t1_data()$Model), "Unique Models",
             icon = icon("list"), color = "yellow")
  })
  output$t1_range <- renderValueBox({
    r <- t1_data() %>% filter(Electric_Range > 0) %>%
      pull(Electric_Range) %>% mean(na.rm = TRUE)
    valueBox(paste0(round(r, 1), " mi"), "Avg Range",
             icon = icon("bolt"), color = "red")
  })
  
  output$t1_plot1 <- renderPlotly({
    top <- t1_data() %>% count(Brand, sort = TRUE) %>% head(10)
    p <- ggplot(top, aes(x = reorder(Brand, n), y = n,
                         text = paste("Brand:", Brand, "<br>Count:", comma(n)))) +
      geom_col(fill = main_col) +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Registrations") + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  # ========== TAB 2: ADOPTION OVER TIME ==========
  t2_data <- reactive({
    d <- df
    if (input$t2_type != "Both") d <- d %>% filter(EV_Type_Short == input$t2_type)
    d
  })
  
  output$t2_plot1 <- renderPlotly({
    yr <- t2_data() %>% count(Model_Year)
    p <- ggplot(yr, aes(Model_Year, n, group = 1,
                        text = paste("Year:", Model_Year, "<br>Count:", comma(n)))) +
      geom_line(color = main_col, linewidth = 1.2) +
      geom_point(color = main_col, size = 2.5) +
      scale_y_continuous(labels = comma) +
      labs(x = "Model Year", y = "Registrations") + thm
    ggp(p)
  })
  
  output$t2_plot2 <- renderPlotly({
    yr <- df %>% count(Model_Year, EV_Type_Short)
    p <- ggplot(yr, aes(Model_Year, n, fill = EV_Type_Short,
                        text = paste("Year:", Model_Year, "<br>Type:", EV_Type_Short,
                                     "<br>Count:", comma(n)))) +
      geom_col() +
      scale_fill_manual(values = bev_phev_pal) +
      scale_y_continuous(labels = comma) +
      labs(x = "Model Year", y = "Registrations", fill = NULL) + thm +
      theme(legend.position = "top")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.3, y = 1.15),
             margin = list(l = 60, r = 10, t = 50, b = 60))
  })
  
  # ========== TAB 3: BEV vs PHEV ==========
  t3_data <- reactive({
    df %>% filter(Model_Year >= input$t3_year[1], Model_Year <= input$t3_year[2])
  })
  
  output$t3_plot1 <- renderPlotly({
    ct <- t3_data() %>% count(EV_Type_Short)
    plot_ly(ct, labels = ~EV_Type_Short, values = ~n, type = "pie",
            hole = 0.5, marker = list(colors = unname(bev_phev_pal)),
            textinfo = "label+percent", hoverinfo = "label+value+percent") %>%
      layout(showlegend = TRUE, margin = list(l = 10, r = 10, t = 30, b = 10))
  })
  
  output$t3_plot2 <- renderPlotly({
    avg <- t3_data() %>% filter(Electric_Range > 0) %>%
      group_by(EV_Type_Short) %>%
      summarise(avg_range = round(mean(Electric_Range, na.rm = TRUE), 1))
    p <- ggplot(avg, aes(EV_Type_Short, avg_range, fill = EV_Type_Short,
                         text = paste("Type:", EV_Type_Short,
                                      "<br>Avg Range:", avg_range, "mi"))) +
      geom_col(width = 0.5, show.legend = FALSE) +
      scale_fill_manual(values = bev_phev_pal) +
      labs(x = NULL, y = "Avg Electric Range (miles)") + thm
    ggp(p)
  })
  
  # ========== TAB 4: BRAND & MODEL ==========
  output$t4_plot1 <- renderPlotly({
    top <- df %>% count(Brand, sort = TRUE) %>% head(15)
    p <- ggplot(top, aes(x = reorder(Brand, n), y = n,
                         text = paste("Brand:", Brand, "<br>Count:", comma(n)))) +
      geom_col(fill = main_col) +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Registrations") + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  output$t4_plot2 <- renderPlotly({
    d <- df %>% filter(Brand == input$t4_brand) %>% count(Model, sort = TRUE) %>% head(10)
    p <- ggplot(d, aes(x = reorder(Model, n), y = n,
                       text = paste("Model:", Model, "<br>Count:", comma(n)))) +
      geom_col(fill = accent_col) +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Registrations",
           title = paste("Top Models -", input$t4_brand)) + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  # ========== TAB 5: ELECTRIC RANGE ==========
  t5_data <- reactive({
    d <- df %>% filter(Model_Year >= input$t5_year[1], Model_Year <= input$t5_year[2],
                       Electric_Range > 0)
    if (input$t5_type != "Both") d <- d %>% filter(EV_Type_Short == input$t5_type)
    d
  })
  
  output$t5_plot1 <- renderPlotly({
    p <- ggplot(t5_data(), aes(Electric_Range)) +
      geom_histogram(fill = main_col, color = "white", bins = 40) +
      scale_y_continuous(labels = comma) +
      labs(x = "Electric Range (miles)", y = "Count") + thm
    ggp(p, tip = NULL)
  })
  
  output$t5_plot2 <- renderPlotly({
    top_brands <- t5_data() %>% filter(!is.na(Range_Per_Dollar)) %>%
      count(Brand, sort = TRUE) %>% head(10) %>% pull(Brand)
    avg <- t5_data() %>% filter(Brand %in% top_brands, !is.na(Range_Per_Dollar)) %>%
      group_by(Brand) %>%
      summarise(avg_rpd = round(mean(Range_Per_Dollar, na.rm = TRUE), 2))
    p <- ggplot(avg, aes(x = reorder(Brand, avg_rpd), y = avg_rpd,
                         text = paste("Brand:", Brand,
                                      "<br>Miles per $1K:", avg_rpd))) +
      geom_col(fill = main_col) +
      coord_flip() +
      labs(x = NULL, y = "Miles per $1,000 (Range/MSRP)") + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  # ========== TAB 6: PRICING ==========
  t6_data <- reactive({
    d <- df %>% filter(Base_MSRP > 0)
    if (input$t6_type != "Both") d <- d %>% filter(EV_Type_Short == input$t6_type)
    if (length(input$t6_brands) > 0) d <- d %>% filter(Brand %in% input$t6_brands)
    d
  })
  
  output$t6_plot1 <- renderPlotly({
    d <- t6_data() %>% filter(Price_Tier != "Unknown") %>% count(Price_Tier)
    p <- ggplot(d, aes(x = Price_Tier, y = n, fill = Price_Tier,
                       text = paste("Tier:", Price_Tier, "<br>Count:", comma(n)))) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = tier_pal) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Number of EVs") + thm
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE, margin = list(l = 60, r = 10, t = 30, b = 100))
  })
  
  output$t6_plot2 <- renderPlotly({
    top_brands <- t6_data() %>% count(Brand, sort = TRUE) %>% head(10) %>% pull(Brand)
    avg <- t6_data() %>% filter(Brand %in% top_brands) %>%
      group_by(Brand) %>%
      summarise(avg_p = round(mean(Base_MSRP, na.rm = TRUE), 0))
    p <- ggplot(avg, aes(x = reorder(Brand, avg_p), y = avg_p,
                         text = paste("Brand:", Brand,
                                      "<br>Avg MSRP: $", comma(avg_p)))) +
      geom_col(fill = second_col) +
      coord_flip() +
      scale_y_continuous(labels = dollar) +
      labs(x = NULL, y = "Avg MSRP ($)") + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  # ========== TAB 7: COUNTY ==========
  t7_data <- reactive({
    d <- df
    if (input$t7_type != "Both") d <- d %>% filter(EV_Type_Short == input$t7_type)
    d
  })
  
  output$t7_plot1 <- renderPlotly({
    top <- t7_data() %>% count(County, sort = TRUE) %>% head(15)
    p <- ggplot(top, aes(x = reorder(County, n), y = n,
                         text = paste("County:", County, "<br>Count:", comma(n)))) +
      geom_col(fill = main_col) +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "EV Registrations") + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  output$t7_plot2 <- renderPlotly({
    avg <- t7_data() %>% filter(Electric_Range > 0) %>%
      group_by(County) %>% filter(n() >= 100) %>%
      summarise(avg_r = round(mean(Electric_Range, na.rm = TRUE), 1)) %>%
      arrange(desc(avg_r)) %>% head(15)
    p <- ggplot(avg, aes(x = reorder(County, avg_r), y = avg_r,
                         text = paste("County:", County,
                                      "<br>Avg Range:", avg_r, "mi"))) +
      geom_col(fill = accent_col) +
      coord_flip() +
      labs(x = NULL, y = "Avg Range (miles)") + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  # ========== TAB 8: CITY ==========
  t8_data <- reactive({
    d <- df
    if (input$t8_county != "All") d <- d %>% filter(County == input$t8_county)
    d
  })
  
  output$t8_plot1 <- renderPlotly({
    top <- t8_data() %>% count(City, sort = TRUE) %>% head(20)
    p <- ggplot(top, aes(x = reorder(City, n), y = n,
                         text = paste("City:", City, "<br>Count:", comma(n)))) +
      geom_col(fill = main_col) +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "EV Count") + thm +
      theme(panel.grid.major.x = element_line(color = "grey90"))
    ggp(p)
  })
  
  output$t8_plot2 <- renderPlotly({
    top_cities <- t8_data() %>% count(City, sort = TRUE) %>% head(10) %>% pull(City)
    d <- t8_data() %>% filter(City %in% top_cities) %>% count(City, EV_Type_Short)
    p <- ggplot(d, aes(x = reorder(City, -n), y = n, fill = EV_Type_Short,
                       text = paste("City:", City, "<br>Type:", EV_Type_Short,
                                    "<br>Count:", comma(n)))) +
      geom_col() +
      scale_fill_manual(values = bev_phev_pal) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Count", fill = NULL) + thm +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.3, y = 1.15),
             margin = list(l = 60, r = 10, t = 50, b = 80))
  })
  
  # ========== TAB 9: CAFV ==========
  t9_data <- reactive({
    df %>% filter(Model_Year >= input$t9_year[1], Model_Year <= input$t9_year[2])
  })
  
  output$t9_plot1 <- renderPlotly({
    ct <- t9_data() %>%
      mutate(CAFV = case_when(
        grepl("Eligible", CAFV_Eligibility) ~ "Eligible",
        grepl("Not eligible", CAFV_Eligibility) ~ "Not Eligible",
        grepl("unknown", CAFV_Eligibility) ~ "Unknown",
        TRUE ~ "Unknown"
      )) %>% count(CAFV)
    plot_ly(ct, labels = ~CAFV, values = ~n, type = "pie",
            hole = 0.5,
            marker = list(colors = c("#2C7BB6", "#FDAE61", "#D7191C")),
            textinfo = "label+percent", hoverinfo = "label+value+percent") %>%
      layout(showlegend = TRUE,
             legend = list(orientation = "h", x = 0, y = 1.15),
             margin = list(l = 20, r = 20, t = 60, b = 20))
  })
  
  output$t9_plot2 <- renderPlotly({
    top_brands <- t9_data() %>% count(Brand, sort = TRUE) %>% head(10) %>% pull(Brand)
    d <- t9_data() %>% filter(Brand %in% top_brands) %>%
      mutate(CAFV = case_when(
        grepl("Eligible", CAFV_Eligibility) ~ "Eligible",
        grepl("Not eligible", CAFV_Eligibility) ~ "Not Eligible",
        grepl("unknown", CAFV_Eligibility) ~ "Unknown",
        TRUE ~ "Unknown"
      )) %>% count(Brand, CAFV)
    p <- ggplot(d, aes(x = reorder(Brand, -n), y = n, fill = CAFV,
                       text = paste("Brand:", Brand, "<br>Status:", CAFV,
                                    "<br>Count:", comma(n)))) +
      geom_col() +
      scale_fill_manual(values = c("Eligible" = "#2C7BB6", "Not Eligible" = "#FDAE61",
                                   "Unknown" = "#D7191C")) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Count", fill = NULL) + thm +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = 1.15),
             margin = list(l = 60, r = 10, t = 50, b = 80))
  })
  
  # ========== TAB 10: MODEL YEAR DEEP DIVE ==========
  t10_data <- reactive({
    d <- df
    if (input$t10_type != "Both") d <- d %>% filter(EV_Type_Short == input$t10_type)
    if (input$t10_brand != "All") d <- d %>% filter(Brand == input$t10_brand)
    d
  })
  
  output$t10_plot1 <- renderPlotly({
    yr <- t10_data() %>% group_by(Model_Year) %>%
      summarise(n_models = n_distinct(Model))
    p <- ggplot(yr, aes(Model_Year, n_models, group = 1,
                        text = paste("Year:", Model_Year,
                                     "<br>Models:", n_models))) +
      geom_line(color = main_col, linewidth = 1.2) +
      geom_point(color = main_col, size = 2.5) +
      labs(x = "Model Year", y = "Unique Models") + thm
    ggp(p)
  })
  
  output$t10_plot2 <- renderPlotly({
    yr <- t10_data() %>% filter(Electric_Range > 0) %>%
      group_by(Model_Year) %>%
      summarise(avg_r = round(mean(Electric_Range, na.rm = TRUE), 1))
    p <- ggplot(yr, aes(Model_Year, avg_r, group = 1,
                        text = paste("Year:", Model_Year,
                                     "<br>Avg Range:", avg_r, "mi"))) +
      geom_line(color = second_col, linewidth = 1.2) +
      geom_point(color = second_col, size = 2.5) +
      labs(x = "Model Year", y = "Avg Range (miles)") + thm
    ggp(p)
  })
  
}

shinyApp(ui = ui, server = server)
