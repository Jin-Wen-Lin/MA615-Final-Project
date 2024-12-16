#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Jin Wen Lin

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)



# Population Data
population_data <- read.csv("POPTOTFJA647NWDB.csv")

population_data$observation_date <- as.Date(population_data$observation_date, 
                                            format = "%m/%d/%Y")
population_data$Year <- year(population_data$observation_date)
population_data <- population_data %>% 
  rename(Population = POPTOTFJA647NWDB)

# Population model
population_model <- lm(Population ~ Year, data = population_data)

# GDP Data
gdp_data <- read_excel("imf-dm-export-20241213.xls")
gdp_clean <- gdp_data %>% 
  filter(`GDP, current prices (Billions of U.S. dollars)`== "Fiji") %>% 
  pivot_longer(
    cols = -`GDP, current prices (Billions of U.S. dollars)`,
    names_to = "Year",
    values_to = "GDP"
  ) %>%
  select(Year, GDP) %>%
  mutate(Year = as.numeric(Year), GDP = as.numeric(GDP)) %>% 
  filter(Year < 2024) # years after 2024 are gdp projections

# gdp model
gdp_model <- lm(GDP ~ Year, data = gdp_clean)

# Tourism / Visitors
visitor_data <- read.csv("summary-of-visitor-arriv.csv")
visitor_data <- visitor_data %>% 
  rename(Year = Category) 
# From another research showing the visitors in 2023
visitor_data <- add_row(visitor_data, Year = 2023, Arrivals = 929740)
visitor_data <- subset(visitor_data, select = -Departures)

# visitors model
visitors_model <- lm(Arrivals ~ Year, data = visitor_data)

# Comparison Data (information collected through research)

comparison_data <- data.frame(
  Island = c("Fiji", "Vanuatu", "Samoa", "Tonga"),
  Population = c(936375, 327777, 218019, 104175),
  GDP = c(5.442, 1.13, 0.93, 0.52),  # in billion USD
  Visitors = c(929740, 30306, 241276, 119916) 
)


# UI
ui <- fluidPage(
  titlePanel("Fiji: An Overview"),
  
  sidebarLayout(
    sidebarPanel(
      # Map sidebar
      conditionalPanel(
        condition = "input.main_tab == 'Maps of Fiji'",
        selectInput("map_level", "Select Map Level:", 
                    choices = c("Country Level", "World Level"), selected = "Country Level")
      ),
      
      # Key Facts Sidebar
      conditionalPanel(
        condition = "input.main_tab == 'Key Facts'", 
        h3("Key Facts Selection"), 
        selectInput("key_facts", "Select section:", 
                    choices = c("Government", "Economy", "Population", "Tourism"), 
                    selected = "Government")
      ),
      
      # Projections Sidebar
      conditionalPanel(
        condition = "input.main_tab == 'Projections'", 
        h3("Projection Controls"), 
        selectInput("projection_var", "Select a variable to project:", 
                    choices = c("Population", "GDP", "Visitors"), selected = "Population"), 
        numericInput("projection_years", "Number of years to project:", value = 5, min = 1, max = 10)
      ),
      
      # Comparison Sidebar
      conditionalPanel(
        condition = "input.main_tab == 'Comparison'", 
        h3("Comparision Controls"), 
        selectInput("comparison_var", "Select a variable for comparison:", 
                    choices = c("Population", "GDP", "Visitors"), selected = "Population")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tab",
        tabPanel("Maps of Fiji", 
                 # Show only the Country Level map when "Country Level" is selected
                 conditionalPanel(
                   condition = "input.map_level == 'Country Level'", 
                   h4("Fiji (Country Level)"),
                   leafletOutput("map_fiji", height = "400px"), 
                   fluidRow(
                     column(10, 
                            tags$p("Fiji is a tropical island country located in 
                                   Oceania, the South Pacific Ocean. It is made 
                                   up of over 332 islands, with 110 permanently 
                                   inhabited. The three islands that are close 
                                   to Fiji are Vanuatu, Samoa, and Tonga.",
                                   style = "font-size:22px; color:black")
                     )
                   )
                 ),
                 
                 # Show only the World Level map when "World Level" is selected
                 conditionalPanel(
                   condition = "input.map_level == 'World Level'", 
                   h4("Fiji (World Level)"),
                   leafletOutput("map_world", height = "400px"), 
                   fluidRow(
                     column(10,
                            tags$p("Another Fun Fact: It is located about 
                                   two-thirds of the way between Hawaii and New Zealand!",
                                   style = "font-size:22px; color:black")
                     )
                   )
                 )
        ),
        
        # Key Facts
        tabPanel("Key Facts",
                 # Government
                 conditionalPanel(
                   condition = "input.key_facts == 'Government'", 
                   h2("Key Facts about Government"), 
                   
                   # Pictures of the Fiji Government
                   fluidRow(
                     column(6, 
                            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/c7/Coat_of_arms_of_Fiji.svg", 
                                     height = "300px", width = "auto")
                            ), 
                     
                     column(6, 
                            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/b/ba/Flag_of_Fiji.svg", 
                                     height = "300px", width = "400px"))
                   ),
                   
                   fluidRow(
                     column(10, 
                            tags$p("Picture (left) Reference: https://en.wikipedia.org/wiki/Coat_of_arms_of_Fiji"),
                            tags$p("Picture (right) Reference: https://commons.wikimedia.org/wiki/File:Flag_of_Fiji.svg"),
                            
                            tags$p("The government of Fiji is a republic with a 
                                   president as the Chief of State and a prime 
                                   minister as the Head of the Government. The 
                                   current president of Fiji is Ratu Wiliame 
                                   Katonivere, and the prime minister is Sitiveni 
                                   Ligamamada Rabuka. Its political system is 
                                   based on a parliamentary democracy, where 
                                   members of Pariliament are elected in order 
                                   to make decisions on behalf of the people. 
                                   President is elected by the parliament 
                                   indirectly and the prime minister is elected 
                                   by the parliament. The election cycle is 
                                   3 years. The highest level of judicial 
                                   authority in Fiji is the federal supreme 
                                   court where judges work independently and 
                                   follow the law in order to make decisions.",
                                   style = "font-size:22px; color:black")
                     )
                   )
                 ),
                 
                 # Economy
                 conditionalPanel(
                   condition = "input.key_facts == 'Economy'", 
                   h2("Key Facts about Economy"), 
                   fluidRow(
                     column(10, 
                            tags$p("Fiji is described as one of the most 
                                   developed countries in the Pacific region. 
                                   The top industries of Fiji are tourism, sugar, 
                                   clothing, and copra (where coconut oil is 
                                   extracted). Fiji's largest trading partners 
                                   are Australia, United States, and New Zealand, 
                                   and the exports include beverages, oil and 
                                   mineral fuels, and seafood. The largest 
                                   composition of GDP (Gross Domestic Product) 
                                   is in services, followed by industry, 
                                   manufacturing and agriculture.",
                                   style = "font-size:22px; color:black"), 
                            
                            tags$p("The following graph illustrates Fiji's GDP 
                                   over the years. This shows how the economic 
                                   output has changed over time.", 
                                   style = "font-size:22px; color:black")
                     )
                   ), 
                   
                   plotOutput("gdp_plot"), 
                   fluidRow(
                     column(10, 
                            tags$p("The graph above shows an increasing trend in 
                                   Fiji's GDP over time. Before the year of 2000, 
                                   the GDP increases gradually but the GDP starts 
                                   to rise more sharp after 2000, which indicates 
                                   a steady economic growth. Additionally, this 
                                   graph also shows some fluctuations, which may 
                                   suggests some economic events or challenges 
                                   that impacted the economic growth. For 
                                   example, there is a sharp decrease in GDP 
                                   around the years of 2019 to 2020, which might 
                                   be impacted by the issue of Covid-19.",
                                   style = "font-size:22px; color:black")
                     )
                   )
                 ), 
                 
                 # Population
                 conditionalPanel(
                   condition = "input.key_facts == 'Population'", 
                   h2("Key Facts about Population"), 
                   fluidRow(
                     column(10, 
                            tags$p("The population in 2024 is approximately 
                                   928,784, with the median age of 27.9, which is 
                                   quite young. Of the population, about 60% of 
                                   the people live in the urban area of Fiji. 
                                   The following graph shows the total population 
                                   of Fiji over years.",
                                   style = "font-size:22px; color:black")
                     )
                   ), 
                   
                   plotOutput("population_plot"), 
                   
                   fluidRow(
                     column(10, 
                            tags$p("Overall, the above graph shows an steady 
                                   increasing trend of the total population in 
                                   Fiji. This tend may suggests that the living 
                                   standards and healthcare etc. are improved 
                                   over the years. It is easily to see that the 
                                   population in 2020 is over 800,000, where the 
                                   population in 1960 is only around 400,000. 
                                   The population is approximately doubled over 
                                   the years, hence the development of Fiji is 
                                   getting better.",
                                   style = "font-size:22px; color:black")
                     )
                   )
                 ), 
                 
                 # Tourism
                 conditionalPanel(
                   condition = "input.key_facts == 'Tourism'", 
                   h2("Key Facts about Population"), 
                   fluidRow(
                     column(6, 
                            tags$img(src = "https://media.cntraveller.com/photos/642aa1ad770beda2d4f5cc22/16:9/w_2240,c_limit/Fiji-march2023issue-JackJohns15.jpg", 
                                     height = "300px", width = "auto")), 
                     column(6, 
                            tags$img(src = "https://media.cntraveller.com/photos/642aa1ae770beda2d4f5cc24/master/w_1600,c_limit/Fiji-march2023issue-JackJohns17.jpg", 
                                     height = "300px", width = "auto"))
                   ), 
                   fluidRow(
                     
                     column(10, 
                            tags$p("Pictures' Reference: https://www.cntraveller.com/article/finding-fiji-the-worlds-friendliest-island-nation"), 
                            
                            tags$p("Fiji is a popular tourist destination known 
                                   for its beautiful beaches, tropical climate, 
                                   and clean waters. People are enjoying doing 
                                   activities like exploring the natural 
                                   enovironment and diving etc. at Fiji. 
                                   The following graph shows the total number of 
                                   visitors to Fiji over years.",
                                   style = "font-size:22px; color:black")
                     )
                   ), 
                   
                   plotOutput("visitors_plot"), 
                   fluidRow(
                     column(10, 
                            tags$p("There is an increasing pattern in the number 
                                   of visitors to Fiji start from 2000 to 2019. 
                                   However, there is a sharp decrease between the 
                                   years of 2019 to 2021. This is most likely due 
                                   to the reason of Covid-19. After 2021, the 
                                   number of visitors increases very quickly 
                                   hence suggesting that the tourism is recovering.",
                                   style = "font-size:22px; color:black")
                     )
                   )
                 )
        ),
         
        # Projection
        tabPanel("Projections",
                 h3("Projection of Key Variable"),
                 plotOutput("projection_plot"), 
                 verbatimTextOutput("projection_results")
        ),
        
        # Comparison
        tabPanel("Comparison",
                 h3("Comparison with Other Pacific Islands"),
                 plotOutput("comparison_plot"), 
                 fluidRow(
                   column(10, 
                          tags$p("The islands that Fiji are comparing to are 
                                 Vanautu, Samoa, and Tonga. As mentioned, they 
                                 are the three neighbors of Fiji.",
                                 style = "font-size:18px; color:black"), 
                          
                          tags$p("Population: By comparing the population of Fiji 
                                 with its neighbour islands Vanautu, Samoa, and 
                                 Tonga, Fiji has the largest population. Vanuatu 
                                 ranks the second and followed closely by Samoa. 
                                 They have much smaller population compared to Fiji. 
                                 Lastly, Tonga has the smallest population.",
                                 style = "font-size:18px; color:black"),
                          
                          tags$p("GDP: By comparing the 2023 GDP of Fiji with its 
                                 neighbours, Fiji dominates the Pacific region 
                                 economically. This may indicates that Fiji has 
                                 a more developed economy compared to its neighbours. 
                                 Then, followed by Vanuatu and Samoa, with Vanuatu 
                                 has slightly higher GDP. Lastly, Tonga has the 
                                 lowest GDP among the islands shown.",
                                 style = "font-size:18px; color:black"), 
                          
                          tags$p("Tourism: By comparing the 2023 visitors of Fiji 
                                 with its neighbour islands, again Fiji has a 
                                 significant lead in the number of visitors. 
                                 Samoa ranked the second, and is far behind Fiji. 
                                 Then, it is followed by Tonga ranked third place 
                                 and the last place Vanuatu. This may suggests that 
                                 the tourism sector for Vanuatu is less popular 
                                 compared to other islands.",
                                 style = "font-size:18px; color:black")
                   )
                 )
        ),
        
        # References
        tabPanel("References", 
                 h3("Data Sources"), 
                 p("https://fred.stlouisfed.org/series/POPTOTFJA647NWDB"), 
                 p("https://www.imf.org/external/datamapper/NGDPD@WEO/FJI?zoom=FJI&highlight=FJI"), 
                 p("https://www.statsfiji.gov.fj/statistics/social-statistics/tourism-and-migration-statistics/"), 
                 h3("Research References"), 
                 p("https://www.investinfiji.today/news-publication/fiji-breaks-visitor-arrival-records/#:~:text=Fiji%20welcomed%20a%20record%20929%2C740,Canada%20have%20exceeded%20past%20years."), 
                 p("https://www.worldometers.info"), 
                 p("https://tradingeconomics.com/"), 
                 p("https://globaledge.msu.edu/countries/fiji"), 
                 p("https://www.sbs.gov.ws/documents/social/International_Arrival_Statistics/2023/Migration_Annual_23.pdf"), 
                 p("https://www.statista.com/statistics/729470/gross-domestic-product-gdp-in-tonga/"), 
                 p("https://tongastats.gov.to/statistics/social-statistics/migration/")
                 
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Map of Fiji
  output$map_fiji <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      setView(lng = 178.0650, lat = -17.7134, zoom = 7) %>%
      addMarkers(lng = 178.0650, lat = -17.7134, popup = "Fiji - Main Island")
  })
  
  # Map of Fiji in the world level
  output$map_world <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 180, lat = 0, zoom = 2) %>%
      addMarkers(lng = 178.0650, lat = -17.7134, popup = "Fiji - South Pacific")
  })
  
  
  # Population Plot
  output$population_plot <- renderPlot({
    ggplot(population_data, aes(x = Year, y = Population)) + 
      geom_line(color = "purple") + 
      geom_point(color = "violet") + 
      labs(title = "Population of Fiji Over Years",
           x = "Year", y = "Population") + 
      theme_minimal() 
  })
  
  # GDP Plot
  output$gdp_plot <- renderPlot({
    ggplot(gdp_clean, aes(x = Year, y = GDP)) + 
      geom_line(color = "darkblue") + 
      geom_point(color = "blue") + 
      labs(title = "GDP of Fiji Over Years",
           x = "Year", y = "GDP (Billions of U.S. dollars)") + 
      theme_minimal() 
  })
  
  # Visitors plot
  output$visitors_plot <- renderPlot({
    ggplot(data = visitor_data, aes(x = Year, y = Arrivals)) + 
      geom_line(color = "darkgreen") + 
      geom_point() + 
      labs(title = "Visitors to Fiji", y = "Number of Visitors", x = "Year") + 
      theme_minimal()
  })
  
  # Projection of selected variable
  projection_data <- reactive({
    selected_variable <- input$projection_var
    
    # Based on selected variable, create projections
    if (selected_variable == "Population") {
      future_years <- (max(population_data$Year) + 1):(max(population_data$Year) + input$projection_years)
      future_population <- predict(population_model, newdata = data.frame(Year = future_years))
      data.frame(Year = future_years, Projected = future_population)
    } else if (selected_variable == "GDP") {
      future_years <- (max(gdp_clean$Year) + 1):(max(gdp_clean$Year) + input$projection_years)
      future_gdp <- predict(gdp_model, newdata = data.frame(Year = future_years))
      data.frame(Year = future_years, Projected = future_gdp)
    } else if (selected_variable == "Visitors") {
      future_years <- (max(visitor_data$Year) + 1):(max(visitor_data$Year) + input$projection_years)
      future_visitors <- predict(visitors_model, newdata = data.frame(Year = future_years))
      data.frame(Year = future_years, Projected = future_visitors)
    }
  })
  
  
  # Projection Plot
  output$projection_plot <- renderPlot({
    # Get projection data
    proj_data <- projection_data()
    
    # Plot using ggplot2
    ggplot(proj_data, aes(x = Year, y = Projected)) +
      geom_line() +
      geom_point() +
      labs(title = paste(input$projection_var, "Projection"),
           x = "Year", y = "Projected Value") +
      theme_minimal()
  })
  
  # projection results
  output$projection_results <- renderText({
    data <- projection_data()
    if (!is.null(data)) {
      # explanation of projection
      text <- paste("For ", input$projection_var, 
                    ", the projection of the next ", input$projection_years, 
                    " years is the following:\n\n", 
                    paste0("Year: ", data$Year, " | Projected: ", round(data$Projected, 2), collapse = "\n"), 
                    "\n\nThese projections are based on historical data and the fitted models for specific variable.", sep = "")
      return(text)
    }
  })
  
  # Comparison with other island states
  output$comparison_plot <- renderPlot({
    ggplot(comparison_data, aes(x = reorder(Island, -.data[[input$comparison_var]]), 
                            y = .data[[input$comparison_var]], fill = Island)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = paste("Comparison of", input$comparison_var, "among Island States"),
           x = "Island State", y = input$comparison_var) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
  })
}

# Run the App
shinyApp(ui = ui, server = server)


