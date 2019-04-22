#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr) #functions
library(ggplot2) #plots
library(countrycode) # continent
library(rworldmap) # quick country-level heat maps
library(gridExtra) # plots

#read csv file
suiciderates <- read.csv("master.csv")

countries <- suiciderates %>%
  distinct(country)

#new dataset without missing data
#data 
#head(suiciderates)
data <- filter(suiciderates,year != 2016)
names(data)[5]<-"suicide-no"
names(data)[7]<-"suicides100kpop"
names(data)[8]<-"country-year"
names(data)[9]<-"HDI-for-year"
names(data)[10]<-"gdp-for-year"
names(data)[11]<-"gdp-per-capita"
data <- data %>% select(-c('HDI-for-year'))

#Adding another attribute continent
data$continent <- countrycode(sourcevar = data[, "country"],origin = "country.name",destination = "continent")
global_average <- (sum(as.numeric(data$`suicide-no`)) / sum(as.numeric(data$population))) * 100000


globalsuicide <- data %>%
  group_by(country, year)  %>%
  summarize(suicides100kpop  = (sum(`suicide-no`) /  sum(population)) * 100000)

globalsuicidebysex <- data %>%
  group_by(country, year,sex)  %>%
  summarize(suicides100kpop  = (sum(`suicide-no`) /  sum(population)) * 100000)

globalsuicidebyage <- data %>%
  group_by(country, year,age)  %>%
  summarize(suicides100kpop  = (sum(`suicide-no`) /  sum(population)) * 100000)

#anaysis b continents
cont <-group_by(data,continent) %>% 
  summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000) %>%
  arrange(suicides100kpop)

conttrend <-data %>%
  group_by(year, continent) %>%
  summarize(suicides100kpop = (sum(as.numeric(`suicide-no`)) / sum(as.numeric(population))) * 100000)

server <- function(input, output) 
  {
  
  output$Country <- renderMenu({
    print(input$tabs)
    if(input$tabs == 'suicideSummary') 
      {sidebarMenu(
        menuItem(
        selectInput("Country",
                    "Select the Country",
                    choices = countries,
                    selected = 'Albania'))
        )}
        else
        {
            sidebarMenu()}
  })
  
  output$plot1 <- renderPlot({
    globalsuicide %>%
      filter(country == input$Country) %>%
      ggplot(aes(x = year, y = suicides100kpop)) +
      geom_line(col = "deepskyblue3", size = 1) +
      geom_point(col = "deepskyblue3", size = 2) +
      labs(
        title = "Global Suicides (per 100k)",
        subtitle = "Trend over time, 1985 - 2015.",
        x = "Year",
        y = "Suicides per 100k"
      )
  })
  
  output$plot2 <-renderPlot({
    
    globalsuicidebysex %>%
      filter(country == input$Country) %>% 
      ggplot(aes(x = sex, y = suicides100kpop, fill = sex)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides (per 100k), by Sex",
           x = "Sex", 
           y = "Suicides per 100k") 
  })
  
  
  output$plot3 <-renderPlot({
    
      globalsuicidebysex %>%
      filter(country == input$Country) %>% 
      ggplot(aes(x = year, y = suicides100kpop, col = factor(sex))) + 
      facet_grid(sex ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Sex", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Sex")
    
  })
  
  output$plot4 <-renderPlot({
    
    globalsuicidebyage %>%
      filter(country == input$Country) %>% 
    ggplot(aes(x = age, y = suicides100kpop, fill = age)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global suicides per 100k, by Age",
           x = "Age", 
           y = "Suicides per 100k") 
    
  })
  
  output$plot5 <-renderPlot({
    
    globalsuicidebyage %>%
      filter(country == input$Country) %>% 
    ggplot(aes(x = year, y = suicides100kpop, col = age)) + 
      facet_grid(age ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Age", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Age") + 
      theme(legend.position = "none")
    
  })
  
  output$plot6<-renderPlot({
    ggplot(cont, aes(x = continent, y = suicides100kpop, fill = continent)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Global Suicides (per 100k), by Continent",
           x = "Continent", 
           y = "Suicides per 100k", 
           fill = "Continent") 
    
  })
  
  output$plot7 <-renderPlot({

      ggplot(conttrend,aes(x = year, y = suicides100kpop, col = factor(continent))) + 
      facet_grid(continent ~ ., scales = "free_y") + 
      geom_line() + 
      geom_point() + 
      labs(title = "Trends Over Time, by Continent", 
           x = "Year", 
           y = "Suicides per 100k", 
           color = "Continent")
    
  })
  
  output$plot8 <-renderPlot({
    
    data %>%
      group_by(continent, sex) %>%
      summarize(n = n(), 
                suicides = sum(as.numeric(`suicide-no`)), 
                population = sum(as.numeric(population)), 
                suicides100kpop = (suicides / population) * 100000) %>%
      ggplot(aes(x = continent, y = suicides100kpop, fill = sex)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
      labs(title = "Gender Disparity, by Continent",
           x = "Continent", 
           y = "Suicides per 100k", 
           fill = "Sex") +
      coord_flip()
  })
  
    output$plot9 <-renderPlot({
      
      data %>%
        group_by(continent, age) %>%
        summarize(n = n(), 
                  suicides = sum(as.numeric(`suicide-no`)), 
                  population = sum(as.numeric(population)), 
                  suicides100kpop = (suicides / population) * 100000) %>%
        ggplot(aes(x = continent, y = suicides100kpop, fill = age)) + 
        geom_bar(stat = "identity", position = "dodge") + 
        geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
        labs(title = "Age Disparity, by Continent",
             x = "Continent", 
             y = "Suicides per 100k", 
             fill = "Age")
      
    })
    
    
}

