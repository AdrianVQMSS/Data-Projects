# load packages
library(rio)
library(rvest)
library(tidyverse)
library(magrittr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(plotly)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(colorspace)
library(tm)
library(stringr)
library(patchwork)
library(scales)
library(DT)

source("setup.R")

shinyUI(
  dashboardPage(
    title = "Changes in the Video Game Landscape",
    header = dashboardHeader(titleWidth = '100%',
                             title = span(
                               tags$img(
                                 src = "pac_trans.png",
                                 width = '100%',
                                 height = '100%'
                               )
                             )),
    
    dashboardSidebar(
      # side bar picture
      # div(tags$img(
      #   src = "pac.png",
      #   style = 'position: fixed',width ="220px",height ="85%"
      # ),
      # style="text-align: left:1;"),
      
      width = 220,
      sidebarMenu(
        style = "position:fixed;width:220px;",
        
        
        #  "Filter Controls",
        # genre1
        conditionalPanel(
          condition = "input.tabselected == 1",
          menuItem(
            tabName = "Genres 1",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "genre1",
              label = "First Genre Filter",
              multiple = F,
              selected = " rpg ",
              choice = genrel,
              options = list(
                create = F,
                placeholder = "Try 'rpg'...",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
            
          )
        ),
        
        
        
        # genre2
        conditionalPanel(
          condition = "input.tabselected == 1",
          menuItem(
            tabName = "Genres 2",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "genre2",
              label = "Second Genre Filter",
              multiple = F,
              choice = genrel,
              selected = "all",
              options = list(
                create = F,
                placeholder = "Try 'moba'...",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
          )
        ),
        
        
        # platform
        conditionalPanel(
          condition = "input.tabselected == 1",
          menuItem(
            tabName = "Platforms",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "platf",
              label = "Platform Filter",
              multiple = T,
              choice = all_plat,
              selected = "PC",
              options = list(
                create = F,
                placeholder = "Try 'PC'...",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
          )
        ),
        
        
        #mental health
        conditionalPanel(
          condition = "input.tabselected == 3",
          menuItem(
            tabName = "Mental Health",
            style = "position:fixed;width: inherit;",
            
            selectizeInput(
              inputId = "mh",
              label = "Search for a Population Category",
              multiple = T,
              choice = mh_cat,
              selected = "age group",
              options = list(
                create = F,
                placeholder = "Search for a Population Category",
                maxItems = "1",
                onDropdownOpen = I(
                  "function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"
                ),
                onType = I("function (str) {if (str === \"\") {this.close();}}")
              )
            )
          )
        ),
        width = 3
      ) # sidebar menu
    ),
    # dashboard sidebar
    
    #################################################################
    dashboardBody(
      tags$style(
        type = "text/css",
        "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }

    /*    Make the image taller */
    .main-header .logo {
        height: 125px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 175px;
    }"
      ),
      
      #################################################################
      shinyDashboardThemes(theme = "grey_dark"),
      # overview tab
      tabsetPanel(
        type = "tabs",
        id = "tabselected",
        selected = 0,
        
        #################################################################
        # Welcome page tab
        tabPanel(
          "Project Overview",
          br(),
          
          fluidRow(column(
            width = 12,
            align = "center",
            h1(strong("Welcome~!"))
          )),
          br(),
          
          fluidRow(column(
            width = 12,
            align = "center",
            h2(strong("Changes in the Video Game Landscape"))
          )),
          #tags$hr(),
          br(),
          #br(),
          
          fluidRow(column(
            width = 4,
            offset = 4,
            tags$img(
              src = "Tennis_For_Two.png",
              width = "100%",
              height = "80%"
            ),
          )),
          fluidRow(column(
            width = 12,
            align="center",
            h5("'Tennis for Two' on a DuMont Lab Oscilloscope")
          )),
          br(),
          
          
          fluidRow(column(
            width = 12,
            align="center",
            h3(strong("Introduction"))
          )),
          
          fluidRow(column(
            width = 8,
            offset=2,
            p(
              "Among the first electronic games ever created, which are now referred to as video games, was 'Tennis for Two', introduced on
              October 18th, 1958. In a short 64 years this one game sparked the creation of a multi-billion dollar industry.",
              style = "font-size:17px;"
            )
          )),
          br(),
          
          
          fluidRow(column(
            width = 8,
            offset=2,
            p(
              "With this new gaming industry came a plethora of innovations including new genres, advancements in software and hardware capabilities,
            and numerous sub-culture's to name a few. For our project, we will examine the video game landscape and how it has changed over time,
            with a focus on the past 4 years or so."), 
            br(),
            
            p("We will take a deep-dive into the industry by looking at the evolution of perceptions 
            of games in the first 'Games' tab, followed by popular platforms and country-specific industry data in the 'Games and Sales' tabs. 
            Next, we explore the impact of Covid-19 on the video game landscape in the 'Covid-19: Games and Gaming' tab, and finally, in the 'Covid-19: Games and 
            Twitter' tab, we provide text analysis visualizations regarding the Top 10 games of each year (2019-2021) to investigate the impact of the COVID-19 pandemic 
            on public sentiment in the gaming sphere. In the appendix tab we have listed our sources.",
              style = "font-size:17px;"
            ),
            br(),
            
            p(strong("Please enjoy your adventure~")),
          )),
          
          
          #################################################################
          tags$hr(),
          #br(),
          h3(strong("Group Members")),
          br(),
          
          # Tobias
          tags$img(
            src = "pac_p.png",
            style = 'position: absolute',
            width = "4.5%",
            height = "4%"
          ),
          br(),
          
          
          fluidRow(column(
            width = 8,
            offset = 1,
            h3("Shengkang 'Tobias' Zhou")
          )),
          fluidRow(column(
            width = 8,
            offset = 1,
            p(
              "Tobias was born and raised in a 'traditional' Chinese family; gaming was the “forbidden fruit” of his childhood. However, his parents made a mistake when they bought him a Windows PC in high school, because then he actually began to play video games.
           His favorite genres are RPG and fantasy, the top three games being:",
              style = "font-size:17px;"
            ),
            tags$li(
              "Elder Scroll V: Skyrim  (bet on George Martin finish GOT before Bethesda release Elder Scroll VI)",
              style = "font-size:15px;"
            ),
            tags$li(
              "Total War: Warhammer Series  (can only beat the campaign on the beginner difficulty)",
              style = "font-size:15px;"
            ),
            tags$li("Magic the Gathering  (too poor to be a competitive player)",
                    style = "font-size:15px;")
          )),
          tags$hr(),
          br(),
          # Adrian
          tags$img(
            src = "pac_o.png",
            style = 'position: absolute',
            width = "4.5%",
            height = "4%"
          ),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 1,
            h3("Adrian Varallyay")
          )),
          fluidRow(column(
            width = 8,
            offset = 1,
            p(
              "Has had an on and off relationship with console gaming. However, like many during the Covid-19 pandemic, he reconnected with gaming via the Nintendo Switch.
          These days he probably spends more time than he should splatting opponents in ‘Splatoon 2’.",
              style = "font-size:17px;"
            ),
            h5("Other favorites include:"),
            
            tags$li("Overcooked 2", style = "font-size:15px;"),
            tags$li("The Metroid series", style = "font-size:15px;"),
            tags$li("The Zelda series", style = "font-size:15px;")
            
          )),
          tags$hr(),
          br(),
          
          # Tazz
          tags$img(
            src = "pac_r.png",
            style = 'position: absolute',
            width = "4.5%",
            height = "4%"
          ),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 1,
            h3("Tazz Orchi")
          )),
          
          fluidRow(column(
            width = 8,
            offset = 1,
            p(
              "Tazz grew up playing whatever free game was installed on her PC until her parents finally bought her a Gameboy Advance SP. She spent her younger years playing exclusively Gameboy games until finding a love for PC gaming.
            She has since built her first PC and enjoys multiplayer games with her friends the most.",
              style = "font-size:17px;"
            ),
            h5("Current Favorite Games:", style = "font-size:17px;"),
            tags$li("Monster Hunter World", style = "font-size:15px;"),
            tags$li("Stardew Valley", style = "font-size:15px;"),
            tags$li("Raft", style = "font-size:15px;")
          )),
          tags$hr(),
          br(),
          
          # Alan
          tags$img(
            src = "pac_b.png",
            style = 'position: absolute',
            width = "4.5%",
            height = "4%"
          ),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 1,
            h3("Xiuyuan 'Alan' Shen")
          )),
          
          fluidRow(column(
            width = 8,
            offset = 1,
            p(
              "Alan, originally from Hong Kong, now lives in Shanghai. He has fond memories of playing Metal Slug and Tony Hawk's Pro Skater with his dad on the Playstation 1.
            He enjoys strategy, RPG, FPS, and sports games. His current top 3 favorite games are:",
              style = "font-size:17px;"
            ),
            tags$li("The Last of Us", style = "font-size:15px;"),
            tags$li("Hades", style = "font-size:15px;"),
            tags$li("Hollow Knight", style = "font-size:15px;")
          )),
          
          tags$hr(),
          br(),
          
          br(),
          value = 0
        ),
        #################################################################
        # tab 1
        # Video Games and Genres Ratings tab
        tabPanel(
          "Games and Genres Popularity", 
          fluidRow(column(
            width = 4,
            offset = 4,
            align = "center",
            h2(strong("GAMES! GAMES! GAMES!"))
          )),
          br(),
          br(),
          fluidRow(column(
            width = 8,
            offset = 2,
            #align = "center",
            p(
              "On this page, we compare the perceptions of games over the past 11 years (2011-2021) between players and professisonal
            game critics (metacritic scores). We conducted comparisons from two perspectives: ",
              style = "font-size:17px;"
            ),
          )),
          fluidRow(column(
            width = 8,
            offset = 2,
            tags$ol(
              tags$li(
                "comparing individual game ratings, filtered by genre and platform.",style = "font-size:15px;"
              ),
              tags$li("comparing the most popular genres over time.",style = "font-size:15px;")
            )
          )),
          br(),
          br(),
          h3("User Instructions For Sidebar:"),
          p("The first two boxplots below render on the filtered dataset based on the inputs."),
          p("TWO genres can be displayed for the gaming platform selected."),
          p("If you only wish to see the results for one genre, put 'all' for the 2nd genre choice."),
          p(strong("NOTE: Some combinations result in a blank graph because no games in the dataset 
                   have that combination of characteristics.", style="font-size:15px;")),
          tags$hr(),
          #br(),
          
        
          
        
          
          fluidRow(column(
            width = 12,
            #offset = 0,
            align = "center",
            h3("Video Game Ratings by Players (5-point scale)")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            #align = "center",
            plotlyOutput("games_rating", height = 550)
          )),
          
          fluidRow(column(
            width = 12,
            # offset = 3,
            align = "center",
            h3("Video Game Ratings by Metacritic (100-point scale)")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            plotlyOutput("games_meta", height = 550)
          )),
          tags$hr(),
          br(),
          
          fluidRow(column(
            width = 12,
            # offset = 3,
            align = "center",
            h3("Top 5 Genres Rated by Players")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            plotlyOutput("t5_genre_rating", height = 550)
          )),
          br(),
          
          fluidRow(column(
            width = 12,
            # offset = 3,
            align = "center",
            h3("Top 5 Genres Rated by Metacritics")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            plotlyOutput("t5_genrer_meta", height = 550)
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            # align="center",
            p(
              "The score for genre is calculated by grouping games by their lable and taking the average of the groups.
           Every genre that has less than 30 games is dropped. The bubble size displays the number of games.",
              style = "font-size:17px;"
            ),
            p(
              "As the graphs indicate, overtime, RPG, Atmospheric and Cooperative games are popular among both players and professional game critics. 
              The Tactical genre is the only one that appears on the top of the list for metacritic reviews,
           but does not appear for the player scores. In 2015, the Tactical genre is the top genre rated by metacritic,
           including games like Metal Gear Solid V: The Phantom Pain and Wasteland 2: Director's Cut.",
              style = "font-size:17px;"
            ),
            p(
              "Comedy is a genre that only appears in the player rating table. It includes games such as
           Grand Thief Auto V, Half-life 2 and Portal 2. ",
              style = "font-size:17px;"
            ),
            p(
              "The Violent and Gore genres are becoming more and more popular since 2018.
           Games like Red Dead Redemption 2 and Seikiro: Shadows Die Twice receive overwhelmingly positive reviews by both
           players and the critics alike.",
              style = "font-size:17px;"
            )
          )),
          tags$hr(),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 1,
            p(
              "Note: The data in these graphs are searchable through the provided datatable below."
            )
          )),
          
          fluidRow(column(
            width = 12,
            #offset = 4,
            align = "center",
            h3("Search for a game")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            DT::dataTableOutput("gamesdt", height = 650)
          )),
          value = 1
        ),
        
        #################################################################
        # Game Counts by Platforms tab
        tabPanel(
          "Games and Sales",
          br(),
          
          fluidRow(column(
            width = 4,
            offset = 4,
            align = "center",
            h2(strong("Big News For Big Business!"))
          )),
          br(),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            h3("Platform Game Counts Tracking Over Time")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "The first ever home video game console was released in 1972 by Magnavox, just 14 years after the release of the first video game.
            Since then game companies have released numerous platforms on which consumers can play.
            There are traditional consoles, which are heavily dominated by Microsoft's XBox Series and Sony's PS consoles,
            as well as handheld devices like the ever popular Nintendo DS family of systems and the recent hybrid behemoth, the Nintendo Switch. The graph below shows the 
            most popular platforms for game releases over the last decade. Following a dip in 2019, the most popular release platforms were PC, Sony PS Consoles, 
            and Xbox series consoles. By hovering over an individual point you can see the number of games released on the associated platform.
            In the last 10 years, the most games ever released on one platform was in 2015 when 441 games were released for PC in one year.",
              style = "font-size:17px;"
            )
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            plotlyOutput("platform", height = "550px")
          )),
          tags$hr(),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            h3("Console Sales Over Time")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "In addition, console sales data indicates that the best selling gaming hardware since 2019 has been the Nintendo Switch console.
            In late 2020, both the Xbox Series S and PlayStation 5 were released to high demand. Due to the demand and current worldwide chip
            shortage, it's very difficult to purchase these popular consoles, though constraints have been easing. In addition, 'scalping', the
            practice of purchasing popular gaming parts and selling much higher than market price, has made it difficult for the average person to
            purchase the latest technology at an accessible price.",
              style = "font-size:17px;"
            )
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            plotlyOutput("salesplot", height = "550px")
          )),
          br(),
          tags$hr(),
          br(),
            
          fluidRow(column(
            width = 12,
            #offset = 2,
            align = "center",
            h3("Map: Top 11 Countries of 2020 and 2021 by Revenue")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "The interactive map below currently only displays the top 11 countries of 2019 and 2020, ranked by video game revenue.
            Eleven countries are displayed, as one country changed on the Top 10 list between the 2 years. More data points (countries) and different types of data (variables) will be added over time as
            we can gain access to more data. Some examples of current available data in pop-up include: country name, ranking, revenue, exports,
            and active palyer info.",
              style = "font-size:17px;"
            )
          )),
          br(),
          
          
          fluidRow(
            
            column(
            width = 8,
            offset = 2,
            align = "center",
            leafletOutput("mymap", height = "600px")
          )),
          tags$hr(),
          br(),
          
          # map DT
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            h3("Explore the data in the map!")
          )),
          br(),
          
          fluidRow(column(
            width = 10,
            offset = 1,
            align = "left",
            DT::dataTableOutput("country", height = 650, width = 220)
          ))
        ),
        
        
        
        
        
        # release date delays tab
        tabPanel(
          "Covid-19: Games and Gaming",
          br(),
          
          h4("User Instructions For Sidebar:"),
          h6(
            "The search field in the sidebar corresponds to the last visualization regarding mental health, see below."
          ),
          tags$hr(),
          #br(),
          
          fluidRow(column(
            width = 4,
            offset = 4,
            align = "center",
            h2(strong("The Mark of Covid-19"))
          )),
          #br(),
          
          fluidRow(column(
            width = 12,
            align = "center",
            h3("Game Company Stocks Over Time")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "Below we see the changes in price of stocks for the top 10 Game companies. In March of 2020, approximately the time that covid-19 began to spread,
            there was a slight dip in stock prices across all companies followed by a steady rise, particularly for Microsoft. It should be noted that all companies,
            with the exception of NTES, experienced rapid increase in stock value in the years following the pandemic.",
              style = "font-size:17px;"
            )
          )),
          fluidRow(column(
            width = 8,
            offset = 2,
            h4("The top 10 game companies are:")
          )),
          
          fluidRow(
            column(
              width = 8,
              offset = 2,
              tags$li("Microsoft (MSFT)"),
              tags$li("Sony (SONY)"),
              tags$li("Nintendo (NTDOY)"),
              tags$li("Tencent (TCEHY)"),
              tags$li("Activision Blizzard (ATVI-recently acquired by MSFT)"),
              tags$li("Electronic Arts (EA)"),
              tags$li("Bandai Namco (NCBDY)"),
              tags$li("NetEase (NTES)"),
              tags$li("Square Enix (SQNNY)"),
              tags$li("Take-Two Interactive (TTWO)")
            )
          ),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            plotlyOutput("stockplot", height = "550px")
          )),
          tags$hr(),
          br(),
          
          fluidRow(column(
            width = 12,
            align = "center",
            h3("Video Game Delays by Dates (2019-2022)")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "In the graph below, displayed is list of games that were delayed between 2019 and 2022."), 
              
              p("The first point in the dumbell plot is the INITIALLY announced release date, 
              and the second point is the ACTUAL release date of the game. The legend displays the color assigned to each game title. The shape next to each title corresponds to whether Covid-19 
              had ANY impact on deveolpment time (was among the factors). Square indicates 'YES', at some point, Covid-19 impacted game development. Circle indicates 'No', Covid-19 did 
              not impact game development. And, tringle indicates 'UNSURE', it is unclear whether Covid-19 was among the factors that impacted game development."), 
              p("All data points here were selected from a collection of titles due to their data completeness. Nonetheless, a simple pattern appears, 
              more games were delayed during 2020 through 2021 than in 2019, reflecting the impact of the Covid-19 pandemic on development, most likely due to the stay-at-home 
              quarentine mandates. Arguably, it could also be the case that several of the game releases in 2022 could also be cases of lingering effects, though that is speculation 
                at this point.",
              style = "font-size:17px;"
            )
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            plotlyOutput("dly", height = "650px")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "This graph displays the same information as above, but is reframed as days delayed instead of dates delayed, in case the reader would like to see the information 
              displayed from a different perspective.",
              style = "font-size:17px;"
            )
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            plotlyOutput("dly2",  height = "750px")
          )),
          tags$hr(),
          br(),
          
          
          fluidRow(column(
            width = 12,
            offset = 0,
            align = "center",
            h3("Impact of Gaming on Mental Health During Covid-19")
          )),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "As referenced above in the instructions for the sidebar functionality for this page, below displays information on 
              the impact of gaming on mental health during the Covid-19 pandemic.
            The selection of population groups to select from are,",
              style = "font-size:17px;"
            )
          )),
          
          fluidRow(
            column(
              width = 8,
              offset = 2,
              tags$li("Age group - Age groups"),
              tags$li("Change in Game Type - Did game type played during the pandemic change? (Y/N/unsure)"),
              tags$li("Frequency - Gaming frequency during the pandemic"),
              tags$li("Degree - Academic degree"),
              tags$li("Gender - Binary/Non-binary gender responses"),
              tags$li("Occupation - Occupation Status (F/T, P/T, etc.)"),
              tags$li("Well-Being - Well-being impact type (Y/N)")
            )
          ),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "Each selection will display the distribution of those who responded yes/no to the question, 
              'whether playing video games during the pandemic affected individual's mental health'.
            Of those who responded yes, the distribution of positive/negative impacts will be shown.",
              style = "font-size:17px;"
            ),
            p("The red density curve represents the proportion of respondents who stated gaming had a 'Negative Imapct'. 
            The gren curve represents the proportion of respondents who stated gaming had 'No Imapct'. The blue curve 
            represents the proportion of respondents who stated gaming had a 'Positive Imapct.'
              "),
            p("The visualizations are diplayed in 2x3 grid, and the columns share the x-axis labels (Density). The farther right the peak of the curve, the higher the density."),
            h6(
              "Note: Position of peaks of density curves on the y-axis, are insignificant, the y-axis position is an artifact of the code"
            )
          )),
          br(),
          
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            plotOutput("mho", height = "650px")
          )),
          br(),
          
          value = 3
        ),
        
        # twitter data tab
        tabPanel(
          "Covid-19: Games and Twitter",
          br(),
          
          fluidRow(column(
            width = 4,
            offset = 4,
            align = "center",
            h2(strong("Welcome to the Twitterverse..."))
          )),
          br(),
          br(),
          
          h6(
            "The wordclouds below are generated from tweets scraped from the Twitter API."
          ),
          tags$hr(),
          br(),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            p(
              "We chose to look at the top ten most popular/best selling games from 2019, 2020, and 2021. For each year, we collected tweets that mentioned the top ten most
            popular games.",
              style = "font-size:17px;"
            ),
            p(
              "After cleaning the data, we were left with around 3000 tweets per year. As we can see from the 2019 wordcloud, the word 'twitch' appears to be the most frequently
            used term, with 207 mentions. Similarly, and even across all years, words like 'stream', 'streamer', 'live', 'community', and 'mixer' (a former streaming platform), all have relatively high frequency.
            This signifies the rise in popularity of live streaming.",
              style = "font-size:17px;"
            ),
            p("For 2022, the 'streaming' trend continues, understandably of course, as people were stuck at home during the lockdown period of the pandemic."),
            p(
              "As for 2021, 'halo' appears to be the most frequently used term with 487 mentions, most likely due to the highly anticipated release of 'Halo: Infinite', the latest installment of
            the 'Halo' franchise. Nonetheless, words like 'twitch' and 'stream' are still very prominent. Generally speaking, it is safe to say from this text analysis that Covid-19 played
            a major role in pushing the idea of live streaming and platforms, like twitch, into mainstream popularity.",
              style = "font-size:17px;"
            ),
            
          )),
          br(),
          tags$hr(),
          
          h6(
            "Note: When the cursor hovers above a word, a pop-up value of word frequency will display."
          ),
          
          fluidRow(column(
            width = 12,
            align = "center",
            h3("2019 Wordcloud")
          )), 
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            wordcloud2Output('my_wc19', height = 500)
          )),
          tags$hr(),
          br(),
          fluidRow(column(
            width = 12,
            #offset = 4,
            align = "center",
            h3("2020 Wordcloud")
          )),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            wordcloud2Output('my_wc20', height = 500)
          )),
          tags$hr(),
          br(),
          
          fluidRow(column(
            width = 12,
            #offset = 4,
            align = "center",
            h3("2021 Wordcloud")
          )),
          
          fluidRow(column(
            width = 8,
            offset = 2,
            align = "center",
            wordcloud2Output('my_wc21', height = 500)
          ))
        ),
        
        # Appendix
        
        tabPanel(
          "Appendix",
          
          fluidRow(column(
            width = 4,
            offset = 4,
            align = "center",
            h2(strong("Continue?")),
            #tags$li("Yes"),
            #tags$li("No")
          )),
          #br(),
          
          # citations
          tags$div(
            h3(strong("Sources")),
            br(),
            
            fluidRow(column(
              width = 4,
              offset = 4,
              tags$img(
                src = "game_over.png",
                #width = '100%',
                #height = '100%',
                style = 'position: absolute; '
              ))),
            
            
            h5('Alpha Vantage Stock API'),
            
            h5("Game Gavel"),
            tags$li(
              a(Href = "https://gamegavel.com/video-game-statistics/", "Game Statistics")
            ),
            
            
            h5("IGN"),
            tags$li(
              a(Href = "https://www.ign.com/articles/video-game-delays-2021", "Game Delays in 2021")
            ),
            tags$li(
              a(Href = "https://www.ign.com/articles/video-game-delays-2021-are-worse-covid", "Covid Delays are worse")
            ),
            tags$li(
              a(Href = "https://www.ign.com/articles/2019/12/12/the-last-of-us-part-2-animal-crossing-new-horizons-and-every-game-delay-announced-in-2019", "Game Delay 2019")
            ),
            
            
            h5("RAWG API"),
            tags$li(
              a(href = "https://rawg.io/apidocs#pricing", "Players and Genres")
            ),
            
            h5("PC Gamer"),
            tags$li(
              a(Href = "https://www.pcgamer.com/delayed-in-2019-the-games-that-didnt-come-out-this-year-and-why/",  "Delayed 2019")
            ),
            
            h5("Twitter API"),
            
            h5('VgChartz Console Sales Data'),
            
            h5("Wikipedia"),
            tags$li(
              a(Href = "https://en.wikipedia.org/wiki/Tennis_for_Two", "Tennis for Two")
            ),
            tags$li(
              a(Href = "https://en.wikipedia.org/wiki/2020_in_video_games", "2020 in Video Games")
            ),
            tags$li(
              a(Href = " https://en.wikipedia.org/wiki/Video_game_industry#2020s", "Video Game Industry 2020")
            ),
            tags$li(
              a(Href = "https://en.wikipedia.org/wiki/2021_in_video_games", "2021 in Video Games")
            ),
            tags$li(
              a(Href = "https://en.wikipedia.org/wiki/Impact_of_the_COVID-19_pandemic_on_the_video_game_industry#Hardware_and_software_releases", "Impact of Covid-19")
            )
          ),
          
          h5("YouTube"),
          tags$li(
            a(href = "https://www.youtube.com/watch?v=Wyg8E5fDLrw", "Video: 2019 Delays")
          ),
          
          br(),
          br(),
            p(strong("Thanks for Playing!")),
          
          tags$hr(),
          h3(strong("Acknowledgements")),
          #br(),
          fluidRow(column(
            width = 4,
            p(
              "We would like to give a special thanks to Professor Matthew Barr at the University of Glasgow, who so kindly shared his data with us. Professor Barr collected data for his study regarding the impact of video gaming on mental health during the Covid-19 pandemic."
            )
          )),
          br(),
          h5("Dr. Matthew Barr"),
          tags$li(
            a(Href = "https://www.researchgate.net/profile/Matthew-Barr-3", "ResearchGate Profile")
          ),
          tags$li(
            a(Href = "http://dx.doi.org/10.1177/15554120211017036", "Article DOI: 10.1177/15554120211017036")
          ),
          br(),
          br(),
        ),
        
        
      )
    ) #main panel
  )
)
