library(shiny)
library(tidyverse)
library(sf)
library(gt)

shinyServer(function(input, output) {

  # Load the election data from our spreadsheet
  data <-
    read_csv("Cheatham_Precinct_Data.csv", col_names = TRUE, col_types = "dcccdcccccd") %>%
    filter(OFFICENAME %in% c("United States President", "Tennessee Governor"))

  # Take the votes by party and compute the % of vote for each
  votes <-
    data %>%
    select(YEAR, PRECINCT, PARTY, VOTES) %>%
    mutate(VOTES = as.integer(VOTES)) %>%
    group_by(YEAR, PRECINCT, PARTY) %>%
    summarize(VOTES = sum(VOTES)) %>%
    ungroup() %>%
    pivot_wider(id_cols = c("YEAR", "PRECINCT"),
                names_from = "PARTY",
                values_from = "VOTES") %>%
    mutate(Total = rowSums(select(., !matches("YEAR|PRECINCT")), na.rm = TRUE))

  # We want to order them from largest vote to smallest vote  
  order <-
    votes %>%
    pivot_longer(-c("YEAR", "PRECINCT"), names_to = "PARTY", values_to = "VOTES") %>% 
    filter(!is.na(VOTES)) %>%
    filter(PARTY != "Total") %>%
    group_by(YEAR, PARTY) %>% 
    summarize(VOTES = sum(VOTES)) %>% 
    arrange(YEAR, desc(VOTES))

  output$plot <- renderPlot({

    # Get the name of the office this election
    office <- data %>% filter(YEAR == input$year) %>%select(OFFICENAME) %>% unique() %>% pull()

    # Order votes largest to smallest
    order <- order %>% filter(YEAR == input$year) %>% pull("PARTY")
    votes <- votes %>% filter(YEAR == input$year) %>% select(PRECINCT, matches(order), Total)

    # Compute the margin of victory
    margin <-
      votes %>%
      mutate_at(vars(-"PRECINCT", -"Total"), ~ . / Total) %>%
      mutate(Other = Total - Democrat - Republican) %>%
      select(PRECINCT, Democrat, Republican, Other, Total) %>%
      mutate(MARGIN = Republican - Democrat) %>%
      select(PRECINCT, MARGIN)

    # Determine which mapfile to use based on the year
    if (input$year <= 2000) {
      mapfile <- "maps/2000/Cheatham_2000.shp"
    } else if (input$year > 2000 & input$year <= 2012) {
      mapfile <- "maps/2010/Cheatham_2010.shp"
    } else if (input$year >= 2014) {
      mapfile <- "maps/2014/Cheatham_2014.shp"
    }

    # Load map and bind it to our data
    map <- read_sf(mapfile) %>% full_join(margin, by = "PRECINCT")

    ggplot(data = map) + 
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "right",
      ) +
      geom_sf(aes(fill = MARGIN), size = 0.1, color = "#000000") +
      scale_fill_gradient2(
        midpoint = 0,
        limits = c(-1, 1),
        labels = scales::percent_format(accuracy = 1),
        guide  = guide_colorbar(barheight = unit(2, "in")),
        low    = "#0015bc",
        mid    = "#ffffff",
        high   = "#e9141d") #+
#     labs(title = paste(input$year, office, "Election"))
  })

  output$table_title <- renderText({

    # Get the name of the office this election

    office <- data %>% filter(YEAR == input$year) %>% select(OFFICENAME) %>% unique() %>% pull()
    office_txt <- paste(input$year, office, "Election")
    print(office_txt)

  })

  output$table <- renderTable({

    # Order votes largest to smallest
    order <- order %>% filter(YEAR == input$year) %>% pull("PARTY")
    votes <- votes %>% filter(YEAR == input$year) %>% select(PRECINCT, matches(order), Total) 

    party_columns <- 
      c("Republican", "Democrat", "Constitution", "Green", "Justice", "Libertarian", "American Third Position", 
        "Independent", "Write-In", "Socialist", "Reform", "Natural Law", "Socialist Workers", "Americas", 
        "Boston Tea", "Total")

    votes %>% 
      gt() %>%
#        tab_header(title = office_txt) %>%
      cols_align(
        columns = matches(party_columns),
        align = "left"
      ) %>%
      fmt_number(
        columns = matches(party_columns),
        scale_by = 1,
        decimals = 0
      ) 
  })

  output$candidates <- renderTable({

    candidates <-
      data %>% 
      filter(YEAR == input$year) %>%
      select(PARTY, CANDIDATE, VOTES) %>%
      group_by(PARTY, CANDIDATE) %>% 
      summarize(VOTES = sum(VOTES)) %>% 
      ungroup() %>% 
      arrange(desc(VOTES))

    candidates %>% 
      gt() %>%
      fmt_number(
        columns = matches("VOTES"),
        scale_by = 1,
        decimals = 0
      ) 

  })

})
