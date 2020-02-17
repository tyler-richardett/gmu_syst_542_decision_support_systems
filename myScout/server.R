shinyServer(function(input, output, session) {
        observeEvent(input$next1, {
                shinyjs::hide("getting_started")
                shinyjs::show("player_div1")
        })
        
        observeEvent(input$previous2, {
                shinyjs::hide("player_div1")
                shinyjs::show("getting_started")
        })
        
        observeEvent(input$next2, {
                if(input$maximum_budget > 0) {
                        shinyjs::hide("player_div1")
                        shinyjs::show("player_div2")
                }
                
                else {
                        showModal(
                                modalDialog(
                                        title = "Error: Invalid Budget",
                                        "Your maximum budget must be set at a value greater than €0."
                                )
                        )
                }
        })
        
        observeEvent(input$previous3, {
                shinyjs::hide("player_div2")
                shinyjs::show("player_div1")
        })
        
        observeEvent(input$next3, {
                shinyjs::hide("player_div2")
                shinyjs::show("player_div3")
        })
        
        observeEvent(input$previous4, {
                shinyjs::hide("player_div3")
                shinyjs::show("player_div2")
        })
        
        observeEvent(input$next4, {
                shinyjs::hide("player_div3")
                
                if("Physical stature" %in% input$attributes2) {
                        shinyjs::show("player_div4")
                }
                
                else if ("Dominant foot" %in% input$attributes2) {
                        shinyjs::show("player_div5")
                }
                
                else {
                        shinyjs::show("player_div6")
                }
        })
        
        observeEvent(input$previous5, {
                shinyjs::hide("player_div4")
                shinyjs::show("player_div3")
        })
        
        observeEvent(input$next5, {
                if(input$height != "No Preference" | input$weight != "No Preference") {
                        shinyjs::hide("player_div4")
                        
                        if ("Dominant foot" %in% input$attributes2) {
                                shinyjs::show("player_div5")
                        }
                        
                        else {
                                shinyjs::show("player_div6")
                        }
                }
                
                else {
                        showModal(
                                modalDialog(
                                        title = "Error: Missing Responses",
                                        "In a previous step, you selected physical stature as an important attribute in your decision-making process. Therefore, you must choose at least one preference for height or weight. If you selected physical stature in error, please return to the previous step and deselect that option."
                                )
                        )
                }
        })
        
        observeEvent(input$previous6, {
                shinyjs::hide("player_div5")
                
                if("Physical stature" %in% input$attributes2) {
                        shinyjs::show("player_div4")
                }
                
                else {
                        shinyjs::show("player_div3")
                }
        })
        
        observeEvent(input$next6, {
                shinyjs::hide("player_div5")
                shinyjs::show("player_div6")
        })
        
        observeEvent(input$previous7, {
                shinyjs::hide("player_div6")
                
                if("Dominant foot" %in% input$attributes2) {
                        shinyjs::show("player_div5")
                }
                
                else if("Physical stature" %in% input$attributes2) {
                        shinyjs::show("player_div4")
                }
                
                else {
                        shinyjs::show("player_div3")
                }
        })
        
        observeEvent(input$next7, {
                shinyjs::hide("player_div6")
                shinyjs::show("player_div7")
                shinyjs::disable("ranked_attributes1")
        })
        
        observeEvent(input$previous8, {
                shinyjs::hide("player_div7")
                shinyjs::show("player_div6")
        })
        
        observeEvent(input$next8, {
                shinyjs::hide("player_div7")
                shinyjs::show("player_div8")
        })
        
        observeEvent(input$previous9, {
                shinyjs::hide("player_div8")
                shinyjs::show("player_div7")
        })
        
        observeEvent(input$submit, {
                shinyjs::disable("submit")
                shinyjs::hide("player_div8")
                shinyjs::show("player_results")
        })
        
        observeEvent(input$find_player, {
                updateTabsetPanel(session, "myScout",
                                  selected = "Find a Player")
        })
        
        observeEvent(input$start_over, {
                shinyjs::disable("start_over")
                session$reload()
        })
        
        output$player_rankings <- renderUI({
                player_rankings_reactive()
        })
        
        shinyjs::disable("attributes1")
        
        ranked_attributes_reactive <- reactive({
                if(length(input$attributes2) == 4) {
                        jqui_sortable(
                                div(
                                        id = "ranked_attributes",
                                        div(span(icon("arrows", "fas")), "Transfer market value"),
                                        div(span(icon("arrows", "fas")), input$attributes2[1]),
                                        div(span(icon("arrows", "fas")), input$attributes2[2]),
                                        div(span(icon("arrows", "fas")), input$attributes2[3]),
                                        div(span(icon("arrows", "fas")), input$attributes2[4])
                                )
                        )
                }
                
                else if(length(input$attributes2) == 3) {
                        jqui_sortable(
                                div(
                                        id = "ranked_attributes",
                                        div(span(icon("arrows", "fas")), "Transfer market value"),
                                        div(span(icon("arrows", "fas")), input$attributes2[1]),
                                        div(span(icon("arrows", "fas")), input$attributes2[2]),
                                        div(span(icon("arrows", "fas")), input$attributes2[3])
                                )
                        )
                }
                
                else if(length(input$attributes2) == 2) {
                        jqui_sortable(
                                div(
                                        id = "ranked_attributes",
                                        div(span(icon("arrows", "fas")), "Transfer market value"),
                                        div(span(icon("arrows", "fas")), input$attributes2[1]),
                                        div(span(icon("arrows", "fas")), input$attributes2[2])
                                )
                        )
                }
                
                else if(length(input$attributes2) == 1) {
                        jqui_sortable(
                                div(
                                        id = "ranked_attributes",
                                        div(span(icon("arrows", "fas")), "Transfer market value"),
                                        div(span(icon("arrows", "fas")), input$attributes2[1])
                                )
                        )
                }
                
                else if(length(input$attributes2) == 0) {
                        jqui_sortable(
                                div(
                                        id = "ranked_attributes",
                                        div(span(icon("arrows", "fas")), "Transfer market value")
                                )
                        )
                }
        })
        
        output$ranked_attributes <- renderUI({
                ranked_attributes_reactive()
        })
        
        submission_table_reactive <- reactive({
                if("Physical stature" %in% input$attributes2 & "Dominant foot" %in% input$attributes2) {
                        data.frame(Questions = c("What is your maximum transfer budget (€EUR) for this player?",
                                                 "For which of the following position types are you seeking a new player?",
                                                 "Which of the following player attributes will weigh into your selection? (Note: Transfer market value and talent level are required attributes.)",
                                                 "What is the ideal height of the player you would like to fill this position?",
                                                 "What is the ideal weight of the player you would like to fill this position?",
                                                 "What is the ideal dominant foot of the player you would like to fill this position?",
                                                 "Do you envision this player as a short-, medium-, or long-term solution?",
                                                 "Drag and drop the selected attributes into a ranked order of importance. (Note: Talent level must remain the top-ranked attribute by default.)"),
                                   Answers = c(dollar(input$maximum_budget, prefix = "€"),
                                               input$position,
                                               paste0(paste0(input$attributes1, collapse = ", "), ", ", paste0(input$attributes2, collapse = ", ")),
                                               input$height,
                                               input$weight,
                                               input$dominant_foot,
                                               input$timeframe,
                                               paste0("Talent level, ", paste0(input$ranked_attributes_order$text, collapse = ", "))),
                                   stringsAsFactors = FALSE)
                }
                
                else if(!("Physical stature" %in% input$attributes2) & "Dominant foot" %in% input$attributes2) {
                        data.frame(Questions = c("What is your maximum transfer budget (€EUR) for this player?",
                                                 "For which of the following position types are you seeking a new player?",
                                                 "Which of the following player attributes will weigh into your selection? (Note: Transfer market value and talent level are required attributes.)",
                                                 "What is the ideal dominant foot of the player you would like to fill this position?",
                                                 "Do you envision this player as a short-, medium-, or long-term solution?",
                                                 "Drag and drop the selected attributes into a ranked order of importance. (Note: Talent level must remain the top-ranked attribute by default.)"),
                                   Answers = c(dollar(input$maximum_budget, prefix = "€"),
                                               input$position,
                                               paste0(paste0(input$attributes1, collapse = ", "), ", ", paste0(input$attributes2, collapse = ", ")),
                                               input$dominant_foot,
                                               input$timeframe,
                                               paste0("Talent level, ", paste0(input$ranked_attributes_order$text, collapse = ", "))),
                                   stringsAsFactors = FALSE)
                }
                
                else if("Physical stature" %in% input$attributes2 & !("Dominant foot" %in% input$attributes2)) {
                        data.frame(Questions = c("What is your maximum transfer budget (€EUR) for this player?",
                                                 "For which of the following position types are you seeking a new player?",
                                                 "Which of the following player attributes will weigh into your selection? (Note: Transfer market value and talent level are required attributes.)",
                                                 "What is the ideal height of the player you would like to fill this position?",
                                                 "What is the ideal weight of the player you would like to fill this position?",
                                                 "Do you envision this player as a short-, medium-, or long-term solution?",
                                                 "Drag and drop the selected attributes into a ranked order of importance. (Note: Talent level must remain the top-ranked attribute by default.)"),
                                   Answers = c(dollar(input$maximum_budget, prefix = "€"),
                                               input$position,
                                               paste0(paste0(input$attributes1, collapse = ", "), ", ", paste0(input$attributes2, collapse = ", ")),
                                               input$height,
                                               input$weight,
                                               input$timeframe,
                                               paste0("Talent level, ", paste0(input$ranked_attributes_order$text, collapse = ", "))),
                                   stringsAsFactors = FALSE)
                }
                
                else {
                        data.frame(Questions = c("What is your maximum transfer budget (€EUR) for this player?",
                                                 "For which of the following position types are you seeking a new player?",
                                                 "Which of the following player attributes will weigh into your selection? (Note: Transfer market value and talent level are required attributes.)",
                                                 "Do you envision this player as a short-, medium-, or long-term solution?",
                                                 "Drag and drop the selected attributes into a ranked order of importance. (Note: Talent level must remain the top-ranked attribute by default.)"),
                                   Answers = c(dollar(input$maximum_budget, prefix = "€"),
                                               input$position,
                                               paste0(paste0(input$attributes1, collapse = ", "), ", ", paste0(input$attributes2, collapse = ", ")),
                                               input$timeframe,
                                               paste0("Talent level, ", paste0(input$ranked_attributes_order$text, collapse = ", "))),
                                   stringsAsFactors = FALSE)
                }
        })
        
        output$submission_table <- renderTable({
                submission_table_reactive()
        })
        
        player_rankings_reactive <- eventReactive(input$submit, {
                topsis_attributes <- c("talent_rank_age_group", "binned_age_score", "player_rating_adjusted", input$ranked_attributes_order$text)
                topsis_attributes <- gsub("(^\\s+|\\s+$)", "", topsis_attributes)
                topsis_attributes <- gsub("Transfer market value", "market_value", topsis_attributes)
                topsis_attributes <- gsub("Physical stature", "physical_stature_score", topsis_attributes)
                topsis_attributes <- gsub("Dominant foot", "dominant_foot_score", topsis_attributes)
                topsis_attributes <- gsub("Recent form", "recent_player_rating_adjusted_per_96", topsis_attributes)
                topsis_attributes <- gsub("Versatility", "versatility", topsis_attributes)
                
                topsis_players_positions <- target_players_model_positions %>% 
                        filter(position_cluster == input$position)
                
                topsis_eligible_players <- target_players_model_values %>% 
                        filter(market_value <= input$maximum_budget,
                               id %in% topsis_players_positions$id) %>% 
                        mutate(age_preference = input$timeframe) %>% 
                        left_join(age_values, c("age_preference", "binned_age"))
                
                if("physical_stature_score" %in% topsis_attributes) {
                        topsis_eligible_players <- topsis_eligible_players %>% 
                                mutate(height_preference = input$height,
                                       weight_preference = input$weight) %>% 
                                left_join(height_values, c("height_preference", "binned_height")) %>% 
                                left_join(weight_values, c("weight_preference", "binned_weight")) %>% 
                                mutate(physical_stature_score = height_score + weight_score)
                }
                
                if("dominant_foot_score" %in% topsis_attributes) {
                        topsis_eligible_players <- topsis_eligible_players %>% 
                                mutate(dominant_foot_score = ifelse(foot == tolower(input$dominant_foot) | foot == "both", 1, 0))
                }
                
                topsis_eligible_players_matrix <- as.matrix(topsis_eligible_players[topsis_attributes])
                
                topsis_weights <- rank.ordinal.weights(rbind(data.frame(), topsis_attributes))$ranking$Weight
                
                topsis_impacts <- rep("+", length(topsis_attributes))
                topsis_impacts[which(topsis_attributes == "market_value")] <- "-"
                
                topsis_scores <- topsis(topsis_eligible_players_matrix, topsis_weights, topsis_impacts)
                
                topsis_primary_position <- target_players_model_positions %>% 
                        group_by(id) %>% 
                        summarize(primary_position = first(position_cluster))
                
                topsis_players_result <- topsis_eligible_players %>% 
                        mutate(score = topsis_scores$score) %>% 
                        arrange(desc(score)) %>% 
                        top_n(10, score) %>% 
                        left_join(topsis_primary_position, "id")
                
                bs_accordion(id = "player_rankings_accordion") %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("1"),
                                    img(src = topsis_players_result$image[1], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[1]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[1] * 100))))
                                ),
                                content = tagList(
                                        div(class = "player_body",
                                            fluidRow(
                                                    column(4,
                                                           h4("Player Information:"),
                                                           p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[1], "%B %d, %Y"), " (", topsis_players_result$age[1], ")"),
                                                           p(em("Height: "), topsis_players_result$height[1], " cm"),
                                                           p(em("Weight: "), topsis_players_result$weight[1], " kg"),
                                                           p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[1])),
                                                           p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[1])),
                                                           br(),
                                                           h4("Contract Information:"),
                                                           p(em("Current Team: "), topsis_players_result$owner_team_name[1]),
                                                           p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                           p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[1]), format(topsis_players_result$contract_expires[1], "%B %d, %Y"), "Unknown")),
                                                           p(em("Market Value: "), dollar(topsis_players_result$market_value[1], prefix = "€"))),
                                                    column(8,
                                                           h4("Career Statistics (by Season):"),
                                                           renderTable({
                                                                   target_players_season_stats %>% 
                                                                           filter(id == topsis_players_result$id[1]) %>%
                                                                           select(-id) %>% 
                                                                           mutate_if(is.numeric, as.character)
                                                           }))
                                            ))
                                )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("2"),
                                    img(src = topsis_players_result$image[2], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[2]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[2] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[2], "%B %d, %Y"), " (", topsis_players_result$age[2], ")"),
                                                   p(em("Height: "), topsis_players_result$height[2], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[2], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[2])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[2])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[2]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[2]), format(topsis_players_result$contract_expires[2], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[2], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[2]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("3"),
                                    img(src = topsis_players_result$image[3], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[3]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[3] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[3], "%B %d, %Y"), " (", topsis_players_result$age[3], ")"),
                                                   p(em("Height: "), topsis_players_result$height[3], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[3], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[3])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[3])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[3]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[3]), format(topsis_players_result$contract_expires[3], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[3], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[3]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("4"),
                                    img(src = topsis_players_result$image[4], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[4]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[4] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[4], "%B %d, %Y"), " (", topsis_players_result$age[4], ")"),
                                                   p(em("Height: "), topsis_players_result$height[4], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[4], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[4])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[4])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[4]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[4]), format(topsis_players_result$contract_expires[4], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[4], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[4]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("5"),
                                    img(src = topsis_players_result$image[5], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[5]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[5] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[5], "%B %d, %Y"), " (", topsis_players_result$age[5], ")"),
                                                   p(em("Height: "), topsis_players_result$height[5], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[5], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[5])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[5])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[5]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[5]), format(topsis_players_result$contract_expires[5], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[5], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[5]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("6"),
                                    img(src = topsis_players_result$image[6], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[6]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[6] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[6], "%B %d, %Y"), " (", topsis_players_result$age[6], ")"),
                                                   p(em("Height: "), topsis_players_result$height[6], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[6], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[6])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[6])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[6]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[6]), format(topsis_players_result$contract_expires[6], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[6], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[6]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("7"),
                                    img(src = topsis_players_result$image[7], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[7]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[7] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[7], "%B %d, %Y"), " (", topsis_players_result$age[7], ")"),
                                                   p(em("Height: "), topsis_players_result$height[7], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[7], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[7])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[7])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[7]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[7]), format(topsis_players_result$contract_expires[7], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[7], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[7]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("8"),
                                    img(src = topsis_players_result$image[8], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[8]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[8] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[8], "%B %d, %Y"), " (", topsis_players_result$age[8], ")"),
                                                   p(em("Height: "), topsis_players_result$height[8], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[8], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[8])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[8])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[8]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[8]), format(topsis_players_result$contract_expires[8], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[8], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[8]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("9"),
                                    img(src = topsis_players_result$image[9], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[9]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[9] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[9], "%B %d, %Y"), " (", topsis_players_result$age[9], ")"),
                                                   p(em("Height: "), topsis_players_result$height[9], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[9], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[9])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[9])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[9]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[9]), format(topsis_players_result$contract_expires[9], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[9], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[9]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        )) %>% 
                        bs_append(title = tagList(
                                div(class = "player_heading",
                                    p("10"),
                                    img(src = topsis_players_result$image[10], width = "60px", height = "60px"),
                                    p(topsis_players_result$name[10]),
                                    div(class = "player_heading_score",
                                        p("Score:"),
                                        p(round(topsis_players_result$score[10] * 100))))
                        ),
                        content = tagList(
                                div(class = "player_body",
                                    fluidRow(
                                            column(4,
                                                   h4("Player Information:"),
                                                   p(em("Birthdate (Age): "), format(topsis_players_result$birth_date[10], "%B %d, %Y"), " (", topsis_players_result$age[10], ")"),
                                                   p(em("Height: "), topsis_players_result$height[10], " cm"),
                                                   p(em("Weight: "), topsis_players_result$weight[10], " kg"),
                                                   p(em("Dominant Foot: "), toTitleCase(topsis_players_result$foot[10])),
                                                   p(em("Primary Position: "), toTitleCase(topsis_players_result$primary_position[10])),
                                                   br(),
                                                   h4("Contract Information:"),
                                                   p(em("Current Team: "), topsis_players_result$owner_team_name[10]),
                                                   p(em("On Loan: "), ifelse(topsis_players_result$on_loan == TRUE, "Yes", "No")),
                                                   p(em("Contract Expires: "), ifelse(!is.na(topsis_players_result$contract_expires[10]), format(topsis_players_result$contract_expires[10], "%B %d, %Y"), "Unknown")),
                                                   p(em("Market Value: "), dollar(topsis_players_result$market_value[10], prefix = "€"))),
                                            column(8,
                                                   h4("Career Statistics (by Season):"),
                                                   renderTable({
                                                           target_players_season_stats %>% 
                                                                   filter(id == topsis_players_result$id[10]) %>%
                                                                   select(-id) %>% 
                                                                   mutate_if(is.numeric, as.character)
                                                   }))
                                    ))
                        ))
        })
})