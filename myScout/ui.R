shinyUI(
        tagList(
                useShinyjs(),
                tags$head(
                        HTML('<link rel="stylesheet" href="https://use.typekit.net/biz3ufq.css">'),
                        HTML('<link rel="stylesheet" href="https://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css">'),
                        includeCSS("myScout.css"),
                        tags$title("myScout | Find the Best Player to Fit Your System")
                        ),
                navbarPage("", 
                           id = "myScout",
                           tabPanel("How It Works",
                                    div(class = "intro",
                                        div(class = "intro_banner",
                                            div(class = "intro_banner_photo"),
                                            div(class = "intro_banner_heading", h1("How myScout Works"), h2("Find the Best Player to Fit Your System"))
                                        )),
                                    fluidRow(
                                            column(8,
                                                   p("Professional soccer is uniquely different from other professional sporting competitions in a multitude of ways. It is played in virtually every country in the world, clubs and club owners often operate at a loss each year, and players and their agents have an outsized amount of influence in the transfer market. This makes the job of clubs—namely the individuals tasked with building a talented and cohesive squad—equal parts exhilarating and challenging."),
                                                   p("That’s where myScout comes in. myScout is a decision support system that assists professional soccer clubs worldwide with the identification, scouting, and acquisition of world-class talent. The primary mission is to provide a ranked-order list of transfer targets that best account for the desires, strategic goals, and risk defined by the user. myScout uses event-level data from more than 2,000 weekly matches worldwide, with the goal of providing scouts, technical directors, and other key leaders with the real-time information necessary to make seven- to nine-figure investments."),
                                                   p("In order to aid selection, myScout will require demographic information and match-level statistics for each player. Central to the decision of any scouting department is the target player’s future and potential skill level, which, depending on his position, could be derived from any number of features. Also important is the target player’s contract information and market value."),
                                                   p("When paired with a quantitative assessment of his past performances and a prediction of his future ones, scouting departments will be provided with the current and potential value of their investments. In addition to potential value, basic demographic information—height, weight, preferred foot—can be selected to better identify talent who meet the needs of a team."),
                                                   p('myScout will take these factors and provide a set of "matched" players who are ranked according to how closely they meet the needs defined in the search process. From there, users can learn more about these players and decide who they will sign during the upcoming transfer window.'),
                                                   p('Select "Find a Player" below to start your talent search.'),
                                                   actionButton("find_player", "Find a Player"),
                                                   offset = 2)
                                    )
                           ),
                           tabPanel("Find a Player",
                                    fluidRow(
                                            column(10,
                                                   h2("Find a Player"),
                                                   div(
                                                           id = "getting_started",
                                                           p("Welcome to myScout, the automated decision support system that will help you field a team that meets your needs. Please respond to the following prompts regarding your transfer budget, your desired player’s position, demographic information, and contract length. After setting the parameters for your decision, you will be provided with 10 player options that are best matched to your requirements along with their career statistics and contract information."),
                                                           p('Select “Get Started” to begin.'),
                                                           actionButton("next1", "Get Started")
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div1",
                                                                   h4("What is your maximum transfer budget (€EUR) for this player?"),
                                                                   sliderInput("maximum_budget", NULL, min = 0, max = 50000000, value = 2500000, step = 250000, pre = "€", sep = ",", width = "80%"),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous2", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("next2", "Next")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div2",
                                                                   h4("For which of the following position types are you seeking a new player?"),
                                                                   selectizeInput("position", NULL,
                                                                                  selected = NULL,
                                                                                  choices = position_clusters,
                                                                                  multiple = FALSE),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous3", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("next3", "Next")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div3",
                                                                   h4("Which of the following player attributes will weigh into your selection? (Note: Transfer market value and talent level are required attributes.)"),
                                                                   checkboxGroupInput(
                                                                           "attributes1",
                                                                           NULL,
                                                                           choices = c("Transfer market value", "Talent level"),
                                                                           selected = c("Transfer market value", "Talent level"),
                                                                           width = "100%"
                                                                   ),
                                                                   checkboxGroupInput(
                                                                           "attributes2",
                                                                           NULL,
                                                                           choices = c("Physical stature", "Dominant foot", "Recent form", "Versatility"),
                                                                           width = "100%"
                                                                   ),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous4", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("next4", "Next")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div4",
                                                                   h4("What is the ideal height of the player you would like to fill this position?"),
                                                                   selectizeInput("height", NULL,
                                                                                  selected = NULL,
                                                                                  choices = c("No Preference", "Short", "Median", "Tall"),
                                                                                  multiple = FALSE),
                                                                   h4("What is the ideal weight of the player you would like to fill this position?"),
                                                                   selectizeInput("weight", NULL,
                                                                                  selected = NULL,
                                                                                  choices = c("No Preference", "Light", "Median", "Heavy"),
                                                                                  multiple = FALSE),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous5", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("next5", "Next")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div5",
                                                                   h4("What is the ideal dominant foot of the player you would like to fill this position?"),
                                                                   selectizeInput("dominant_foot", NULL,
                                                                                  selected = NULL,
                                                                                  choices = c("Left", "Right"),
                                                                                  multiple = FALSE),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous6", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("next6", "Next")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div6",
                                                                   h4("Do you envision this player as a short-, medium-, or long-term solution?"),
                                                                   selectizeInput("timeframe", NULL,
                                                                                  selected = NULL,
                                                                                  choices = c("Short-Term", "Medium-Term", "Long-Term"),
                                                                                  multiple = FALSE),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous7", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("next7", "Next")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div7",
                                                                   h4("Drag and drop the selected attributes into a ranked order of importance. (Note: Talent level must remain the top-ranked attribute by default.)"),
                                                                   div(id = "talent_level", span(icon("lock", "fas")), "Talent level"),
                                                                   uiOutput("ranked_attributes"),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous8", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("next8", "Next")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_div8",
                                                                   h4("Review your responses below before submitting."),
                                                                   tableOutput("submission_table"),
                                                                   fluidRow(column(6,
                                                                                   actionButton(
                                                                                           "previous9", "Previous"
                                                                                   )),
                                                                            column(6,
                                                                                   actionButton("submit", "Submit")))
                                                           )
                                                   ),
                                                   
                                                   hidden(
                                                           div(
                                                                   id = "player_results",
                                                                   uiOutput("player_rankings"),
                                                                   actionButton("start_over", "Start Over")
                                                           )
                                                   ),
                                                   offset = 1)
                                    )
                           )
                )
        )
)