navbarPage("Run Forest. Run!", id="run",
           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                          #includeScript("gomap.js")
                        ),
                        leaflet::leafletOutput(outputId = "map", width="100%",
                                               height="100%"),

                        absolutePanel(id = "trackpoint",
                                      class = "panel panel-default",
                                      fixed = TRUE,
                                      draggable = TRUE, top = "auto",
                                      bottom = 5, left = 5, right = 5,
                                      height = "auto",
                                      plotly::plotlyOutput(
                                        outputId = "trackpoint_plot")
                                      ),
                        absolutePanel(id = "distribution",
                                      class = "panel panel-default",
                                      fixed = TRUE, top=50, right=5,
                                      left = "auto", width = 250,
                                      height = 100,
                                      selectInput(inputId = "lap_type",
                                                  label = "Split by:",
                                                  choices = c("device laps",
                                                              "equal distance"),
                                                  selected = "From device",
                                                  multiple = FALSE)
                                      ),
                        absolutePanel(id = "distribution2",
                                      class = "panel panel-default",
                                      fixed = TRUE, top=170, right=5,
                                      left = "auto", width = 250,
                                      height = 200,
                                      plotly::plotlyOutput(
                                        outputId = "distribution_lap_plot")
                        ),
                        absolutePanel(id = "distribution3",
                                      class = "panel panel-default",
                                      fixed = TRUE, top=390, right=5,
                                      left = "auto", width = 250,
                                      height = 50,
                                      plotly::plotlyOutput(
                                        outputId = "distribution_zone_plot")
                                    )
                        )
                    )
           )


