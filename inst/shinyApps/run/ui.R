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
                                      plotly::plotlyOutput(outputId = "plot")
                                      ),
                        absolutePanel(id = "test",
                                      class = "panel panel-default",
                                      fixed = TRUE, top=100, right=10,
                                      left = "auto", width = 600,
                                      height = "auto",
                                      verbatimTextOutput("hover")
                                      )
                        )
                    )
           )


