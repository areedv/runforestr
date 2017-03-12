navbarPage("Run Forest. Run!", id="run",
           tabPanel("Data",
                    absolutePanel(id = "test_panel",
                                  class = "panel panel-default",
                                  fixed = TRUE, top=140, right=500,
                                  left = "auto", width = 400,
                                  height = "auto",
                                  #verbatimTextOutput("test_panel")
                                  fileInput("selected_data", "Load file",
                                            multiple = FALSE,
                                            accept = ".tcx")
                    )
                    ),
           if (1==1) {
            interactiveMapInput()
           }
           # tabPanel("Interactive map",
           #          div(class="outer",
           #
           #              tags$head(
           #                # Include our custom CSS
           #                includeCSS("styles.css")
           #                #includeScript("gomap.js")
           #              ),
           #              leaflet::leafletOutput(outputId = "map", width="100%",
           #                                     height="100%"),
           #
           #              absolutePanel(id = "laps",
           #                            class = "panel panel-default",
           #                            fixed = TRUE, top=140, right=5,
           #                            left = "auto", width = 250,
           #                            height = "auto",
           #                            plotly::plotlyOutput(
           #                              outputId = "distribution_lap_plot")
           #              ),
           #              absolutePanel(id = "laps_select",
           #                            class = "panel panel-default",
           #                            fixed = TRUE, top=60, right=5,
           #                            left = "auto", width = 250,
           #                            height = 75,
           #                            selectInput(inputId = "lap_type",
           #                                        label = "Split by:",
           #                                        choices = c("device laps",
           #                                                    "equal distance"),
           #                                        selected = "From device",
           #                                        multiple = FALSE)
           #              ),
           #              absolutePanel(id = "intensity_zones",
           #                            class = "panel panel-default",
           #                            fixed = TRUE, bottom = "auto", left = 5,
           #                            right = "auto", width = 210,
           #                            height = 110, top = 520,
           #                            plotly::plotlyOutput(
           #                              outputId = "distribution_zone_plot")
           #                          ),
           #              absolutePanel(id = "trackpoint",
           #                            class = "panel panel-default",
           #                            fixed = TRUE,
           #                            draggable = TRUE, top = "auto",
           #                            bottom = 5, left = 5, right = 5,
           #                            height = 400,
           #                            plotly::plotlyOutput(
           #                              outputId = "trackpoint_plot")
           #              )
           #              )
           #          )
           )


