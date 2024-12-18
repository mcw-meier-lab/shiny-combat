#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(bslib)
library(ggplot2)
library(dplyr)

# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = c('numpy', 'pandas', 'neuroHarmonize')


# Example data path (adjust as needed)
example_data_path <- "example.csv"

ui <- page_sidebar(
    sidebar = sidebar(
        fileInput(
            "file",
            "Choose CSV file",
            accept=".csv",
            multiple=FALSE
        ),
        actionButton(
            "load_example",
            "Load Example Dataset",
            class_="btn-secondary"
        ),
        radioButtons(
            "version",
            "Combat version",
            choices=c(
                "Fortin neuroCombat",
                "Pomponio neuroHarmonize",
                "ENIGMA Combat",
                "Beer longCombat"
            ),
            selected="Fortin neuroCombat"
        ),
        conditionalPanel(
            condition = "input.version == 'Fortin neuroCombat'",
            radioButtons(
                "combat_eb",
                "Use Empirical Bayes",
                choices=c("True","False"),
                selected="True"
            ),
            radioButtons(
                "combat_parametric",
                "Use parametric adjustments",
                choices=c("True","False"),
                selected="True"
            ),
        ),
        conditionalPanel(
            condition = "input.version == 'Pomponio neuroHarmonize'",
            radioButtons(
                "neuro_eb",
                "Use Empirical Bayes",
                choices=c("True","False"),
                selected="True"
            ),
            selectizeInput(
                "smooth_terms",
                "Choose Non-linear Covariates",
                choices=character(0),
                multiple=TRUE
            )
        ),
        conditionalPanel(
            condition = "input.version == 'ENIGMA Combat'",
            radioButtons(
                "enigma_eb",
                "Use Empirical Bayes",
                choices=c("True","False"),
                selected="True"
            )
        ),
        conditionalPanel(
            condition = "input.version == 'Beer longCombat'",
            selectizeInput(
                "subject_var",
                "Choose and ID variable",
                choices=character(0),
                multiple=FALSE
            ),
            selectizeInput(
                "time_var",
                "Choose a time variable",
                choices=character(0),
                multiple=FALSE
            ),
            selectizeInput(
                "random_effect",
                "Choose a random effect",
                choices=character(0),
                multiple=FALSE
            )
        ),
        selectizeInput(
            "batch_var",
            "Choose a batch/site variable",
            choices=character(0),
            multiple=FALSE
        ),
        selectizeInput(
            "voi",
            "Choose variables to combat adjust",
            choices=character(0),
            multiple=TRUE
        ),
        radioButtons(
            "plot_type",
            "Select Plot Type",
            choices=c(
                "Boxplot",
                "Norm",
                "KDE",
                "Time Series"
            ),
            selected="Boxplot"
        ),
        textInput(
            "model_desc",
            "Model Description",
            placeholder="Describe the model"
        ),
        actionButton(
            "run_btn",
            "Run Combat",
            icon=icon("play"),
            class_="btn-primary"
        ),
        downloadButton(
            "download_adjusted",
            "Download Adjusted Dataset",
            class_="btn-secondary"
        ),
        title="Setup",
        width=300

    ),
    page_fluid(layout_columns(
        value_box(
            "Combat version",
            uiOutput("combat_version"),
            showcase=icon("code")
        ),
        value_box(
            "Model",
            uiOutput("model"),
            showcase=icon("gear")
        ),
        fill=FALSE
    ),
    card(
        DTOutput("table"),
        full_screen=TRUE
    ),
    h4("Plots"),
    card(
        card_header(
            "Before Adjustment (Click the '...' to change variables)",
            popover(
                icon("ellipsis"),
                uiOutput("before_radio"),
                radioButtons(
                    "before_voi",
                    "Variable of Interest",
                    choices=character(0),
                    selected=character(0),
                    inline=TRUE
                ),
                title="View another variable",
                placement="top"
            ),
            class="d-flex justify-content-between align-items-center"
        ),
        plotlyOutput("plot_before"),
        full_screen=TRUE,
    ),
    card(
        card_header(
            "After Adjustment (Click the '...' to change variables)",
            popover(
                icon("ellipsis"),
                uiOutput("after_radio"),
                radioButtons(
                    "after_voi",
                    "Variable of Interest",
                    choices=character(0),
                    selected=character(0),
                    inline=TRUE
                ),
                title="View another variable",
                placement="top",
            ),
            class="d-flex justify-content-between align-items-center"
        ),
        plotlyOutput("plot_after"),
        full_screen=TRUE
    ),
    h4("Corrected Data Table"),
    card(
        DTOutput("output_table"),
        full_screen=TRUE
    ),
    includeCSS("styles.css"),
    fillable=TRUE,
    title="Combat Data Harmonization",
    theme = bs_theme(bootswatch="pulse")
))


server <- function(input, output, session) {
    # Set up Python environment with reticulate
    virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
    #python_path = Sys.getenv('PYTHON_PATH')

    # Create virtual env and install dependencies
    reticulate::virtualenv_create(envname = virtualenv_dir)
    reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES)
    reticulate::use_virtualenv(virtualenv_dir, required = T)


    pd <- reticulate::import("pandas")
    data_store <- reactiveVal(NULL)
    combat_store <- reactiveVal(NULL)

    batch <- reactive({input$batch_var})
    voi <- reactive({input$voi})
    subject <- reactive({input$subject_var})
    time_var <- reactive({input$time_var})
    effect <- reactive({input$random_effect})
    model_desc <- reactive({input$model_desc})
    version <- reactive({input$version})


    load_data <- function(file_path) {
        pd$read_csv(file_path)
    }

    observeEvent(input$file, {
        req(input$file)
        data <- load_data(input$file$datapath)
        data_store(data)
        update_ui_inputs(data)
    })

    observeEvent(input$load_example, {
        data <- load_data(example_data_path)
        data_store(data)
        update_ui_inputs(data)
    })

    update_ui_inputs <- function(data) {
        choices = colnames(data)
        updateSelectizeInput(session, "batch_var", choices=choices)
        updateSelectizeInput(session, "subject_var", choices=choices)
        updateSelectizeInput(session, "voi", choices=choices)
        updateSelectizeInput(session, "smooth_terms", choices=choices)
        updateSelectizeInput(session, "time_var", choices=choices)
        updateSelectizeInput(session, "random_effect", choices=choices)
    }

    preprocess_data <- function(data) {
        filtered_df <- data[,voi()]
        filtered_df[] <- lapply(filtered_df, as.numeric)
        filtered_df[[batch()]] <- data[[batch()]]
        filtered_df[[subject()]] <- data[[subject()]]

        for (c in voi()) {
            if (tolower(c) == "sex" || grepl("sex", tolower(c))) {
                factor_var <- model.matrix(~ data[[c]] - 1)
                colnames(factor_var) <- paste(c, levels(factor(data[[c]])), sep = "_")

                filtered_df <- cbind(filtered_df, factor_var)
            }
        }

        return(filtered_df)
    }

    validate_text <- function(text) {
        if (is.null(text) || text == "") {
            return("Waiting for model description.")
        }

        if (nchar(text) > 200) {
            return("Error: Model description is too long. Please limit to 200 characters.")
        }

        # Allow only alphanumeric characters, spaces, and basic punctuation
        if (!grepl("^[a-zA-Z0-9\\s\\+\\*\\~]+$", text)) {
            return("Error: Model description contains invalid characters. Use only letters, numbers, spaces, and basic punctuation.")
        }

        return(NULL)
    }

    get_model <- function() {
        model_txt <- model_desc()
        validation_err <- validate_text(model_txt)

        if (!is.null(validation_err)) {
            return(validation_err)
        }

        return(model_txt)
    }

    output$combat_version <- renderUI({
        version()
    })

    output$model <- renderUI({
        get_model()
    })

    output$table <- renderDT({
        data <- data_store()
        req(data)
        datatable(as.data.frame(data))
    })

    create_plot <- function(df, voi, batch, plot_type) {
        if (plot_type == "Boxplot") {
            groups <- unique(df[[batch]])
            fig <- plot_ly()
            for (g in groups) {
                fig <- fig %>%
                    add_trace(
                        y = df[df[[batch]] == g, voi],
                        type = "box",
                        name = paste(batch, "=", g),
                        boxpoints = "all",
                        jitter = 0.3,
                        pointpos = -1.8
                    )
            }
        }
        else if (plot_type == "Norm") {
            groups <- unique(df[[batch]])
            len <- length(groups)
            if (len <= 2) {
                len <- 3
            }
            colors <- RColorBrewer::brewer.pal(len, "Set2")

            # Prepare normalized data
            norm_data <- lapply(groups, function(g) {
                df[df[[batch]] == g, voi]
            })

            # Create a distribution plot
            fig <- plot_ly()

            for (i in seq_along(groups)) {
                fig <- fig %>%
                    add_trace(
                        x = norm_data[[i]],
                        type = "histogram",
                        histnorm = "probability density",
                        name = groups[i],
                        marker = list(color = colors[i], opacity = 0.5)
                    ) %>%
                    add_trace(
                        x = density(norm_data[[i]])$x,
                        y = density(norm_data[[i]])$y,
                        type = "scatter",
                        mode = "lines",
                        name = paste(groups[i], "Curve"),
                        line = list(color = colors[i], width = 2)
                    )
            }

            # Update layout
            fig <- fig %>%
                layout(
                    title = voi,
                    xaxis = list(title = voi),
                    yaxis = list(title = "Density"),
                    barmode = "overlay"
                )

        }
        else if (plot_type == "KDE") {
            groups <- unique(df[[batch]])
            len <- length(groups)
            if (len <= 2) {
                len <- 3
            }
            colors <- RColorBrewer::brewer.pal(len, "Set2")

            # Prepare KDE data
            kde_data <- lapply(groups, function(g) {
                df[df[[batch]] == g, voi]
            })

            # Create a distribution plot
            fig <- plot_ly()

            for (i in seq_along(groups)) {
                fig <- fig %>%
                    add_trace(
                        x = kde_data[[i]],
                        type = "histogram",
                        histnorm = "probability density",
                        name = groups[i],
                        marker = list(color = colors[i], opacity = 0.5)
                    ) %>%
                    add_trace(
                        x = density(kde_data[[i]])$x,
                        y = density(kde_data[[i]])$y,
                        type = "scatter",
                        mode = "lines",
                        name = paste(groups[i], "KDE"),
                        line = list(color = colors[i], width = 2)
                    )
            }

            # Update layout
            fig <- fig %>%
                layout(
                    title = voi,
                    xaxis = list(title = voi),
                    yaxis = list(title = "Density"),
                    barmode = "overlay"
                )
        }
        else if (plot_type == "Time Series") {
            #fig <-
        }
        fig
    }

    before_var <- reactive({input$before_voi})
    after_var <- reactive({input$after_voi})
    plot_type <- reactive({input$plot_type})

    output$plot_before <- renderPlotly({
        data <- data_store()
        req(data,before_var())
        create_plot(data, before_var(), batch(), plot_type())
    })

    output$plot_after <- renderPlotly({
        data <- combat_store()
        req(data)
        create_plot(data, after_var(), batch(), plot_type())
    })

    observeEvent(input$voi, {
        if (length(input$voi) > 0) {
            voi <- input$voi[1]
        }
        else {
            voi <- character(0)
        }

        updateRadioButtons(session,"before_voi",choices=input$voi,selected=voi)
        updateRadioButtons(session,"after_voi",choices=input$voi,selected=voi)
    })

    observeEvent(input$run_btn, {
        data <- data_store()

        df <- preprocess_data(data)
        model <- model.matrix(as.formula(model_desc()),data)
        version <- version()


        if (version == "Fortin neuroCombat") {
            library(neuroCombat)
            eb <- as.logical(input$combat_eb)
            param <- as.logical(input$combat_parametric)
            full_combat <- neuroCombat(
                dat=t(df[,voi()]),
                batch=df[[batch()]],
                model,
                eb=eb,
                parametric=param,
            )
            combat_df <- as.data.frame(t(full_combat$dat.combat))
            combat_df <- rename_with(combat_df,~paste0(.,"_adj"))
            combat_df <- cbind(combat_df,data)
        }
        else if (version == "Pomponio neuroHarmonize") {
            neuroHarmonize <- reticulate::import("neuroHarmonize")
            np <- reticulate::import("numpy")
            df <- df[complete.cases(df), ]
            names(df)[names(df) == batch()] <- "SITE"
            ncovar <- df[["SITE"]]
            if (is.character(ncovar)) {
                ncovar <- gsub(" ", "_", ncovar)
            }
            model_matrix <- as.data.frame(model)
            model_matrix[["SITE"]] <- ncovar
            if ("(Intercept)" %in% names(model_matrix)) {
                model_matrix <- model_matrix[, !(names(model_matrix) %in% "(Intercept)")]
            }
            eb <- as.logical(input$neuro_eb)
            if (length(input$smooth_terms) > 0) {
                smooth_terms = input$smooth_terms
            }
            else {
                smooth_terms = list()
            }
            adjusted <- neuroHarmonize$harmonizationLearn(
                np$asarray(df[,voi()]),
                model_matrix,
                eb=eb,
                smooth_terms=smooth_terms
            )
            combat_df <- as.data.frame(py_to_r(adjusted[[2]]))
            combat_df <- rename_with(combat_df,~paste0(.,"_adj"))
            combat_df <- cbind(combat_df, data)
        }
        else if (version == "ENIGMA Combat") {
            library(combat.enigma)
            eb <- as.logical(input$enigma_eb)
            harmonized <- combat_fit(df[,voi()],as.factor(df[[batch()]]),model,eb)
            applied <- combat_apply(harmonized,df[,voi()],as.factor(df[[batch()]]),model)
            combat_df <- as.data.frame(applied$dat.combat)
            combat_df <- rename_with(combat_df,~paste0(.,"_adj"))
            combat_df <- cbind(combat_df, data)
        }
        else if (version == "Beer longCombat") {
            library(longCombat)
            long <- longCombat(
                idvar=subject(),
                timevar=time_var(),
                batchvar=batch(),
                features=voi(),
                formula=model,
                ranef=paste0("(1|",effect(),")"),
                data=df
            )
            combat_df <- long$data_combat
            combat_df <- rename_with(combat_df,~paste0(.,"_adj"))
            combat_df <- cbind(combat_df,data)
        }

        combat_store(combat_df)
    })

    output$output_table <- renderDT({
        data <- combat_store()
        req(data)
        datatable(as.data.frame(data))
    })

    output$download_adjusted <- downloadHandler(
        filename = "adjusted_dataset.csv",
        content = function(file) {
            data <- combat_store()
            write.csv(data,file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
