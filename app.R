library(shiny)
library(bslib)
library(gt)
library(googlesheets4)

# d <- tibble::tibble(
#     id = as.character(seq(1,5,1)),
#     name = c("Joe", "Lisa", "Steven", "Albert", "Vanessa"),
#     occurrence_type = c("absent", "other", "tardy", "absent", "absent"),
#     reason = c("illness", "emergency", "personal", "childcare", "illness"),
#     notes = c("has doctor's note", "", "missed bus", "daycare closed", "no note"),
#     date = as.Date(c("10-12-2024", "10-12-2024", "10-13-2024", "10-15-2024", "10-15-2024"), format = "%m-%d-%Y"),
#     time = c("9:04", "8:33", "10:15", "14:34", "17:12")
#     )

{
    # Set authentication token to be stored in a folder called `.secrets`
    options(gargle_oauth_cache = ".secrets")
    
    # Authenticate manually
    #gs4_auth()
    
    # If successful, the previous step stores a token file.
    # Check that a file has been created with:
    #list.files(".secrets/")
    
    # Check that the non-interactive authentication works by first deauthorizing:
    #gs4_deauth()
    
    # Authenticate using token. If no browser opens, the authentication works.
    gs4_auth(cache = ".secrets", 
             email = "community.table.food.service@gmail.com")
    
    #write_sheet(d)
    ss <- gs4_get("https://docs.google.com/spreadsheets/d/1isvCY_g_updwG3N0xct6CHg5PwAN1AR_JEFbrIhc-o8/edit?gid=1275446717#gid=1275446717")
    d <- read_sheet(ss, "occurrences_data")
}

ui <- page_navbar(
    title = "Employee Attendance Tracker",
    sidebar = sidebar(
        selectInput(
            inputId = "employee_id",
            label = "Employee ID:",
            choices = seq(1,100,1),
            selectize = TRUE,
            selected = NULL
        ),
        
        selectInput(
            inputId = "first_name",
            label = "Employee first name:",
            choices = d$first_name,
            selectize = TRUE,
            selected = NULL
        ),
        
        selectInput(
            inputId = "last_name",
            label = "Employee last name:",
            choices = d$last_name,
            selectize = TRUE,
            selected = NULL
        ),
        
        selectInput(
            inputId = "type",
            label = "Call-off or Tardy:",
            choices = c("absent", "tardy", "other"),
            selectize = TRUE
        ),
        selectInput(
            inputId = "reason",
            label = "Occurrence reason:",
            choices = c("illness employee" = "illness_emp",
                        "illness family" = "illness_fam",
                        "PTO unscheduled" = "pto_unsched",
                        "PTO scheduled" = "pto_sched",
                        "bereavement",
                        "late grace period" = "late_grace",
                        "late 7 to 60 mins" = "late_less_hour",
                        "late 61+ mins" = "late_more_hour",
                        "FMLA" = "fmla",
                        "resignation",
                        "no reason given" = "no_reason", 
                        "jury duty" = "jury_duty",
                        "other"),
            selectize = TRUE
        ),
        
        dateInput(
            inputId = "date",
            label = "Date: ",
            format = "mm-dd-yyyy",
            value = as.Date(Sys.Date(), format = "%m-%d-%Y")
        ),
        
        textInput(
            inputId = "notes",
            label = "Notes:"
        ),
        
        checkboxInput(
            inputId = "grace",
            label = "Check if grace given"
        ),
        
        selectizeInput(
            inputId = "supervisor",
            label = "Supervisor: ",
            choices = d$supervisor
        ),
        
        actionButton(
            inputId = "add_row",
            label = "Enter"
        ),
        hr(),
        actionButton(
            inputId = "save",
            icon = icon("save"),
            label = "Save data"
        )
    ),
    
    nav_spacer(),
    nav_panel("Occurrence table",
                  layout_column_wrap(
                      width = 1/2,
                      card(gt_output("table")),
                      card(gt_output("tracker"))
                  )
              ),
    nav_panel("Insights",
              card())
)

server <- function(input, output, session) {
    
    d <- reactiveVal({
        d
    })

    observeEvent(input$add_row, {
        
        d <- d() |> 
            dplyr::add_row(
                id = input$employee_id,
                first_name = input$first_name,
                last_name = input$last_name,
                type = input$type,
                reason = input$reason,
                notes = input$notes,
                date = input$date,
                grace = input$grace,
                supervisor = input$supervisor
            )
        
        d(d)


    })
    
    output$table <- render_gt({
        
        d() |> 
            tidyr::unite("name", c(first_name, last_name), remove = TRUE, sep = " ")  |> 
            gt() |>
            cols_label(
                id = "Employee ID",
                name = "Name",
                type = "Type",
                reason = "Call-off or\nTardy Reason",
                notes = "Notes",
                date = "Date",
                grace = "Grace given?",
                supervisor = "Supervisor"
            )

    })
    
    output$tracker <- render_gt({
        

        d() |>
            tidyr::unite("name", c(first_name, last_name), remove = TRUE, sep = " ")  |>
            dplyr::mutate(point_day = ifelse(grace == FALSE, 
                                             ifelse(type == "tardy", .25, 1),
                                             0)) |> 
            dplyr::mutate(total_points = sum(point_day),
                          tot_num_occurrences = dplyr::n(), .by = id) |> 
            dplyr::select(id, name, total_points, tot_num_occurrences) |>
            dplyr::slice_tail(n = 1, by = id) |>
            gt() |> 
            data_color(
                columns = total_points,
                palette = "YlOrRd",
                domain = 0:15,
                method = "bin",
                bins = c(0,6,8,15)
            ) |>
            cols_label(
                id = "Employee ID",
                name = "Name",
                total_points = "Points",
                tot_num_occurrences = "Total Number of\nCall-offs and Tardies",
            )


    })
    
    observeEvent(input$save, {
        
        sheet_write(
            d(),
            ss,
            "occurrences_data")
        
        sheet_write(
            d() |>
                tidyr::unite("name", c(first_name, last_name), remove = TRUE, sep = " ")  |>
                dplyr::mutate(point_day = ifelse(grace == FALSE, 
                                                 ifelse(type == "tardy", .25, 1),
                                                 0)) |> 
                dplyr::mutate(total_points = sum(point_day),
                              tot_num_occurrences = dplyr::n(), .by = id) |> 
                dplyr::select(id, name, total_points, tot_num_occurrences) |>
                dplyr::slice_tail(n = 1, by = id),
            ss,
            "tracker"
        )
        
        showModal(modalDialog(
            title = "Success!",
            tags$p("Data successfully saved to drive ", tags$a(href = ss$spreadsheet_url,
                                                               "here",
                                                               target = "_blank"))
        ))
    })

    
}

shinyApp(ui = ui, server = server)
