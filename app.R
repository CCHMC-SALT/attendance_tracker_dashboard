library(shiny)
library(bslib)
library(gt)


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
    salt_board <- pins::board_connect(auth = "manual",
                                      server = Sys.getenv("CONNECT_SERVER"),
                                      key = Sys.getenv("CONNECT_API_KEY"))
    
    d_all <- pins::pin_read(salt_board,
                        'vanug6_117748@cchmc.org/food_service_staff_listing')

    
    
}

ui <- page_navbar(
    title = "Attendance Tracking Dashboard",
    sidebar = sidebar(
        shinyWidgets::pickerInput(
            inputId = "eid_name",
            choices = d_all$eid_name,
            multiple = FALSE,
            label = "Select employee:",
            options = list(
                title = "nobody selected",
                `live-search` = TRUE
            )
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
            inputId = "manager",
            label = "Manager: ",
            choices = d_all$manager_name
        ),
        
        actionButton(
            inputId = "add_row",
            label = "Enter",
            style = "background-color: #BECFB2;"
        ),
        
        width = 350
    ),
    
    nav_spacer(),
    nav_panel("Occurrence table",
                  layout_column_wrap(
                      width = 1/2,
                      card(
                          card_header("Staff listing"),
                          gt_output("table")
                      ),
                      
                      card(
                          card_header("New entries"),
                          gt_output("new_rows"),
                          actionButton(
                              inputId = "save",
                              icon = icon("save"),
                              label = "Save data",
                              style = "background-color: #BECFB2;"
                          )),
                      
                      card(
                          card_header("Existing entries"),
                          gt_output("existing_rows")
                      ),
                      
                      card(
                          card_header("Point counter"),
                          gt_output("point_counter"))
                          
                  )
    ),
    nav_panel("Insights",
              card())
)

server <- function(input, output, session) {
    
    d <- reactiveVal({
        d_all
    })
    
    d_track <- reactiveVal({
        
        d_track <- pins::pin_read(
            salt_board,
            'vanug6_117748@cchmc.org/food_service_attendance_tracking_dataset')
        
        d_track$date <- as.Date(d_track$date, format = "%m-%d-%Y")
        
        as.data.frame(d_track)
    })
    
    

    observeEvent(input$add_row, {
        
        d_track <- d_track() |> 
            dplyr::add_row(
                eid_name = input$eid_name,
                type = input$type,
                reason = input$reason,
                notes = input$notes,
                date = input$date,
                grace = input$grace,
                manager_name = input$manager
            )
        
        d_track(d_track)


    })
    
    output$table <- render_gt({
        
        d() |> 
            dplyr::select(-c(eid, name)) |> 
            gt() |> 
            cols_label(
                eid_name = "Employee",
                total_length_of_service = "Total tenure",
                current_length_of_service = "Current tenure",
                cost_center_number = "Cost center number",
                cost_center_name = "Cost center name",
                manager_name = "Manger",
                work_space = "Location"
            ) |> 
            opt_interactive(
                use_search = TRUE,
                page_size_default = 5
            )
        
    })
    
    output$new_rows <- render_gt({
        
        req(input$add_row)
        
        d_track() |> 
            dplyr::filter(!is.na(type)) |> 
            gt() |>
            cols_label(
                eid_name = "Employee", 
                type = "Type",
                reason = "Call-off or\nTardy Reason",
                notes = "Notes",
                date = "Date",
                grace = "Grace given?",
                manager_name = "Manager"
            )

    })
    
    output$existing_rows <- render_gt({
        
        pins::pin_read(
            salt_board,
            'vanug6_117748@cchmc.org/food_service_attendance_tracking_dataset') |> 
            dplyr::filter(!is.na(type)) |> 
            gt() |>
            cols_label(
                eid_name = "Employee", 
                type = "Type",
                reason = "Call-off or\nTardy Reason",
                notes = "Notes",
                date = "Date",
                grace = "Grace given?",
                manager_name = "Manager"
            )
    })
    
    output$point_counter <- render_gt({

        pins::pin_read(
            salt_board,
            'vanug6_117748@cchmc.org/food_service_attendance_tracking_dataset') |>
            dplyr::filter(!is.na(type)) |>
            dplyr::mutate(point_day = ifelse(grace == FALSE, 
                                             ifelse(type == "tardy", .25, 1),
                                             0)) |> 
            dplyr::mutate(total_points = sum(point_day),
                          tot_num_occurrences = dplyr::n(), .by = eid_name) |> 
            dplyr::select(eid_name, total_points, tot_num_occurrences) |>
            dplyr::slice_tail(n = 1, by = eid_name) |>
            gt() |> 
            data_color(
                columns = total_points,
                palette = "YlOrRd",
                domain = 0:15,
                method = "bin",
                bins = c(0,6,8,15)
            ) |>
            cols_label(
                eid_name = "Employee",
                total_points = "Points",
                tot_num_occurrences = "Total Number of\nCall-offs and Tardies",
            )


    })
    
    observeEvent(input$save, {
        
        pins::pin_write(
            salt_board,
            d_track() |> 
                dplyr::mutate(date = as.Date(date, format = "%m-%d-%Y")),
            "food_service_attendance_tracking_dataset", 
            type = "csv"
        )
        
        output$existing_rows <- render_gt({

            pins::pin_read(
                salt_board,
                'vanug6_117748@cchmc.org/food_service_attendance_tracking_dataset') |>
                dplyr::filter(!is.na(type)) |>
                gt() |>
                cols_label(
                    eid_name = "Employee",
                    type = "Type",
                    reason = "Call-off or\nTardy Reason",
                    notes = "Notes",
                    date = "Date",
                    grace = "Grace given?",
                    manager_name = "Manager"
                )
        })
        
        output$point_counter <- render_gt({
            
            pins::pin_read(
                salt_board,
                'vanug6_117748@cchmc.org/food_service_attendance_tracking_dataset') |>
                dplyr::filter(!is.na(type)) |>
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
        
        
        # sheet_write(
        #     d() |>
        #         tidyr::unite("name", c(first_name, last_name), remove = TRUE, sep = " ")  |>
        #         dplyr::mutate(point_day = ifelse(grace == FALSE, 
        #                                          ifelse(type == "tardy", .25, 1),
        #                                          0)) |> 
        #         dplyr::mutate(total_points = sum(point_day),
        #                       tot_num_occurrences = dplyr::n(), .by = id) |> 
        #         dplyr::select(id, name, total_points, tot_num_occurrences) |>
        #         dplyr::slice_tail(n = 1, by = id),
        #     ss,
        #     "tracker"
        # )
        
        showModal(modalDialog(
            title = "Success!",
            tags$p("Data successfully saved to drive ", tags$a(href = "https://salt.cchmc.org/content/6dba6a34-a9c7-4ac1-aa86-1e51f36f166b",
                                                               "here",
                                                               target = "_blank"))
        ))
    })

    
}

shinyApp(ui = ui, server = server)
