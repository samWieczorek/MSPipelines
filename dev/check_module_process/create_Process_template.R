library(shiny)

options(shiny.fullstacktrace = TRUE)
options(shiny.trace=FALSE)

source("./process_template_funcs.R")
source("./mod_AddStep.R")




# download button


ui <- fluidPage(
  tagList(
    textInput('pipeline_name','Pipeline name'),
  textInput('process_name','process name'),
  selectInput("members", "Number of steps", choices = 1:5, width='100px'),
  uiOutput('show_step_forms'),

  actionButton('validate', 'Generate process skeleton')
  )
)



server <- function(input, output, session){

  rv <- reactiveValues(
    step_form = list(),
    step_value = NULL
  )

  output$show_step_forms <- renderUI({
    members <- as.integer(input$members) # default 2
    req(input$members > 0)
    rv$step_value <- lapply(1:as.numeric(input$members), function(i) {
      mod_AddStep_server(paste0('step_', i))
    })
    lapply(1:members, function(i) {
      mod_AddStep_ui(paste0('step_', i))
    })
  })


  observeEvent(input$validate, {
    call_createModule()
    })

  call_createModule <- function(){
   # browser()

    pipeline <- paste0('mod_', input$pipeline_name)
    process <- input$process_name
    #----------------------------------------------------------------#


    file <- paste0("./test/temp_mod_pipe_", process, ".R")

    cat(NULL, file=file)
    append=TRUE



    #----------------------------------------------------------------#

    ll.steps <- lapply(1:length(rv$step_value), function(x){rv$step_value[[x]]$name()})
    vec.steps <- unlist(ll.steps) #separe chaque etape
    steps <- paste0('\'',vec.steps,'\'') #chaque mot encadre
    steps <- paste0(steps,collapse = ",") #chaine char sep par ,
    steps <- paste0('c(', steps, ')') #chaine char like vector

    #----------------------------------------------------------------#

    ll.isMandatory <-  lapply(1:length(rv$step_value), function(x){rv$step_value[[x]]$isMandatory()})
    mandatory <- unlist(ll.isMandatory) #separe chaque etape
    mandatory <- paste0(mandatory,collapse = ",") #chaine char sep par ,
    mandatory <- paste0('c(',mandatory,')') #chaine char like vector

    #----------------------------------------------------------------#



    write_ui(name=process,
             file=file,
             append = append)

    write_start_server(name=process,
                       file=file,
                       append = append)

    all.widgets.name <- unlist(lapply(1:length(rv$step_value), function(x){rv$step_value[[x]]$widgets.name()}))
    all.widgets.defaultVal <- unlist(lapply(1:length(rv$step_value), function(x){rv$step_value[[x]]$widgets.defaultVal()}))
    write_widgets_default_values(name = process,
                                 widgets.name = all.widgets.name,
                                 widgets.defaultVal = all.widgets.defaultVal,
                                 file = file,
                                 append = append)

    write_moduleServerFunc(file = file, append = append)


    write_config(name = process,
                 steps = steps,
                 mandatory = mandatory,
                 file = file,
                 append = append)


    write_rv_widgets_def(widgets.name = all.widgets.name,
                         file = file,
                         append = append)


    write_initModule_func(file = file, append = append)


    write_output_description_template(file = file, append = append)

    for (i in 1:length(vec.steps)){
      step.widgets.name <- unlist(rv$step_value[[i]]$widgets.name())
      step.widgets.defaultVal <- unlist(rv$step_value[[i]]$widgets.defaultVal())
      step.widgets.renderUI <- unlist(rv$step_value[[i]]$widgets.renderUI())
      step.widgets.type <- unlist(rv$step_value[[i]]$widgets.type())
      browser()

      write_header(step.name = vec.steps[i], file = file, append = append)
      write_observer_for_widgets(step.widgets.name, file, append)

      ind.renderUI.widgets <- which(isTRUE(step.widgets.renderUI))
      if (length(ind.renderUI.widgets) > 0)
        write_code_for_renderUI_widgets(widgets.renderUI.names = step.widgets.name[ind.renderUI.widgets],
                                        step.name = vec.steps[i],
                                        file = file,
                                        append)

      write_header_for_global_step_renderUI(step.name = vec.steps[i],
                                            file = file,
                                            append)

      if (length(ind.renderUI.widgets) > 0)
        write_code_for_call_renderUI_widgets(widgets.renderUI.names = step.widgets.name[ind.renderUI.widgets],
                                             file = file,
                                             append)

      ind.direct.widgets <- which(!isTRUE(step.widgets.renderUI))
      if (length(ind.direct.widgets) > 0){
        if (length(ind.renderUI.widgets) > 0)
          write_additional_comma(file, append)
        write_code_for_call_direct_widgets(widgets.direct.names = step.widgets.name[ind.direct.widgets],
                                           step.name =  vec.steps[i],
                                           file = file,
                                             append)

        write_code_for_validation_btn(step.name = vec.steps[i],
                                      file = file,
                                      append)
        write_code_to_end_global_step_renderUI(file = file,
                                               append)

      }
    }

   # create_rNav(process=process, steps = steps, file=file, append = append)

   # create_rvModule(process=process, widgets_list=widgets_list, file=file, append = append)

   # create_reset(widgets_list=widgets_list,nb_screen=length(unlist(strsplit(steps,","))),file=file, append = append)

    #create_screen(process=process,file=file, append = append)

   # create_widgets_from_input(widgets_list=widgets_list,file=file, append = append)

   # create_end_server(name=name,file=file, append = append)

  }

}


shinyApp(ui, server)
