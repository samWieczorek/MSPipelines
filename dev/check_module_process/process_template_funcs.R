
source("./module_chunks_in_variables.R") #names(.GlobalEnv)


write_ui <- function(name, file, append = FALSE) {
  new_ui <- gsub('#name#', name, ui)
  cat(new_ui, file=file, append = append)
}


write_start_server <- function(name, file, append = FALSE) {
  new_start_server <- gsub('#name#' ,name , start_server)
  cat(new_start_server, file=file, append = append)
}


write_widgets_default_values <- function(name, widgets.names, widgets.defaultVal, file, append = FALSE){
  cat("# Define default selected values for widgets\n", file=file, append = append)
  cat("widgets.default.values <- list(\n", file=file, append = append)
  for (i in 1:length(widgets.names)){
    cat(paste0(widgets.names[i], " = ", widgets.defaultVal[i]), file=file, append = append)
    if (i < length(widgets.names))
      cat(",", file=file, append = append)
    cat("\n", file=file, append = append)
  }

  cat(")\n\n", file=file, append = append)
}


write_moduleServerFunc <- function(file, append = FALSE){
  cat(moduleServerFunc_template, file=file, append = append)
}

write_config <- function(name, steps, mandatory, file, append = FALSE) {
  new_config <- gsub('#name#' ,name , config_template)
  new_config <- gsub('#steps#' ,steps , new_config)
  new_config <- gsub('#mandatory#' ,mandatory , new_config)
  cat(new_config, file=file, append = append)
}


write_rv_widgets_def <- function(widgets.names, file, append = FALSE){
  cat("rv.widgets <- reactiveValues(\n", file=file, append = append)

  for (i in 1:length(widgets.names)){
    cat(paste0(widgets.names[i], " =  widgets.default.values$", widgets.names[i]), file=file, append = append)
    if (i < length(widgets.names))
      cat(",", file=file, append = append)
    cat("\n", file=file, append = append)
  }

  cat(")\n\n", file=file, append = append)
}



write_initModule_func <- function(file, append = FALSE){
  cat(init_module_template, file=file, append = append)

}

write_output_description_template <- function(file, append = FALSE){
  cat(output_Description_template, file=file, append = append)

}


write_header <- function(step.name, file, append = FALSE){
  cat(paste0("# Code for step ", step.name),file=file, append = append)
}


write_observer_for_widgets <- function(widgets.names, file, append = FALSE){
  for (i in 1:length(widgets.names))
    cat(gsub('#name#' , widgets.names[i] , observer_for_widget_template),
        file=file,
        append = append)
}


write_code_for_renderUI_widgets <- function(widgets.renderUI.names, step.name, file, append = FALSE){
  txt <- gsub('#step#' , step.name , widget.renderUI.template)

  for (i in widgets.renderUI.names)
    cat(gsub('#name#' , i , txt), file = file, append = append)
}



write_header_for_global_step_renderUI <- function(step.name, file, append=FALSE){
  txt <- gsub('#step.name#' , step.name , code_for_global_step_renderUI_header)
  cat(txt, file, append)
}

write_code_for_call_renderUI_widgets <- function(widgets.renderUI.names, file, append = FALSE){

  for (i in 1:length(widgets.renderUI.names)){
    cat(gsub('#widget.name#' , widgets.renderUI.names[i] , code_for_global_step_renderUI_call_widget_renderUI),
        file = file,
        append = append)
    if (i < length(widgets.renderUI.names))
      cat(',', file = file, append = append)

  }
}

write_additional_comma <- function(file, append = FALSE){
  cat(',\n', file = file, append = append)
}

write_code_for_call_direct_widgets <- function(widgets.direct.names, step.name, file, append = FALSE){
  txt <- gsub('#step#' , step.name , code_for_global_step_renderUI_call_widget)
  for (i in 1:length(widgets.direct.names)){
    cat(gsub('#widget.name#', widgets.direct.names[i], txt),
        file = file,
        append = append)
    if (i < length(widgets.direct.names))
      cat(',', file = file, append = append)

  }
}

write_code_for_validation_btn <- function(step.name, file, append=FALSE){
  txt <- gsub('#step.name#' , step.name , code_for_global_step_renderUI_call_validate_btn)
  cat(txt, file, append)
}


write_code_to_end_global_step_renderUI <- function(file, append = FALSE){

  cat(code_for_global_step_renderUI_end, file, append)
}





create_rNav <- function(process, steps, file, append = FALSE) {


  nb_screen <- length(unlist(strsplit(steps,',')))
  new_rNav <- gsub('nb_screen',nb_screen,rNav) # warning special case like isDone =  c(FALSE,TRUE,FALSE)

  ll.UI <- c()
  for (i in 1:nb_screen){
    ll.UI[i] <- paste0("screenStep",i," = uiOutput(ns('Screen_process_",i,'\'))')
  }
  ll.UI <- paste0(ll.UI,collapse = ",")
  new_rNav <- gsub('ll.UI_list',as.character(c(ll.UI)),new_rNav)

  new_rNav <- gsub('process',process,new_rNav)
  new_rNav <- gsub('\\<steps\\>',steps,new_rNav)



  cat(new_rNav, file=file, append = append)

}

create_rvModule <- function(process, widgets_list, settings=TRUE, file, append = FALSE) {
  new_rvModule <- gsub('process',paste0('process_',process),rvModule)
  new_rvModule <- gsub('widgets_list',widgets_list,new_rvModule)
  if (!settings){ new_rvModule <- gsub('settings = NULL,','',new_rvModule) }
  cat(new_rvModule, file=file, append = append)
}


create_reset <- function(widgets_list, nb_screen, file, append = FALSE) {
  new_reset <- gsub('widgets_list',widgets_list,reset)
  new_reset <- gsub('nb_screen',nb_screen,new_reset)
  cat(new_reset, file=file, append = append)
}

# create_modalAlert <- function(file, append = FALSE){
#   cat(modalAlert, file=file, append = append)
# }
# create_shinyAlert <- function(file, append = FALSE){
#   cat(shinyAlert, file=file, append = append)
# }


create_end_server <- function(name, file, append = FALSE) {
  new_end_server <- gsub('name',name,end_server)
  cat(new_end_server, file=file, append = append)
}


create_screen <- function(process, file, append=FALSE){

  screen_list <- c()
  nb_screen <- length(unlist(strsplit(screen_content_ui,',')))
  print(nb_screen)
  for (i in 1:nb_screen){
    screen_list[i] <- paste0('\n#Screen',i,
                             '\noutput$Screen_process_',i,' <- renderUI({\n\n',screen_content_ui[[i]],'\n\n})\n\n',
                             screen_content_server[[i]])
  }
  screen_list <- paste0(screen_list,collapse = "\n")
  new_screen <- paste0('\n\n## Definitions of the screens\n\n', screen_list)
  new_screen <- gsub('process',process,new_screen)
  cat(new_screen, file=file, append = append)
}


create_widgets_from_input <- function(widgets_list, file, append=FALSE){

  widgets_list <- unlist(strsplit(widgets_list,","))
  widgets_list <- sub("[[:space:]]*=.*", "", widgets_list)
  widgets_list <- gsub(' ','',widgets_list)

  widgets_from_input <- c()
  for (i in 1:length(widgets_list)){
    widgets_from_input[i] <- paste0('\n\nobserveEvent(input$',
                                    widgets_list[i],
                                    ', ignoreInit=TRUE,{\nrv.filter$widgets$',
                                    widgets_list[i],' <- input$',
                                    widgets_list[i],'\n})\n')
  }

  cat(widgets_from_input, file=file, append = append)
}


#-----------------------------------------------------------#
create_watch_file <- function(name, process){
  new_watch_file <- gsub('name',name,watch_file)
  new_watch_file <- gsub('process',process,new_watch_file)
  file=paste0("watch_pipe_",name,".R")
  cat(new_watch_file, file=file)
}

# create_watch_file(name='protein_Filtering',
#                   process='Filtering')
# #-----------------------------------------------------------#
#
#
# create_ui(name='protein_Filtering',
#           file="new_ui.R")
#
# create_start_server(name='protein_Filtering',
#                     file="new_start_server.R")
#
# create_rNav(process='Filtering',
#             steps = 'c(\'screen1\', \'table2\', \'plop\', \'toto\', \'titi\')',
#             file="new_rNav.R")
#
# create_rvModule(process='Filtering',
#                 widgets_list='ChooseFilters = "None",seuilNA = 0, seuil_plop=50',
#                 file="new_rvModule.R")
#
# create_reset(widgets_list='ChooseFilters = "None",seuilNA = 0, seuil_plop=50',
#              nb_screen=5,
#              file="new_reset.R")
#
# # create_modalAlert(file="modalAlert.R")
# # create_shinyAlert(file="shinyAlert.R")
#
# create_end_server(name='protein_Filtering',
#                   file="new_end_server.R")
#
# create_screen(process='plop',
#               file="new_screen.R")
#
# create_widgets_from_input(widgets_list=' ChooseFilters= "None",  seuilNA   =   10',
#                           file="new_widgets_from_input.R")


#-----------------------------------------------------------#
#-----------------------------------------------------------#
#source("./copy_paste_test_wParam.R")

createModule <- function(name, process, steps, widgets_list, append = TRUE){

  file <- paste0("mod_pipe_",name,".R")

  cat(NULL, file=file)

  create_ui(name=name,file=file, append = append)

  create_start_server(name=name,file=file, append = append)

  create_rNav(process=process,steps = steps,file=file, append = append)

  create_rvModule(process=process,widgets_list=widgets_list,file=file, append = append)

  create_reset(widgets_list=widgets_list,nb_screen=length(unlist(strsplit(steps,","))),file=file, append = append)

  # create_modalAlert(file=file, append = append)

  # create_shinyAlert(file=file, append = append)

  create_screen(nb_screen=length(unlist(strsplit(steps,","))),process=process,file=file, append = append)

  create_widgets_from_input(widgets_list=widgets_list,file=file, append = append)

  create_end_server(name=name,file=file, append = append)


}

# createModule(name='protein_Filtering',
#              process='Filtering',
#              steps = 'c(\'screen1\', \'table2\', \'plop\', \'toto\', \'titi\')',
#              widgets_list='ChooseFilters = "None",seuilNA = 0, seuil_plop=50',
#              append=TRUE)



