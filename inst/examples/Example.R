
Example = R6Class(
  "Example",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'Example',
                   steps = c('Description', 'ProcessA', 'ProcessB', 'ProcessC'),
                   mandatory = c(T, F, T, F)
    )
  ),
  
  public = list(
    Global_server = function(input, output){}
  )
)