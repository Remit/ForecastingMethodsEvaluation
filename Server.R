#devtools::install_github('thomasp85/fiery')
#install.packages("routr")
library(fiery)
library(routr)

# Create a New App
app <- Fire$new(port = 10245L)

# Load the libraries it starts
app$on('start', function(server, ...) {
  suppressMessages(library(imputeTS))
  suppressMessages(library(ggplot2))
  suppressMessages(library(tseries))
  suppressMessages(library(forecast))
})

router <- RouteStack$new()
route <- Route$new()
router$add_route(route, 'main')

# We start with a catch-all route that provides a welcoming html page
route$add_handler('get', '*', function(request, response, keys, ...) {
  response$type <- 'html'
  response$status <- 200L
  response$body <- '<h1>All your AI are belong to us</h1>'
  TRUE
})

# Then on to the /info route
route$add_handler('get', '/info', function(request, response, keys, ...) {
  response$status <- 200L
  response$body <- structure(R.Version(), class = 'list')
  response$format(json = reqres::format_json())
  TRUE
})

# Lastly we add the /predict route
route$add_handler('get', '/predict', function(request, response, keys, arg_list, ...) {
  response$body <- predict(
    arg_list$model, 
    data.frame(x=as.numeric(request$query$val)),
    se.fit = TRUE
  )
  response$status <- 200L
  response$format(json = reqres::format_json())
  TRUE
})

# And just to show off reqres file handling, we'll add a route 
# for getting a model plot
route$add_handler('get', '/plot', function(request, response, keys, arg_list, ...) {
  f_path <- tempfile(fileext = '.png')
  png(f_path)
  plot(arg_list$model)
  dev.off()
  response$status <- 200L
  response$attach(f_path, filename = 'model_plot.png')
  TRUE
})

# Finally we attach the router to the fiery server
app$attach(router)
app$ignite()