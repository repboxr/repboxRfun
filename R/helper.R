repbox.get.env = function() {
  env = getOption(".repbox.env")
  if (is.null(env)) {
    env = new.env(parent=globalenv())
    options(.repbox.env = env)
  }
  env
}

repbox.funs.opts = function() {
  getOption(".repbox.options")
}

repbox.rfile = function() {
  getOption(".repbox.rfile")
}

repbox.output.counter = function() {
  counter = getOption("repbox.output.counter")
  if (length(counter)==0) {
    counter = 1
  }
  counter
}

repbox.next.output.counter = function(counter = getOption("repbox.output.counter")+1) {
  if (length(counter)==0) {
    counter = 1
  }
  options(repbox.output.counter=counter)
  counter
}
