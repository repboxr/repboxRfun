repbox.source = function(file,...) {
  restore.point("repbox.source")
  opts = repbox.funs.opts()
  if (opts$eval_mode == "evaluate") {
    script_num = repbox_rfile_to_script_num(file)
    if (is.na(script_num)) {
      stop(paste0("Could not find ", file, " under R scripts. No file is sourced."))
    }
    env = getOption(".repbox.env")
    repbox_evaluate_script_chunks(opts$project_dir,script_num = script_num,env = env)
  } else if (opts$eval_mode == "spin") {
    repbox.stitch.source(file)
  } else if (opts$eval_mode == "source") {
    source(file, ...)
  } else {
    stop(paste0("Unknown opts$eval_mode = ", opts$eval_mode))
  }
  return(invisible())
}

repbox_rfile_to_script_num = function(file) {
  restore.point("repbox_rfile_to_script_num")
  opts = repbox.funs.opts()
  script_df = opts$script_df
  bases = basename(script_df$file_path)
  ind = match(basename(file), bases)
  if (is.na(ind)) return(NA_integer_)
  script_df$script_num[ind]
}

repbox.stitch.source = function(file) {
  restore.point("repbox.stitch.source")
  oldwd = getwd()
  if (!require(knitr)) {
    source(file)
    return(invisible(FALSE))
  }
  cat("\nWe stitch ", file,"...\n")
  opts = repbox.funs.opts()
  env = repbox.get.env()
  project_dir = opts$project_dir

  # Currently only knitr::spin works recursively (not knitr::stitch)
  # but for spin, we cannot specify the output file path
  # we thus copy the R file into our log directory
  log.dir = file.path(project_dir,"repbox/r/log")

  log_files = list.files(log.dir, glob2rx("*.r"),ignore.case = TRUE)
  counter = length(log_files)+1

  base = tools::file_path_sans_ext(basename(file))
  new.file = paste0(log.dir,"/", counter,"_", base, ".r")
  file.copy(from=file, to=new.file)
  setwd(log.dir)
  if (opts$show.spin.log) {
    knitr::spin(new.file, envir = env)
  } else {
    res = capture.output(knitr::spin(new.file, envir = env))
  }
  if (!is.null(oldwd)) {
    setwd(oldwd)
  } else {
    setwd(project_dir)
  }
  invisible(TRUE)
}
