
repbox.save.call.info = function(type,callid, subdir=type) {
  opts = repbox.funs.opts()
  counter = repbox.output.counter()
  project.dir = opts$project.dir
  sup.dir = opts$sup.dir
  scalls = sys.calls()

  restore.point("repbox.save.call.info")

  project = basename(project.dir)

  branch = normalizePath(sup.dir, mustWork=FALSE)
  if (!endsWith(branch,"/")) {
    branch = paste0(branch,"/")
  }

  rfile = repbox.rfile()

  inds = seq_along(scalls)
  inds = setdiff(inds, c(seq_len(opts$middleman.call.stack.skip), max(inds)))
  scalls = scalls[inds]
  stack.df = sys.calls.to.df(scalls)

  if (type == "reg") {
    inner.call = scalls[length(scalls)][[1]][[2]]
    inner.call = deparse1(inner.call)
    fun = str.left.of(inner.call,"(")

  } else {
    inner.call = ""
    fun = ""
  }

  res = list(info_df = data.frame(project=project,callid=callid, type=type, call=inner.call, fun=fun, rfile=rfile, counter=counter) , stack_li = stack.df)
  saveRDS(res, file.path(project.dir,paste0("repbox/r/", subdir,"/","call_", counter,".Rds")))
  return(invisible(res))
}


sys.calls.to.df = function(scalls) {
  restore.point("sys.calls.to.df")
  sfuns = sapply(scalls, function(call) as.character(call[[1]]))
  srcrefs = lapply(scalls, function(call) attr(call, "srcref"))
  srclines = sapply(srcrefs, function(x) x[[1]] )
  srcfiles = sapply(srcrefs, function(x) attr(x, "srcfile")$filename )
  list(call=as.list(scalls), fun=sfuns, srcfile=srcfiles, srcline = srclines)
}

try_with_stack = function (quoted_code, env) {
  capture_calls <- function(e) {
    e["call"] <- e["call"]
    e$calls <- head(sys.calls()[-seq_len(frame + 7)], -2)
    signalCondition(e)
  }
  frame <- sys.nframe()
  tryCatch(withCallingHandlers(eval(quoted_code, env), error = capture_calls),
           error = identity)
}

