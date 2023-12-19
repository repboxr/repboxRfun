##############################################################
# Regression Infos
##############################################################

repbox.reg = function(reg,callid,...) {
  restore.point("repbox.reg")

  if (!suppressMessages(require(broom,quietly = TRUE)))
    return(reg)

  opts = repbox.funs.opts()
  project_dir = opts$project_dir

  if (isTRUE(opts$verbose.middleman)) {
    cat("\nCalled repbox.reg")
  }
  counter = repbox.next.output.counter()

  tidy.reg = broom::tidy(reg,conf.int=TRUE)
  base::saveRDS(tidy.reg,file.path(project_dir, paste0("repbox/r/reg/tidy_",counter,".Rds")))

  glance.reg = broom::glance(reg)
  base::saveRDS(glance.reg,file.path(project_dir, paste0("repbox/r/reg/glance_",counter,".Rds")))

  # Save general middleman call info
  repbox.save.call.info("reg",callid)

  return(reg)
}
