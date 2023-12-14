##############################################################
# FINDING PATHS
##############################################################

is.abs.path = function(x) {
  startsWith(x, "/") | startsWith(x,"~") | grepl(":",x, fixed=TRUE)
}

# Find closest directory to dir starting from end
find.closest.path = function(dir, dirs) {
  sp = strsplit(dir, "/")[[1]]
  n = length(sp)
  for (i in 1:n) {
    di = do.call(file.path,as.list(sp[i:n]))
    ok= which(endsWith(dirs, di))
    if (any(ok))
      return(dirs[ok[1]])
  }
  return(NULL)
}


repbox.write.log.find.path = function(callid, mode, file, org.file,sup.dir, type) {
  restore.point("repbox.write.log.find.path")
  cat("\nFile path ", org.file, " -> ", file, "\n")
  #return(invisible())
  opts = repbox.funs.opts()
  project.dir = opts$project.dir
  sup.dir = opts$sup.dir
  log.file = file.path(project.dir,"repbox","r","find_path_log.csv")

  if(!file.exists(log.file)) return()
  log.txt = paste0(c(callid, mode, file, org.file, sup.dir,type), collapse=",")
  write(log.txt, log.file, append=TRUE)
}


repbox.path = function(file, mode, callid) {
  restore.point("repbox.path")
  opts = repbox.funs.opts()
  wdir = getwd()
  sup.dir = opts$sup.dir
  project.dir = opts$project.dir

  org.file = file
  # normalize to linux convention
  file = gsub("\\","/", file, fixed=TRUE)

  # starts with main directory
  # possibly because some Stata path
  # prefix variable was empty
  # Then add sup.dir to file
  if (isTRUE(substring(file,1,1)=="/")) {
    file = paste0(sup.dir, file)
  }

  if (mode=="cd") {
    dir = file
    if (dir.exists(dir)) {
      repbox.write.log.find.path(callid, mode,dir, org.file,  sup.dir,"direct_exists")
      return(dir)
    }
    dirs = list.dirs(sup.dir,recursive = TRUE, full.names = TRUE)
    mdir = find.closest.path(dir, dirs)
    if (!is.null(mdir)) {
      repbox.write.log.find.path(callid, mode, mdir, org.file,  sup.dir, "dir_found")
      return(mdir)
    }
    if (is.abs.path(dir)) {
      repbox.write.log.find.path(callid, mode, mdir, org.file,  sup.dir, "nodir_abs")
      return(sup.dir)
    } else {
      repbox.write.log.find.path(callid, mode, mdir, org.file,  sup.dir,"nodir_rel")
      return(sup.dir)
    }
  }


  if (isTRUE(try(file.exists(file)))) {
    repbox.write.log.find.path(callid, mode, file, org.file,  sup.dir,"direct_exists")
    return(file)
  }


  # In a save command we just want to match the directory
  # the file itself may not exist
  if (mode=="s") {
    dirs = list.dirs(sup.dir,recursive = TRUE, full.names = TRUE)
    dir = dirname(file)
    if (isTRUE(try(dir.exists(dir)))) {
      return(file,"save_dir_exists")
    }
    mdir = find.closest.path(dir, dirs)
    if (is.null(mdir)) {
      # If no match is found for absolute path
      # return sup.dir
      if (is.abs.path(dir)) {
        file = file.path(sup.dir,basename(file))
        repbox.write.log.find.path(callid, mode, file, org.file,  sup.dir, "save_nodir_abs")
        return(file)
      }
      # For a relative path just return the original file
      try(dir.create(dirname(file)))
      repbox.write.log.find.path(callid, mode, file, org.file,  sup.dir,"save_nodir_rel")
      return(file)
    }
    file = file.path(mdir, basename(file))
    repbox.write.log.find.path(callid, mode, file, org.file,  wdir,"save_dir_found")
    return(file)
  }

  files = list.files(sup.dir,full.names = TRUE,recursive = TRUE, include.dirs = TRUE)
  mfile = find.closest.path(file,files)
  if (is.null(mfile)) {
    repbox.write.log.find.path(callid, mode, file, org.file,  sup.dir, "not_found")
    return(file)
  }
  # Return absolute path
  repbox.write.log.find.path(callid, mode, mfile, org.file,  sup.dir, "found")
  return(mfile)
}
