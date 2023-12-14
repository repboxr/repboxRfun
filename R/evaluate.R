repbox_evaluate_script_chunks = function(project.dir, script_num, env=new.env(parent=globalenv()), chunk_df = NULL) {
  restore.point("repbox_evaluate_script_chunks")

  opts = getOption(".repbox.options")
  if (script_num == 2) {
    restore.point("repbox_evaluate_script_chunks2")
  }

  if (is.null(chunk_df)) {
    chunk.file = paste0(project.dir, "/repbox/r/chunks/chunks_", script_num, ".Rds")
    chunk_df=readRDS(chunk.file)
  }

  store.class = function(x,my_class, visible=TRUE, obj_file=NULL) {
    my_class = my_class[length(my_class)]
    attr(x,"org_class") = my_class
    attr(x,"is_visible") = visible
    if (!is.null(obj_file))
      attr(x,"obj_file") = obj_file
    x
  }

  # Generate figures
  figure.dir = file.path(project.dir,"repbox","r","figure")
  if (!dir.exists(figure.dir)) dir.create(figure.dir)

  out.dir = paste0(project.dir, "/repbox/r/out_objects")
  if (!dir.exists(out.dir)) dir.create(out.dir)
  out_counter = 0

  handler = repboxEvaluate::new_output_handler(
    value = function(x, visible, row_ind) {
      restore.point("myhandler")

      save_class = get.save.object.class(x, visible)
      if (!is.na(save_class) & opts$save_plot_rds) {
        restore.point("kijsfksdfkj")
        out_counter <<- out_counter+1
        obj_file = paste0(script_num, "_", out_counter,"__",save_class, ".Rds")
        if (opts$save_plot_rds) {
          saveRDS(x, file.path(out.dir,obj_file))
        }
        org_class = save_class
      } else {
        obj_file = ""
        org_class = class(x)
      }
      if (isTRUE(save_class == "ggplot") & !visible) {
        plot(x)
        return(store.class("",org_class, visible, obj_file))
      }

      is_reg = has.class(x, repbox.reg.classes())
      # Typically result is not visible if we assign a variable
      # then we just store a printout of the structure of the object
      if (!visible & !is_reg) {
        restore.point("lshkfhkskf")
        if (is.null(x))
          return(store.class("",org_class, visible, obj_file))

        res = paste0(capture.output(str(x)), collapse="\n")
        res = shorten.str(res,opts$invisible_max_char,"\n... further output omitted ...")
        return(store.class(res,org_class, visible, obj_file))
      }

      if (is.data.frame(x)) {
        restore.point("myhandler_data_frame")
        return(store.class(repbox_evaluate_data_frame(x),"data.frame", visible,obj_file))
      }
      res = paste0(capture.output(x), collapse="\n")
      res = shorten.str(res,opts$visible_max_char,"\n... further output omitted ...")
      store.class(res, org_class, visible, obj_file)
    }
  )

  #debug(evaluate_call)
  df = repbox_evaluate(chunk_df$mod_code, envir=env,output_handler = handler)


  i = 10
  out_li = lapply(seq_along(df$out), function(i) {
    restore.point("lkjshfkhdkfhk")
    li = df$out[[i]]
    chunkid = chunk_df$chunkid[[i]]
    if (length(li)==0) return(NULL)
    types = names(li)

    out_file =  rep("", length(li))
    out_class = rep("", length(li))
    visible = rep(TRUE, length(li))
    value_inds = which(types=="value")

    if (length(value_inds)>0) {
      out_class[value_inds] = sapply(value_inds, function(j) attr(li[[j]], "org_class"))
      visible[value_inds] = sapply(value_inds, function(j) attr(li[[j]], "is_visible"))
      out_file[value_inds] = sapply(value_inds, function(j) attr(li[[j]], "obj_file"))
    }


    # TO DO: Some plots will not be nicely stored
    plot_inds = which(types=="graphics")
    if (length(plot_inds)>0) {
      for (j in seq_along(plot_inds)) {
        plot_ind = plot_inds[j]
        out_counter <<- out_counter+1
        obj_file = paste0(script_num, "_", out_counter,"__plot.Rds")
        if (opts$save_plot_rds) {
          saveRDS(li[[plot_ind]], file.path(out.dir, obj_file))
        }
        out_file[plot_ind] = obj_file
        try({
          plot_file = paste0(script_num, "_", out_counter,"__plot.PNG")
          save_recorded_plot(li[[plot_ind]], file.path(figure.dir,plot_file))

        })
        li[[plot_ind]] = ""
      }
    }

    out_text = sapply(li, function(el) paste0(as.character(el), collapse="\n"))
    img_files = ifelse(out_file == "", "", paste0(str.left.of(out_file,".Rds"),".PNG"))

    data.frame(chunkid = chunk_df[i,"chunkid"], out_type=types, out_text = out_text, out_class=out_class, obj_file=out_file, is_visible = visible, is_html = out_class=="data.frame" & visible, img_file = img_files)
  })
  out_df = do.call(rbind, out_li)

  chunk_out_file = paste0(project.dir, "/repbox/r/chunks/out_", script_num, ".Rds")
  saveRDS(out_df, chunk_out_file)
  invisible(out_df)

}

has.class = function(x, classes) {
  any(class(x) %in% classes)
}

repbox.reg.classes = function() {
  c("lm","glm","fixest","ivreg")
}

get.save.object.class = function(x, visible) {
  #classes = c("ggplot")
  if (visible) return(NA)
  if (is(x,"ggplot")) return("ggplot")
  return(NA)
  #classes[match(classes, class(x))]
}

save_recorded_plot = function(x, file,  width = 480, height = 360) {
  grDevices::png(file,width = width, height=height)
  print(x)
  dev.off()
}


