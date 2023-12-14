repbox_evaluate_data_frame = function(x, df_opts=repbox_evaluate_data_frame_options()) {
  restore.point("repbox_evaluate_data_frame")
  if (is.matrix(x))
    x = as.data.frame(x)

  table.max.rows=df_opts$table.max.rows
  table.max.cols=df_opts$table.max.cols
  data.frame.theme=df_opts$data.frame.theme

  adapt.data = FALSE
  missing.txt = NULL
  if (!is.null(table.max.rows)) {
    if (NROW(x)>table.max.rows) {
      adapt.data = TRUE
      missing.txt = paste0("... only ", table.max.rows, " of ", NROW(x), " rows")
    }
  }
  if (!is.null(table.max.cols)) {
    if (NCOL(x)>table.max.cols) {
      adapt.data = TRUE
      if (is.null(missing.txt)) {
        missing.txt = paste0("... only ", table.max.cols, " of ", NROW(x), " columns")
      }
      missing.txt = paste0(missing.txt, " and ", table.max.cols, " of ", NCOL(x), " columns")
    }
  }
  if (adapt.data) {
    missing.txt = paste0(missing.txt, " shown ...")
    x = crop_df_to_max_size(x,table.max.rows, table.max.cols)
  }
  x = format.df.cols(x,df_opts=df_opts)

  if (data.frame.theme=="html") {
    html = simple_df_html_table(x)
    if (adapt.data) {
      html = paste0(html, "<p>",missing.txt,"</p>")
    }
    return(html)
  } else {
    txt = capture.output(print(x))
    txt = c(paste0(txt,collapse="\n"),missing.txt)
    return(txt)
  }
}

simple_df_html_table = function(df, class="data-frame-tab", id=NULL) {
  title_df = paste0('<th>',names(df),'</th>', collapse="")
  cols = 1:NCOL(df)
  code = paste0('"<td>",df[[',cols,']],"</td>"', collapse=",")
  code = paste0('paste0("<tr>",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main_html = eval(parse(text=code))
  tab_html = paste0(
'<table',
  if (!is.null(id)) paste0(' id="',id,'"'),
  if (!is.null(class)) paste0(' class="',class,'"')
, '>
<thead>',title_df,'</thead>
<tbody>\n', main_html,'
</tbody></table>'
  )
  tab_html
}


repbox_evaluate_data_frame_options = function(table.max.rows=25, table.max.cols=NULL, round.digits=8, signif.digits=8, max_nchar=100, html_escape = data.frame.theme != "code", data.frame.theme=c("html","code")[1]) {
  list(
    table.max.rows=table.max.rows,
    table.max.cols=table.max.cols,
    round.digits=round.digits,
    signif.digits=signif.digits,
    max_nchar=max_nchar,
    data.frame.theme=data.frame.theme
  )
}


crop_df_to_max_size = function(x, max.rows=NULL, max.cols=NULL) {
  if (!is.null(max.rows)) {
    if (NROW(x)>max.rows) {
      x = x[1:max.rows,]
    }
  }
  if (!is.null(max.cols)) {
    if (NCOL(x)>max.cols) {
      x = x[,1:max.cols]
    }
  }
  x
}

format.df.cols = function(x,df_opts) {
  #restore.point("format.df.cols")
  as.data.frame(lapply(x,format.vals, df_opts=df_opts))
}

format.vals = function(vals, signif.digits=df_opts$signif.digits, round.digits=df_opts$round.digits, max_nchar=df_opts$max_nchar, html_escape=isTRUE(df_opts$html_escape), df_opts=NULL) {
  if (is.numeric(vals)) {
    if (is.null(signif.digits) & is.null(round.digits)) {
      return(vals)
    } else if (!is.null(signif.digits) & is.null(round.digits)) {
      return(signif(vals, signif.digits))
    } else if (is.null(signif.digits) & !is.null(round.digits)) {
      return(round(vals, signif.digits))
    } else {
      return(signif(round(vals, round.digits), signif.digits))
    }
  } else if (is.character(vals)) {
    if (is.finite(max_nchar)) {
      vals = shorten.str(vals, max_nchar)
    }
    if (html_escape) {
      vals = htmlEscape(vals)
    }
  } else if (is.list(vals)) {
    vals = rep("list(...)", length(vals))
  }
  vals
}





#' Escape HTML entities
#'
#' Directly copied from htmltools package
#'
#' Escape HTML entities contained in a character vector so that it can be safely
#' included as text or an attribute value within an HTML document
#'
#' @param text Text to escape
#' @param attribute Escape for use as an attribute value
#'
#' @return Character vector with escaped text.
#'
#' @export
htmlEscape <- local({

  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse='|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(names(.htmlSpecialsAttrib), collapse='|')

  function(text, attribute=FALSE) {
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib
    else
      .htmlSpecialsPattern

    text <- enc2utf8(as.character(text))
    # Short circuit in the common case that there's nothing to escape
    if (!any(grepl(pattern, text, useBytes = TRUE)))
      return(text)

    specials <- if(attribute)
      .htmlSpecialsAttrib
    else
      .htmlSpecials

    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed = TRUE, useBytes = TRUE)
    }
    Encoding(text) <- "UTF-8"

    return(text)
  }
})

