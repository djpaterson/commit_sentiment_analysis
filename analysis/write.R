write.macro <- function(name, value, comment="", min.dp=2, round.dp=2, xspace=T){
  if("tbl_df" %in% class(value)){
    value <- value[[1]][1]
  }
  sink(macro.file, append=T, split=T)
  cmd.string = paste0("\\newcommand{\\",name,"}")
  if(is.numeric(value)){
    value=format(round(value, round.dp), nsmall=min.dp)
  }
  if (xspace){
    cmd.string=paste0(cmd.string, "{{",value,"}\\xspace} \n")
  } else {
    cmd.string=paste0(cmd.string, "{{",value,"}} \n")
  }
  if (nchar(comment) > 0){
    cat(paste0(comment, "\n"))
  }
  cat(cmd.string,sep="")
  sink()
}

write_table <- function(df, 
                        transpose=F, 
                        colnames.as.headers=T, 
                        bold.sig=F, 
                        sig.col=NULL, 
                        exclude.columns=c(), 
                        hlines=T, 
                        create.file=T, 
                        exclude.formatting=c(), 
                        max.dp=2, 
                        round.dp=2, 
                        headers=c(),
                        filename
                        ){
  if(bold.sig){
    if(is.null(sig.col)){
      if(!"p.value" %in% colnames(df) & !"p.value" %in% df[1,]){
        stop("P Value must be in columns of data frame in order to make significanct values bold")
      } else {
        sig.col="p.value"
      }
    }
  }
  if(create.file){
    table.dir=paste(Sys.getenv("THESIS_DIR"), "tables", sep="/")
    if(!dir.exists(table.dir)){
      dir.create(table.dir, recursive=T)
    }
    if(nchar(filename) > 0){
      table.filename <- paste0(table.dir,"/",filename)
      if(file.exists(table.filename)){
        file.remove(table.filename)
      }
    } else {
      tables = list.files(table.dir)
      num.tables = length(tables) + 1
      table.filename=paste0(table.dir, "/table", num.tables, ".tex")
    }
    
    print(paste("Writing to", table.filename))
    sink(table.filename, append=T, split=T)
  }
  num.cols = length(colnames(df)) - length(exclude.columns)
  if(length(exclude.columns) > 0){
    filter_select = Reduce(function(x,y){return(ifelse(x=="", paste0("-",y), paste0("-",x,", ",y)))}, exclude.columns, "")
    df %<>% select_(filter_select)
  }
  num.rows = nrow(df)
  colstring=Reduce(function(x,y){
    r=ifelse(nchar(x) > 0, x, "")
    if(nchar(y) > 0){
      r = paste0(r, ifelse(is.numeric(df[[y]]), "r", "l"))
    }
    return(r)
  }, colnames(df), "")
  cat("\\begin{tabular}{",colstring,"}\n")
  if(hlines){
    cat("\\toprule\n")
  }
  if(colnames.as.headers){
    row=colnames(df)
  } else {
    row=df[1,]
    df = df[-1,]
    num.rows = num.rows - 1
  }
  if(length(headers) > 0){
    if(length(headers) == num.cols){
      cat(get.row.text(headers))
    } else {
      cols.per.header = (num.cols - 1) / length(headers)
      coltext <- Reduce(f = function(x,y){
        if(!is.null(x)){
          col.header <- x
          if(x == headers[[1]]){
            col.header <- paste0("& \\multicolumn{",cols.per.header,"}{c}{",x,"} & ")
          }
          return(paste0(col.header,paste0("\\multicolumn{",cols.per.header,"}{c}{",y,"} & ")))
        }
      }, headers)
      coltext <- paste0(substr(coltext,1,nchar(coltext) - 2), "\\\\ \n")
      cat(coltext)
    }
  }
  rowtext=get.row.text(row, num.cols, 1, exclude.formatting)
  cat(rowtext)
  if(hlines){
    cat("\\midrule\n")
  }
  for(i in 1:num.rows){
    row=df[i,]
    if(is.null(sig.col)){
      sig=1
    } else {
      sig = row[,sig.col]
    }
    rowtext = get.row.text(row, num.cols, sig, exclude.formatting, max.dp=max.dp, round.dp = round.dp)
    cat(rowtext)
  }
  if(hlines){
    cat("\\bottomrule\n")
  }
  cat("\\end{tabular}")
  if(create.file){
    sink()
  }
}

get.row.text <- function(row, num.cols, sig, exclude.formatting, max.dp=2, round.dp=2){
  illegal.chars=c("#", "<", ">", "%", "_")
  replacement.chars=c("\\#", "$<$", "$>$", "\\%", "\\_")
  paste(lapply(seq(1,num.cols),function(i){
    if(is.numeric(row[[i]])){
      cell=round(row[[i]], max.dp)
      if(!(colnames(row)[[i]] %in% exclude.formatting)){
        cell=format(cell, nsmall = round.dp)
      }
    } else {
      cell=row[[i]]
    }
    if(sig < 0.05){
      text=paste0("\\textbf{", cell, "}")
    } else {
      text=cell
    }
    for(x in seq(1, length(illegal.chars))){
      if(grepl(illegal.chars[[x]], text)){
        text = gsub(illegal.chars[[x]], replacement.chars[[x]], text, fixed=T) 
      }
    }
    if(i == num.cols){
      sepchar="\\\\\n"
    } else {
      sepchar="& "
    }
    paste(text, sepchar, sep=" ")
  } ), sep="", collapse="")
}
