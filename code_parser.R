remove(list = ls())
gc()
library('stringi')

# classes and constructers
source("ec_classes.R")
source("ec_constructors.R")
source("ec_methods.R")

#' Loading R scripts
#'
#' Tylko pliki teraz obsluguje
#'
#' @name scripts_loader
#' 
#' @param x A string, vector or list with R files.
#'
#' @example 
#' \dontrun{
#' scripts_loader(x)
#' }
#' @return A list with loaded libe-by-line scripts.
#' @export

scripts_loader <- function(x){
  #-- ##CORRECT dodac lepszy warunek
  if(length(x)){
    # check which of provided files are existing files
    file_ind <- ifelse(file.exists(x),T,F)
    
    # I will read scripts with respect to file_ind
    scripts_lst <- lapply(seq_along(x), function(i){
      if(file_ind[i]){
        file_con <- file(x[[i]])
        x <- readLines(file_con)
        close.connection(file_con)  
        x
      }else{
        NULL
      }
    })
    names(scripts_lst) <- x
  }else{
    stop('Unrecognized input.\nPlease ensure that input is an existing file or string with R script')
  }
  scripts_lst
}

#' Function from R script extractor
#'
#'
#' @name ec_funcs_extractor
#' 
#' @param x A list with R scripts line-by-line.
#'
#' @example 
#' \dontrun{
#' ec_funcs_extractor(x)
#' }
#' @return 
#' @export
#TODO: sprawdzac czy nazwy funckji sa unikalne - trzeba zrobic unikalne bo inaczej obiekty beda sie nadpisywac, albo zrobic obiekt z wieksza liczba wartosci

ec_funcs_extractor <- function(x){
  # argument tests 
  stopifnot(class(x)=='list')
  
  # internal functions 
  func_arguments_extractor <- function(x){
    #-- placeholder
    func_args_lst <- list()
    l_bracket_cnt <- 0
    r_bracket_cnt <- 0
    
    #- vector of chars
    x_stp <- strsplit(x, split='')[[1]]
    s_ind <- 1
    j <- 1
    for(i in seq_along(x_stp)){
      if(x_stp[i] == "("){
        l_bracket_cnt <- l_bracket_cnt + 1
      }
      if(x_stp[i] == ")"){
        r_bracket_cnt <- r_bracket_cnt + 1
      }
      
      if(x_stp[i] == "," & l_bracket_cnt == r_bracket_cnt){
        func_args_lst[[j]] <- trimws(paste0(x_stp[s_ind:(i-1)], collapse = ""))
        s_ind <- i + 1
        j <- j + 1
      }
      
      if(i == length(x_stp))
        func_args_lst[[j]] <- trimws(paste0(x_stp[s_ind:i], collapse = ""))
    }
    func_args_lst
  }
  func_body_extractor <- function(x){
    brackets_cnt <- 1
    for(i in seq_len(length(x))){
      brackets_cnt <- brackets_cnt + stri_count(x[i], fixed = '{')
      brackets_cnt <- brackets_cnt - stri_count(x[i], fixed = '}')
      if(!brackets_cnt)
        break
    }
    x[1:(i-1)]
  }
  # TODO: removing quotated strings - function names can be there
  funcs_startpoints <- function(x, funcs_nams = NULL){
    # arguments tests 
    stopifnot(length(x)>0)
    stopifnot(!is.null(funcs_nams))
    
    for(i in seq(x)){
      # temporary commentaries removal in order to prevent detecting false functions startpoints
      y <- stri_replace_all(x[[i]], regex = "#.*", replacement = "")
      # looking for functions evaluations and building map
      for(z in funcs_nams){
        ind <- which(stri_detect(y, regex = paste0("((\\s|\\A)|(-|=))",z,"\\s*\\(")))
        if(length(ind))
          eval(parse(text = paste0('ec__',z,"@evals[[length(",paste0('ec__',z,"@evals"),")+1]] <<- list('script' = \"",names(x)[i],"\", 'lines' = c(",paste0(ind, collapse =','),"))")))
      }
    }
    return(invisible(NULL))
  }
  funcs_linker <- function(x){
    # arguments tests 
    stopifnot(length(x)>0)
    
    # data frame of ranges and source scripts
    func_ec_dfr <- do.call('rbind', lapply(x, function(yy){
      yy1 <- eval(parse(text = paste0('ec__',yy)))
      data.frame('function' = yy, 'source' = yy1@source, 'range_start' = yy1@range[1], 'range_end' = yy1@range[2])
    }))
    
    for(yy in x){
      yy1 <- eval(parse(text = paste0('ec__',yy)))
      x_parent <- unique(unlist(lapply(yy1@evals, function(zz){
        x[which(apply(cbind(func_ec_dfr$source == zz$script,
                            sapply(zz$lines,function(ii){func_ec_dfr$range_start <= ii}),
                            sapply(zz$lines,function(ii){func_ec_dfr$range_end >= ii})),1,'all'))]
      })))
      
      if(length(x_parent)){
        eval(parse(text = paste0('ec__',yy,'@parents <<- append(ec__',yy,'@parents,','\"',x_parent,'\")')))
        for(xx in x_parent)
          eval(parse(text = paste0('ec__',xx,'@children <<- append(ec__',xx,'@children,','\"',yy,'\")')))
      }
    }
  }
  #
  
  funcs_names_lst <- lapply(seq_along(x), function(i){
    if(is.character(x[[i]]) && any(nchar(x[[i]])>0)){
      line_chars <- nchar(x[[i]])
      
      #-- functions locations
      funcs_loc_ind <- which(stri_detect(x[[i]], regex = "\\s*<-\\s*function\\("))
      
      #- functions names, arguments and body (if some definitions are not unique I will add artificial ID) - POZNIEJ
      if(length(funcs_loc_ind)){
        res_lst <- list()
        it <- 0
        
        for(j in funcs_loc_ind){
          it <- it + 1
          #-- function name
          func_name <- trimws(stri_extract_first(x[[i]][j],regex = "(\\A|\\s)[^\\s]+(?=(\\s*<-\\s*function))"))
          res_lst[[it]] <- func_name
          #-- function arguments list
          func_args <- func_arguments_extractor(x = stri_extract_first(x[[i]][j],regex = "(?<=\\().+(?=\\))"))
          #-- function body
          #-- finding line where "{" sign appeard - maybe should be more sophisticated
          k <- min(which(stri_detect_fixed(x[[i]][j:length(x[[i]])], pattern = "{")))
          func_body <- func_body_extractor(x = x[[i]][(j+k):length(x[[i]])])
          
          body_str <- paste0(func_body, collapse = ";")
          body_str <- stri_replace_all(body_str, fixed = "'", replacement = "\\'")
          body_str <- stri_replace_all(body_str, fixed = "\"", replacement = '\"')
          
          txt <- paste0("ec__",func_name," <<- ec_function(","args = list(",paste0("\"",paste0(func_args, collapse = "\", \""),"\""),")",
                        paste0(", body = '",body_str,"'",', source = \"',names(x)[i],'\",','range = c(start = ',j, ', end = ',j+length(func_body),'))'))
          eval(parse(text = txt))
        }
        res_lst  
      }else{
        NULL
      }
    }else{
      NULL
    }
  })
  funcs_names_vec <- unlist(funcs_names_lst)

  # startpoints
  funcs_startpoints(x, funcs_nams = funcs_names_vec)
  # linker
  funcs_linker(x = funcs_names_vec)
  
  #-- poprawic tworzenie srodowiska EC
  explain_env <<- ec_env(lapply(unlist(funcs_names_lst), function(y){if(length(y)){paste0("ec__",y)}}))
  
  return(invisible(NULL))
}

#' Explain code function
#' 
#' 
#' 
#' @name explain
#' 
#' @param x
#'
#' @example 
#' \dontrun{
#' explain(x)
#' }
#' @return 
#' @export

explain <- function(x){
  # tests
  stopifnot(is.character(x))
  # determining if there are any directories
  file_ex <- file.exists(x)
  dir_ex <- dir.exists(x)
  
  if(any(dir_ex)){
    # looking for R files in provided directories
    dir_ex <- x[which(dir_ex)]
    file_vec <- list.files(dir_ex, recursive = T, include.dirs = T, full.names = T)
  }
  if(any(file_ex)){
    file_vec <- c(files_from_dirs, x[which(file_ex)])
  }
  
  # validation if provided files have proper extension
  r_file <- which(tolower(sapply(strsplit(basename(file_vec), split = ".", fixed = T),'[[',2)) == "r")
  
  # taking only unique files with proper extension
  file_vec <- unique(file_vec[r_file])
  
  # loading scripts
  scripts_lst <- suppressWarnings(scripts_loader(file_vec))
  
  ec_funcs_extractor(x = scripts_lst)

}

ec_graph <- function(x){
  x_true <- gsub('ec__','',x)
  edge_mat <- do.call('rbind',lapply(seq(x), function(i){
    y <- eval(parse(text = x[i]))
    fun_prev <- y@parents
    fun_next <- y@children
    if(length(fun_prev))
      prev_mat <- cbind(fun_prev,x_true[i])
    else
      prev_mat <- c('','')
  
    if(length(fun_next))
      next_mat <- cbind(x_true[i],fun_next)
    else
      next_mat <- c('','')
    
    rbind(prev_mat,next_mat)  
  }))
  
  edge_mat <- edge_mat[which(nchar(edge_mat[,1]) > 0 | nchar(edge_mat[,1]) > 0),]
  if(dim(edge_mat)[1]){
   graph_fun_igh <- igraph::graph_from_edgelist(edge_mat)  
   igraph::E(graph_fun_igh)$weight <- 1
   graph_fun_igh <- igraph::simplify(graph_fun_igh, remove.multiple = T, edge.attr.comb = 'sum')
   # library(visNetwork)
   # visNetwork::visIgraph(graph_fun_igh) %>% visNetwork::visLayout(hierarchical = T) %>% visNetwork::visHierarchicalLayout(sortMethod = 'directed')
  }
}

