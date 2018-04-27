


bin <- function(df_col,levs,rev = FALSE){
  require(Hmisc,quietly = T)
  require(forcats,quietly = T)
  s <- as.factor(as.character(as.numeric(cut2(df_col, g=levs))))
  if (rev == TRUE){
    s <- fct_rev(s)
  }
  s
}# bins

look <- function(x, y = 15){
  require(stringr,quietly = T)
  require(tidyverse,quietly = T)
  message('############################')
  message('############################')
  nm <-deparse(substitute(x))
  print(nm)
  message('-----------------')
  message('Number of variables')
  print(length(x))
  message('Number of rows')
  print(nrow(x))
  message('-----------------')
  is.even <- function(x) x %% 2 == 0
  
  if(is.even(length(x)) == F){
    x$pad <- 1:nrow(x)
  }
  
  
  k <- round(length(x)/2,0)
  l <- k+1
  y <- data_frame(
    Name = str_trunc(names(x)[1:k],y,'right',ellipsis = ""), #35
    Index = 1:k,                                                                #35
    Type = str_trunc(flatten_chr(map(x[1:k],class)),3,'right',ellipsis = ""),# 35
    Missing = flatten_dbl(map(x[1:k],function(p){sum(is.na(p))})), #35
    Name. = str_trunc(names(x)[l:length(x)],y,'right',ellipsis = ""), #35
    Index. = l:length(x) ,#35
    Type. = str_trunc(flatten_chr(map(x[l:length(x)],class)),3,'right',ellipsis = ""),#35
    Missing. = flatten_dbl(map(x[l:length(x)],function(p){sum(is.na(p))})))#35
  
  print(y,n = nrow(y))
  message('++++++++++++++++++++++++++++')
  message('++++++++++++++++++++++++++++')
}

'%!in%' <- function(x,y)!('%in%'(x,y))


clean <- function(x){
  x <- x %>% str_to_title() # makes title case
  x <- x %>% str_trim()# trims off whitespace
  x <- gsub('\\.',' ',x)#removes periods
  x <- gsub('\\,',' ',x)#removes commas
  x
}

clean_all <- function(x){
  x <- x %>% str_to_lower() # makes tolower case
  x <- x %>% str_trim()# trims off whitespace
  x <- gsub("[^A-Za-z0-9]",' ',x) # keeps only alpha_numeric
as
  x
}

clean_map <- function(x){
  x <- gsub(' County','',x)  
  x <- gsub('_',' ',x)  
  x <- x %>% str_to_lower()
  x <- x %>% str_trim()# trims off whitespace
  x <- gsub('\\.','',x)#removes periods
  x <- gsub('\\,','',x)#removes periods
  return(x)
}


join.nrow <- function(x,y){
  i <- list()
  i$left <- left_join(x,y) %>% nrow
  i$right <- right_join(x,y) %>% nrow
  i$inner <- inner_join(x,y) %>% nrow
  i$anti <- anti_join(x,y) %>% nrow
  print(nrow(x))
  print(nrow(y))
  return(i)
}

makemetrics <- function(gr) {
  data.frame(Degree=degree(gr), 
             Closeness = closeness(gr), 
             Betweenness = betweenness(gr))
}

factor.color.D2 <- function(x,col){
  # x[col] <- as.factor(as.character(x[col]))
  i <- colorRampPalette(brewer.pal(8,'Dark2'))#z is the color
  e <- data.frame(
    vary1 = levels(as.factor(as.character(x[[col]]))),
    vary2 = i(length(levels(as.factor(as.character(x[[col]]))))))
  colnames(e)[1] <- colnames(x[col])
  r <- paste(colnames(x[col]),'.color',sep = '')
  colnames(e)[2] <- r
  left_join(x,e)
  
}
node_ids <- function(x){
 # x requires this order of varibles
  #  - - - V1,V2,ID.match - - - -
  
  colnames(x)[1] <- "name"
  # x <- x %>% filter(x[[1]]!=x[[2]])#takes out self-loops
  x[4] <- ifelse(x[[1]] %in% x[[2]],NA,as.character(x[[2]]))
  w <- x %>% 
    gather(key,name,-3,-2) %>% 
    select(name) %>% 
    na.omit %>% 
    distinct %>% 
    left_join(.,x) %>% 
    .[c(1,3)] %>% 
    distinct()
  y <- graph_from_data_frame(x)# puts the edgelist into x
  z <- igraph::as_data_frame(y, what="vertices")#puts vertices into z
  left_join(z[1],w)
}



to.character <- function(x){ #changes to character, use with lapply

  x <- x %>% as.character() 
} 



rescale <- function(nchar,low,high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d) + low
  rscl
}

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

factorise <- function(x){
  x <- as.factor(as.character(x))
}


identicator <- function(x){
  
  for ( i in seq_along(x)){
    
    x <- ifelse(agrepl(x[[i]],x),x[[i]],x)
    
  }
  x  ###
}


identicator_ii <- function(x = match,y = matched,z = .1){
    
    for ( i in seq_along(x)){
        
        y <- ifelse(agrepl(x[[i]],y,max.distance = z),x[[i]],y)
        
    }
    y  ###
}

# dplyr helpers


strip_n <- function(x = a.data_frame,y = NUMBER.to.filter,z = y){
  
  x %>% 
    map_df(~n_distinct(.)) %>% 
    gather(key,value) %>% 
    filter(value >= y) %>% 
    filter(value <= z) %>% 
    pull(key) 
  
  
}

strip_percent_na <- function(x = a.data_frame,y = DECIMAL.to.filter){
  x %>% 
    map_df(~sum(is.na(.))) %>% 
    gather(key,value) %>% 
    mutate(value = value/(nrow(.))*100) %>% 
    filter(value <= y) %>% 
    pull(key) 
}


