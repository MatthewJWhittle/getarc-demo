map_labels <- 
  function(qpal, domain, n){
    qpal_labs <- quantile(domain, seq(0, 1, 1/n), na.rm = TRUE) # depends on n from pal
    qpal_labs <- paste(round(qpal_labs, 1), "Hrs")
    paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
    
  }
map_colours <-
  function(qpal, domain, n){
    unique(qpal(sort(domain))) # hex codes
  }