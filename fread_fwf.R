fread_fwf<-function(file, schema) {
  sc<-read.table(file=schema, header=TRUE, sep=";") #schema fwf
  variables<- as.character(sc[,1])  ##Variables
  start_col<-sc[,2] ##Posicion inicial
  end_col<-sc[,3] ##Posicion inicial
  start_end <- cbind(start_col, end_col) 
  
  dt<-  fread(file=file,     
                   colClasses = "character", 
                   sep = "\n", 
                   header = FALSE)
  
 extrae<-function(x) {
    apply(start_end, 1, function(y) stri_sub(x, y[1], y[2]))}
   
 dt[, (variables) := data.table((lapply(dt, extrae))$V1)] [,V1:=NULL]
 
 rm(extrae)
 
 return(dt)
}
