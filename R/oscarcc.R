#' Functions by Òscar Conejo
#' -------------------------
#' @param ip 
#' @return
#' @export
#' @examples
#' 

#' Function 1: Aquesta funció verifica si una IP és vàlida al introduïr-a com a string.

format<-FALSE
f1<-function(ip){
  val<-long2ip(ip)
  format<-hasIPformat(val)
  
  if(format==TRUE){
    mis<-"és vàlida"
  }else{
    mis<-"NO és vàlida"
  }
  resp<-(c("La IP:",val,mis))
  return (resp)
}

#' Function 2: Retorna una adreça IP random

f2<-function(){
  
  randIP<-random::randomNumbers(4,1,254,1,10)
  IP<-(c(randIP[1,],randIP[2,], randIP[3,], randIP[4,]))
  return (IP)
}


#' Function 3: Retorna totes les adreces IP d'un rang concret

f3<-function(rang){
  
  if(rang=="A"){
    imprIP(0, 127)
  }else if(rang=="B"){
    imprIP(128, 191)
  }else if(rang=="C"){
    imprIP(192, 223)
  }
}

imprIP<-function(p,l){
  for (i in p:l){
    for (y in 0:255){
      for (x in 0:255){
        for (z in 0:255){
          print (c(i,y,x,z))
        }
      }
    }
  }
}
