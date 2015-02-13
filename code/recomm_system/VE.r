library(plyr)
library(bnlearn)
evaluate_node <- function(bit_string, treatment_options, n_options, fitted, method){
  #####Obtain CPT's#####
  cpt <- lapply(1:length(fitted),function(y){
    d <- data.frame(fitted[[y]]$prob)
    if(nrow(d)) {
      colnames(d) <- c(fitted[[y]]$node,colnames(d)[-1])
      d$Freq[which(d$Freq=="NaN")] <- 0
    }
    return(d)
  })
  #######################
  options(warn=-1)
  node_order <- elimination_order(fitted)
  cpt_tmp <- cpt
  evid <- treatment_options
  ev <- bit_string
  p <- get_prob(cpt_tmp,evid,ev)
  p
  return(p[2,2]/sum(p[,2]))
}
get_prob <- function(cpt_tmp,evid,ev){
  for(e in 1:length(evid)){
    cpt_tmp <- get_evidencetable(cpt_tmp,evid[e],ev[e])
  }
  for(t in 1:length(node_order)){
    table_names <- sapply(cpt_tmp,colnames)
    tk <- grep(node_order[t],table_names)
    if(length(tk)){
      if(length(tk)>1) Ts <- multiplytables(cpt_tmp[tk]) else Ts <- cpt_tmp[[tk]]
      Tv <- sumtables(Ts,node_order[t])
      #     print(Tv)
      cpt_tmp <- c(cpt_tmp[-tk],list(Tv))
    }
  }
  
  ind <- grep("vital_status",sapply(cpt_tmp,colnames))
  p <- cpt_tmp[[ind]]
#   joint_prob <- prod(unlist(cpt_tmp[-ind]))
#   p$Freq <- p$Freq*prod(joint_prob)
  return(p)
}
#"Temozolomide"==F
# vital_status      Freq
# 1        FALSE 0.1301114
# 2         TRUE 0.0000000
#"Temozolomide"==T
# vital_status      Freq
# 1        FALSE 0.5893641
# 2         TRUE 0.2737064
#without any evidence
# vital_status      Freq
# 1        FALSE 0.7194755
# 2         TRUE 0.2737064
# person_neoplasm_cancer_status==WITH TUMOR
# vital_status      Freq
# 1        FALSE 0.7158943
# 2         TRUE 0.2343132
# person_neoplasm_cancer_status==TUMOR FREE
# vital_status        Freq
# 1        FALSE 0.003581206
# 2         TRUE 0.039393261
###################################
get_evidencetable <- function(cpt_tmp,e,ev){
  table_names <- sapply(cpt_tmp,colnames)
  tk <- grep(e,table_names)
  for(m in 1:length(tk)){
    cp <- cpt_tmp[[tk[m]]]
    cpt_tmp[[tk[m]]] <- cpt_tmp[[tk[m]]][which(cp[,e]==ev),]
  }
  return(cpt_tmp)
}
#######################
#####multiply CPT's######
multiplytables <- function(pt){
  t1 <- pt
  cn <- Reduce(intersect,lapply(t1,colnames))
  Tz <- Reduce(function(x,y) merge(x,y,all=TRUE,by=cn[cn!="Freq"]),t1)
  f <- grep("Freq",colnames(Tz))
  Freq_tmp <- apply(Tz[,f],1,prod)
  Tz$Freq_tmp <- Freq_tmp
  Tz <- Tz[,-f]
  colnames(Tz)  <- c(colnames(Tz)[-ncol(Tz)],"Freq")
  return(Tz)
}
#####sum a CPT####
sumtables <- function(pt,z){
  y <- setdiff(unlist(colnames(pt)),z)
  if(length(y)>1)  Td <- aggregate(Freq ~ ., pt[,y], sum)
  else Td <- sum(pt[,"Freq"])
  return(Td)
}
####Find an elimination order####
elimination_order <- function(res){
  G <- res
  node_order <- ""
  for(k in 1:(length(nodes(res))-1)){
    blk_lst <- which(nodes(G)=="vital_status")
    node <- nodes(G)[-blk_lst]
    w <- width(G)[-blk_lst]
    node_order[k] <- node[which.min(w)]
    G <- subgraph(G,nodes=nodes(G)[nodes(G)!=node_order[k]])
  }
  return(node_order)
}
####find min-fill width of a node####
width <- function(G){
  # G <- res
  c <- rep(0,length(nodes(G)))
  for(m in 1:length(nodes(G))){
    n <- nodes(G)[m]
    tmp <- nbr(G,n)
    g <- subgraph(x=G,nodes=nodes(G)[nodes(G)!=n])
    if(length(tmp)>1){
      for(i in 2:length(tmp)){
        for(j in i:length(tmp)){
          ex <- paste(tmp[i-1],"|",tmp[i],collapse="")
          e <- grepl(ex,G$arcs[,1]) & grepl(ex,G$arcs[,2])
          #           e <- path(g,from=tmp[i-1],to=tmp[j],underlying.graph=T)
          if(!sum(e)) c[m] <- c[m]+1
        }
      }
    }
  }
  return(c)
}
##########################################