poscountiter <- function(n, givelayout = FALSE, qlays = list()){
  
  sum1 = 0
  j0 = 1
  k0list = list()
  for(i in 2:n){
    k0list[[i]] = integer(0)
  }

  ite = 0
  mat = data.frame(x = numeric(0), y = numeric(0))
  k0list[[j0]] = 1:n
  while(j0 > 0){
    mat = dplyr::filter(mat, x<j0)
    if(length(k0list[[j0]]) == 0){
      k0list[[j0]] = setdiff(1:n, c(unique(mat$y), j0 + mat$y - mat$x, mat$y + mat$x - j0))
    }
    
    if(j0 == n){
      sum1 = sum1 + 1
      mat = rbind.data.frame(mat, data.frame(x = j0, y = k0list[[j0]][1]))
      if(givelayout & length(k0list[[n]] != 0)){qlays[[length(qlays) + 1]] = mat}
      k0list[[j0]] = integer(0)
      j0 = max(which(sapply(k0list, "length")>0))
      next
    }
    
    if(length(k0list[[j0]]) == 0){
      mat = dplyr::filter(mat, x != j0)
      hvec = which(sapply(k0list, "length")>0)
      j0 = ifelse(length(hvec) > 0, max(hvec), 0)
      next
    }else{
      k0 = k0list[[j0]][1]
      if(length(k0list[[j0]]) == 1){
        k0list[[j0]] = integer(0)
      }else{
        k0list[[j0]] = k0list[[j0]][2:length(k0list[[j0]])]
      }
      mat = rbind.data.frame(mat, data.frame(x = j0, y = k0))
      j0 = j0 + 1
    }
  }
  if(givelayout){
    return(list(s = sum1, qlays = qlays))
  }else{
    return(list(s = sum1))
  }
}

poscount <- function(n, j0 = 1, mat, givelayout = FALSE, qlays = list()){

  if(j0 > n){
    if(givelayout){
      qlays[[length(qlays) + 1]] = mat
      return(list(qlays = qlays, s = 1))
    }else{
      return(list(s = 1))
    }
  }
  
  if(missing(mat))
    mat = data.frame(x = numeric(0), y = numeric(0))
  
  if(nrow(mat) == n){
    if(givelayout){
      qlays[[length(qlays) + 1]] = mat
      return(list(qlays = qlays, s = 1))
    }else{
      return(list(s = 1))
    }
  }
  
  sum1 = 0
  
  sett = setdiff(1:n, c(unique(mat$y), j0 + mat$y - mat$x, mat$y + mat$x - j0))
  
  if(length(sett) == 0){
    if(givelayout){
      return(list(qlays = qlays, s = 0))
    }else{
      return(list(s = 0))
    }
  }
  
  for(k0 in sett){
    mat0 = mat
    mat0 = rbind.data.frame(mat0, data.frame(x = j0, y = k0))
    if(givelayout){
      pp = poscount(n, j0 + 1, mat = mat0, givelayout = givelayout, qlays = qlays)
      sum1 = sum1 + pp$s
      qlays = pp$qlays
    }else{
      sum1 = sum1 + poscount(n, j0 + 1, mat = mat0)$s
    }
  }
  if(givelayout){
    return(list(qlays = qlays, s = sum1))
  }else{
    return(list(s = sum1))
  }
}

queenchess <- function(qlays, maxboard = 20, randomorder = TRUE, tsize){
  
  require(ggplot2)
  mat1 = data.frame()
  n = max(qlays[[1]]$x)
  if(length(qlays) > maxboard){
    if(randomorder){
      iset = sample(1:length(qlays), maxboard)  
    }else{
      iset = 1:maxboard
    }
  }else{
    iset = 1:length(qlays)
  }
  for(i in iset){
    
    mat = as.data.frame(do.call("rbind", lapply(1:n, function(i) cbind(i, 1:n))))
    colnames(mat) = c('x', 'y')
    qq = qlays[[i]]
    qq$room = 1
    qq$piece = sprintf("\u265b")
    
    mat = merge(mat, qq, all.x = TRUE, by = c('x', 'y'))
    mat$i = i
    mat1 = rbind.data.frame(mat1, mat)
  }
  
  if(missing(tsize))
    tsize = ceiling(24 - length(iset))
  
  ggplot(mat1, aes(x = x, y = y, fill = (x+y)%%2)) +
    theme(legend.position = "none", axis.ticks = element_blank(), panel.background = element_blank(),
          strip.background = element_blank(), strip.text.x = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(), aspect.ratio = 1) +
    geom_tile(color = "black", size = 0.5, alpha = 0.75) +
    scale_fill_gradient(low = "white", high = "black") +
    xlab(NULL) + ylab(NULL) +
    geom_text(aes(label = piece, color = "darkred"), size = tsize, vjust = 0.36, alpha = 1) +
    facet_wrap(~i)
}

if(FALSE){
queenchess(poscount(n = 5, givelayout = TRUE)$qlays, randomorder = FALSE)

nvec = c(1,4:12)
timevec = matrix(-1, ncol = 3, nrow = length(nvec))
i = 1
for(n in nvec){
  timevec[i,1] = n
  timevec[i,2] = system.time(poscountiter(n = n))[3]
  timevec[i,3] = system.time(poscount(n = n))[3]
  i = i + 1
}
}