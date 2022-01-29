This notebook contains two simple functions poscount and queenchess.

1.  poscount: Counts the total number of possible placements of n queens
    on an nxn chessboard given that no queen can attack the other in one
    turn. This function can also return the list of all possible
    layouts.

2.  queenchess: This function plots the possible placements of queens on
    a chessboard. Maximum number of boards that can be displayed is
    bounded above by 20 as the default.

<!-- -->

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

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Registered S3 methods overwritten by 'tibble':
    ##   method     from  
    ##   format.tbl pillar
    ##   print.tbl  pillar

    queenchess <- function(qlays, maxboard = 20, randomorder = TRUE, tsize){
      
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
        geom_tile(color = "white", size = 1) +
        xlab(NULL) + ylab(NULL) +
        geom_text(aes(label = piece, color = (x+y+1)%%2), size = tsize) +
        facet_wrap(~i)
    }

    cat("The number of possible layouts for an 8x8 and a 10x10chessboard are ", poscount(n = 8)$s, " and ", poscount(n= 10)$s, '.\n', sep = '')

    ## The number of possible layouts for an 8x8 and a 10x10chessboard are 92 and 724.

Below we can see two example plots. The size of the queen can be
adjusted depending on plot size.

    plot(queenchess(poscount(n = 8, givelayout = TRUE)$qlays, maxboard = 6, tsize = 5))

![](counting_queen_layouts_files/figure-markdown_strict/plot-1.png)

    plot(queenchess(poscount(n = 5, givelayout = TRUE)$qlays, tsize = 5))

![](counting_queen_layouts_files/figure-markdown_strict/plot-2.png)

    plot(queenchess(poscount(n = 4, givelayout = TRUE)$qlays))

![](counting_queen_layouts_files/figure-markdown_strict/plot-3.png)