rm(list=ls(all=T))

setwd("/Volumes/IKH/Kati/Lox/LoxMods") #E:\Kati\Lox\LoxMods
data <- read.csv("ArcMap_sorted.csv")

rows <- (max(data$Yutm) - min(data$Yutm))/400 + 1
cols <- (max(data$Xutm) - min(data$Xutm))/400 + 1

elev <- matrix(nrow = rows, ncol = cols)
wat_lev <- matrix(nrow = rows, ncol = cols)
can_dist <- matrix(nrow = rows, ncol = cols)
struc_dist <- matrix(nrow = rows, ncol = cols)
veg <- matrix(nrow = rows, ncol = cols)


col <- 1

for (x in seq(min(data$Xutm), max(data$Xutm), 400)) {
    
    a <- subset(data, Xutm == x)
    a <- arrange(a, Xutm, -Yutm)
    
    if ((max(data$Yutm) - a[1,4]) == 0) {
        veg[,col] <- rbind(cbind(a[,10]), cbind(matrix(nrow = (rows - length(a[,10])), ncol = 1))) #deleted '- b' from the 'nrow =' statement
    } else {
        b <- ((max(data$Yutm) - a[1,4])/400 - 1)
        veg[,col] <- rbind(cbind(matrix(nrow = b, ncol = 1)), cbind(a[,10]), cbind(matrix(nrow = (rows - length(a[,10]) - b), ncol = 1)))
    }
    
    for (t in 1:rows) {
        if (is.na(veg[t,col] == T)) {
            veg[t,col] <- ""
        }
    }
    
    col <- col + 1
}


write.table(veg, file = paste("sorted_data-veg.csv", sep = ""))


