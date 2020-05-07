require(Matrix)
snp <- data.table::fread("data/Bna_EFF_f.txt",data.table = F)
id <- as.character(snp[,1])
files <- list.files(path = "./prepare_data/", pattern = ".allele")
for (i in seq_along(1:length(files))){
  cat(paste0(files[i], " Starts!\n"))
  data <- data.table::fread(paste0("./prepare_data/", files[i]), data.table = F)
  data <- data[data$ID %in% id,]
  snp_id <- data[,1]
  out_id <- paste0("./prepare_data/",files[i],".idnum")
  write.table(snp_id, file = out_id, col.names = F, row.names = F,quote = F)
  rownames(data) <- data[,1]
  data <- data[,-1]
  data <- as.matrix(data)
  chr <- str_sub(rownames(data)[2], start = 1, end = 3)
  
  # snp.data.allele <- apply(data, 1, function(x){
  #   x <- x[!x %in% c("H", "N")]
  #   y <- sort(table(x), decreasing=TRUE)
  #   major <- names(y)[1]
  #   minor <- names(y)[2]
  #   return(c(major, minor, "H"))
  # })
  # snp.data.allele <- t(snp.data.allele)
  # colnames(snp.data.allele) <- c("major", "minor", "Het")
  snp.data.inter <- apply(data, 1, function(x){
    x.fil <- x[!x %in% c("H", "N")]
    y <- sort(table(x.fil), decreasing=TRUE)
    major <- names(y)[1]
    minor <- names(y)[2]
    z <- x
    z[z==major] <- 0
    z[z==minor] <- 1
    z[z=="H"] <- 2
    z[z=="N"] <- NA
    return(z)
  })
  snp.data.inter <- t(snp.data.inter)
  mode(snp.data.inter) <- "integer"
  
  library(Matrix)
  snp.data.inter.Matrix <- as(snp.data.inter, "sparseMatrix")
  start <- str_split(files[i], pattern = "-")[[1]][2]
  end <- str_split(files[i], pattern = "-")[[1]][3]
  file <- paste0(chr,"-", start,"-", end, ".RData")
  outfile <- paste0("./prepare_data/",file)
  save(snp.data.inter.Matrix,file = outfile,compress = "xz")
  cat(paste0(files[i]," finished!\n"))
}

