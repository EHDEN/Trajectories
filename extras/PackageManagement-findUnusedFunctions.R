if (!require(mvbutils)) install.packages("mvbutils")
#get all R-files from the "R" folder
thefiles = list.files(path = "./R/", full.names = TRUE)
#load all R-files to memory
sapply(thefiles,source)
par(mar=rep(0.1,4))
foodweb(
        border=TRUE,
        textcolor="black", cex=1.0, lwd=2)


result <- foodweb(plotting=FALSE)

# The following line returns the number of times each function is called
res <- sapply(rownames(result$funmat), function(n) length(callers.of(n)))

# Get those functions that are never called:
names(res[res==0])
