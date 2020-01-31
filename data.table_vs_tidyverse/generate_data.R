
#args = commandArgs(trailingOnly=TRUE)
n=10000000
#n = as.integer(args[1])
df1 = data.frame(
    col1=runif(n),
    c1  = runif(n),
    c2 = runif(n),
    group=c(rep("A", n/2), rep("B", n/2))
    )

df2 = as.data.table(df1)


library(data.table)
