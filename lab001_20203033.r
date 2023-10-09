T = read.table("data0.txt",header = FALSE)
print(nrow(T))
print(ncol(T))
L = T$V2 >= T$V3 * 1.025
print(T[L,])
D = T$V1 == "2023.08.25"
print(T[D,2])