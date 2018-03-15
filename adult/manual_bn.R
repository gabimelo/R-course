library(bnlearn)

named_nodes = c("american", "white", "male", "married", "primeage", "over50k")

e = empty.graph(named_nodes)

adj = matrix(0L, ncol = 6, nrow = 6,
             dimnames = list(named_nodes, named_nodes))

adj["american", "white"] = 1L
adj["white", "over50k"] = 1L
adj["male", "over50k"] = 1L
adj["married", "over50k"] = 1L
adj["primeage", "over50k"] = 1L

amat(e) = adj

e

bn.fit(e, df)
