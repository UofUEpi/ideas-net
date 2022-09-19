library(data.table)

# Processing papers
dat <- readLines("ideas.csv")
dat <- gsub("^\"|\"$", "", dat)

dat2 <- lapply(dat, strsplit, split = "\\.")
dat2 <- lapply(dat2, unlist)

dat2 <- lapply(dat2, \(d) {

  auths <- strsplit(d[2], split = ",\\s*")[[1L]]

  data.table(
    id      = rep(d[1], length(auths)),
    authors = auths
  )

}) |> rbindlist()

dat2[, authors := gsub("^\\s+|\\s+$", "", authors)]

# Processing who is from the u
query <- readLines("pubmed-query.txt")
query <- stringr::str_extract_all(query, "(?<=[(])[[:alpha:] -]+(?=\\[(A|a))")[[1]]

query <- sort(query) |> tolower()

dat2[, is_u := tolower(authors) %in% query]

# Creating the edgelist
nodes <- dat2[, .(authors, is_u)] |> unique()

edges <- merge(
  dat2[, .(id, ego   = authors)],
  dat2[, .(id, alter = authors)],
  allow.cartesian = TRUE
)

edges <- edges[, .(
  ego   = fifelse(ego > alter, ego, alter),
  alter = fifelse(ego > alter, alter, ego)
  )]

edges <- edges[, .(n = .N), by = .(ego, alter)]
edges <- edges[ego != alter]


# Drawing the colaboration network ---------------------------------------------
library(netplot)
library(igraph)

set.seed(7723)
net <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
ly  <- layout_nicely(net)

nplot(
  net,
  sample.edges = .7,
  vertex.color = fifelse(V(net)$is_u, "tomato", "steelblue")
  )


library(rgexf)
gf <- igraph.to.gexf(net, nodesVizAtt = list(
  position = cbind(ly, 0),
  color    = fifelse(V(net)$is_u, "tomato", "steelblue")
))
plot(gf, nodeSizeFactor = .05, edgeWidthFactor = .125)
