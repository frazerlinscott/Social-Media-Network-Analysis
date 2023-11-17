#2.4) Perform centrality analysis by detecting degree centrality, betweenness centrality, and closeness centrality
#Centrality

# Create actor network - Twitter Post Malone Data

post_twitter_actor_network <- twitter_data %>% Create("actor")
post_twitter_actor_graph <- twitter_actor_network %>% Graph()
#write.graph(twomode_graph, file = "PostTwitterTwomode.graphml", format = "graphml")
V(post_twitter_actor_graph)$name <- V(post_twitter_actor_graph)$screen_name

sort(degree(post_twitter_actor_graph, mode = "total"), decreasing = TRUE)[1:20]
sort(closeness(post_twitter_actor_graph, mode = "total"), decreasing = TRUE)[1:20]
sort(betweenness(post_twitter_actor_graph, directed = FALSE), decreasing = TRUE)[1:20]

#--------------------------------------------------------------------------------------------

#Plot degree - Post
# Calculate the degree centrality for 
Post_degree_values <- degree(post_twitter_actor_graph, mode = "total")
# Sort the degree centrality values and extract the top 20 nodes
Post_top_20_degree <- sort(Post_degree_values, decreasing = TRUE)[1:20]
# Get the node names of the top 20 nodes
Post_top_20_degree_node_names <- names(Post_top_20_degree)
# Find the corresponding vertex ids in the graph
Post_top_20_degree_vertex_ids <- match(Post_top_20_degree_node_names, V(post_twitter_actor_graph)$name)
# Create the subgraph containing the top 20 nodes
Post_top_20_degree_subgraph <- induced_subgraph(post_twitter_actor_graph, vids = Post_top_20_degree_vertex_ids)
# Create a data frame with node names and degree values
Post_degree_df <- data.frame(name = names(Post_degree_values), degree = Post_degree_values)
# Filter the data frame to keep only the top 20 nodes
Post_top_20_degree_df <- Post_degree_df[Post_top_20_degree_vertex_ids, ]
# Create vertex labels by combining node names and their degree values
Post_vertex_labels <- paste(Post_top_20_degree_df$name, "\n", sprintf("%d", Post_top_20_degree_df$degree), sep = "")
Post_vertex_labels
# Calculate the layout for the graph
Post_layout <- layout_nicely(Post_top_20_degree_subgraph)
# Plot the graph with combined node labels and degree centrality
plot(Post_top_20_degree_subgraph, layout = Post_layout,
     vertex.label = Post_vertex_labels,
     vertex.size = 6, edge.arrow.size = 0.1,
     vertex.label.dist = 1.5)

#--------------------------------------------------------------------------------------------

#Plot closeness - Post

# Calculate the closeness centrality
Post_closeness_values <- closeness(post_twitter_actor_graph)
# Sort the closeness centrality values and extract the top 20 nodes
Post_top_20_closeness <- sort(Post_closeness_values, decreasing = TRUE)[1:20]
# Get the node names of the top 20 nodes
Post_top_20_closeness_node_names <- names(Post_top_20_closeness)
# Find the corresponding vertex ids in the graph
Post_top_20_closeness_vertex_ids <- match(Post_top_20_closeness_node_names, V(post_twitter_actor_graph)$name)
# Create the subgraph containing the top 20 nodes
Post_top_20_closeness_subgraph <- induced_subgraph(post_twitter_actor_graph, vids = Post_top_20_closeness_vertex_ids)
# Create a data frame with node names and closeness values
Post_closeness_df <- data.frame(name = names(Post_closeness_values), closeness = Post_closeness_values)
# Filter the data frame to keep only the top 20 nodes
Post_top_20_closeness_df <- Post_closeness_df[Post_top_20_closeness_vertex_ids, ]
# Create vertex labels by combining node names and their closeness values
Post_vertex_labels_closeness <- paste(Post_top_20_closeness_df$name, "\n", sprintf("%.1f", Post_top_20_closeness_df$closeness), sep = "")
# Calculate the layout for the graph
Post_layout_closeness <- layout_nicely(Post_top_20_closeness_subgraph)
# Plot the graph with combined node labels and closeness centrality
plot(Post_top_20_closeness_subgraph, layout = Post_layout_closeness,
     vertex.label = Post_vertex_labels_closeness,
     vertex.size = 6, edge.arrow.size = 0.1,
     vertex.label.dist = 1.5)


#Plot betweenness - Post

Post_betweenness_values <- betweenness(post_twitter_actor_graph, directed = FALSE)
# Sort the betweenness centrality values and extract the top 20 nodes
Post_top_20_betweenness <- sort(Post_betweenness_values, decreasing = TRUE)[1:20]
# Get the node names of the top 20 nodes
Post_top_20_betweenness_node_names <- names(Post_top_20_betweenness)
# Find the corresponding vertex ids in the graph
Post_top_20_betweenness_vertex_ids <- match(Post_top_20_betweenness_node_names, V(post_twitter_actor_graph)$name)
# Create the subgraph containing the top 20 nodes
Post_top_20_betweenness_subgraph <- induced_subgraph(post_twitter_actor_graph, vids = Post_top_20_betweenness_vertex_ids)
# Create a data frame with node names and betweenness values
Post_betweenness_df <- data.frame(name = names(Post_betweenness_values), betweenness = Post_betweenness_values)
# Filter the data frame to keep only the top 20 nodes
Post_top_20_betweenness_df <- Post_betweenness_df[Post_top_20_betweenness_vertex_ids, ]
# Create vertex labels by combining node names and their betweenness values
Post_vertex_labels_betweenness <- paste(Post_top_20_betweenness_df$name, "\n", sprintf("%.0f", Post_top_20_betweenness_df$betweenness), sep = "")
# Calculate the layout for the graph
Post_layout_betweenness <- layout_nicely(Post_top_20_betweenness_subgraph)
# Plot the graph with combined node labels and betweenness centrality
plot(Post_top_20_betweenness_subgraph, layout = Post_layout_betweenness,
     vertex.label = Post_vertex_labels_betweenness,
     vertex.size = 5, edge.arrow.size = 0.5,
     vertex.label.dist = 2)

#--------------------------------------------------------------------------------------------

# Create actor network - Twitter Bruno Mars Data
#View(Bruno_twitter_data)
bruno_twitter_actor_network <- Bruno_twitter_data %>% Create("actor")
bruno_twitter_actor_graph <- bruno_twitter_actor_network %>% Graph()
#write.graph(twomode_graph, file = "PostTwitterTwomode.graphml", format = "graphml")
V(bruno_twitter_actor_graph)$name <- V(bruno_twitter_actor_graph)$screen_name
V(bruno_twitter_actor_graph)$name
sort(degree(bruno_twitter_actor_graph, mode = "total"), decreasing = TRUE)[1:20]
sort(closeness(bruno_twitter_actor_graph, mode = "total"), decreasing = TRUE)[1:20]
sort(betweenness(bruno_twitter_actor_graph, directed = FALSE), decreasing = TRUE)[1:20]

#--------------------------------------------------------------------------------------------

# Plot degree - Bruno
# Calculate the degree centrality
Bruno_degree_values <- degree(bruno_twitter_actor_graph, mode = "total")
# Sort the degree centrality values and extract the top 20 nodes
Bruno_top_20_degree <- sort(Bruno_degree_values, decreasing = TRUE)[1:20]
# Get the node names of the top 20 nodes
Bruno_top_20_degree_node_names <- names(Bruno_top_20_degree)
# Find the corresponding vertex ids in the graph
Bruno_top_20_degree_vertex_ids <- match(Bruno_top_20_degree_node_names, V(bruno_twitter_actor_graph)$name)
# Create the subgraph containing the top 20 nodes
Bruno_top_20_degree_subgraph <- induced_subgraph(bruno_twitter_actor_graph, vids = Bruno_top_20_degree_vertex_ids)
# Create a data frame with node names and degree values
Bruno_degree_df <- data.frame(name = names(Bruno_degree_values), degree = Bruno_degree_values)
# Filter the data frame to keep only the top 20 nodes
Bruno_top_20_degree_df <- Bruno_degree_df[Bruno_top_20_degree_vertex_ids, ]
# Create vertex labels by combining node names and their degree values
Bruno_vertex_labels <- paste(Bruno_top_20_degree_df$name, "\n", sprintf("%d", Bruno_top_20_degree_df$degree), sep = "")
# Calculate the layout for the graph
Bruno_layout_degree <- layout_with_kk(Bruno_top_20_degree_subgraph)
# Plot the graph with combined node labels and degree centrality
plot(Bruno_top_20_degree_subgraph, layout = Bruno_layout_degree,
     vertex.label = Bruno_vertex_labels,
     vertex.size = 6, edge.arrow.size = 0.1,
     vertex.label.dist = 1.5)


#Plot closeness - Bruno
# Calculate the closeness centrality for Bruno
Bruno_closeness_values <- closeness(bruno_twitter_actor_graph)
# Sort the closeness centrality values and extract the top 20 nodes
Bruno_top_20_closeness <- sort(Bruno_closeness_values, decreasing = TRUE)[1:20]
# Get the node names of the top 20 nodes
Bruno_top_20_closeness_node_names <- names(Bruno_top_20_closeness)
# Find the corresponding vertex ids in the graph
Bruno_top_20_closeness_vertex_ids <- match(Bruno_top_20_closeness_node_names, V(bruno_twitter_actor_graph)$name)
# Create the subgraph containing the top 20 nodes
Bruno_top_20_closeness_subgraph <- induced_subgraph(bruno_twitter_actor_graph, vids = Bruno_top_20_closeness_vertex_ids)
# Create a data frame with node names and closeness values
Bruno_closeness_df <- data.frame(name = names(Bruno_closeness_values), closeness = Bruno_closeness_values)
# Filter the data frame to keep only the top 20 nodes
Bruno_top_20_closeness_df <- Bruno_closeness_df[Bruno_top_20_closeness_vertex_ids, ]
# Create vertex labels by combining node names and their closeness values
Bruno_vertex_labels_closeness <- paste(Bruno_top_20_closeness_df$name, "\n", sprintf("%.1f", Bruno_top_20_closeness_df$closeness), sep = "")
# Calculate the layout for the graph
Bruno_layout_closeness <- layout_nicely(Bruno_top_20_closeness_subgraph)
# Plot the graph with combined node labels and closeness centrality
plot(Bruno_top_20_closeness_subgraph, layout = Bruno_layout_closeness,
     vertex.label = Bruno_vertex_labels_closeness,
     vertex.size = 6, edge.arrow.size = 0.1,
     vertex.label.dist = 1.5)

#Plot betweenness - Bruno

# Calculate the betweenness centrality for Bruno
Bruno_betweenness_values <- betweenness(bruno_twitter_actor_graph, directed = FALSE)
# Sort the betweenness centrality values and extract the top 20 nodes
Bruno_top_20_betweenness <- sort(Bruno_betweenness_values, decreasing = TRUE)[1:20]
# Get the node names of the top 20 nodes
Bruno_top_20_betweenness_node_names <- names(Bruno_top_20_betweenness)
# Find the corresponding vertex ids in the graph
Bruno_top_20_betweenness_vertex_ids <- match(Bruno_top_20_betweenness_node_names, V(bruno_twitter_actor_graph)$name)
# Create the subgraph containing the top 20 nodes
Bruno_top_20_betweenness_subgraph <- induced_subgraph(bruno_twitter_actor_graph, vids = Bruno_top_20_betweenness_vertex_ids)
# Create a data frame with node names and betweenness values
Bruno_betweenness_df <- data.frame(name = names(Bruno_betweenness_values), betweenness = Bruno_betweenness_values)
# Filter the data frame to keep only the top 20 nodes
Bruno_top_20_betweenness_df <- Bruno_betweenness_df[Bruno_top_20_betweenness_vertex_ids, ]
# Create vertex labels by combining node names and their betweenness values
Bruno_vertex_labels_betweenness <- paste(Bruno_top_20_betweenness_df$name, "\n", sprintf("%.0f", Bruno_top_20_betweenness_df$betweenness), sep = "")
# Calculate the layout for the graph
Bruno_layout_betweenness <- layout_nicely(Bruno_top_20_betweenness_subgraph)
# Plot the graph with combined node labels and betweenness centrality
plot(Bruno_top_20_betweenness_subgraph, layout = Bruno_layout_betweenness,
     vertex.label = Bruno_vertex_labels_betweenness,
     vertex.size = 5, edge.arrow.size = 0.5,
     vertex.label.dist = 2)





brunomars_index <-which(V(bruno_twitter_actor_graph)$name == "BrunoMars")
brunomars_index
# Get the degree, betweenness, and closeness 
brunomars_degree <- degree(bruno_twitter_actor_graph, mode = "total")[brunomars_index]
brunomars_betweenness <- betweenness(bruno_twitter_actor_graph, directed = FALSE)[brunomars_index]
brunomars_closeness <- closeness(bruno_twitter_actor_graph, mode = "total")[brunomars_index]
# Create a data frame with the centrality measures for "bruno Mars
brunomars_centrality_df <- data.frame(
  Node = "@brunomars",
  Degree = brunomars_degree,
  Betweenness = brunomars_betweenness,
  Closeness = brunomars_closeness
)
# Print the data frame
print(brunomars_centrality_df)

# Find the index of "@brunomars" in the original Post
post_index <-which(V(bruno_twomode_subgraph)$name == "@brunomars")
# Get the degree, betweenness, and closeness 
post_degree <- degree(bruno_twomode_subgraph, mode = "total")[brunomars_index]
post_betweenness <- betweenness(bruno_twomode_subgraph, directed = FALSE)[brunomars_index]
post_closeness <- closeness(bruno_twomode_subgraph, mode = "total")[brunomars_index]
# Create a data frame with the centrality measures for "bruno Mars
post_centrality_df <- data.frame(
  Node = "@brunomars",
  Degree = post_degree,
  Betweenness = post_betweenness,
  Closeness = post_closeness
)
# Print the data frame
print(brunomars_centrality_df)

