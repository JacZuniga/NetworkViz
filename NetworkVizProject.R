# Getting working directory and setting working directory
getwd()
setwd("C:/Users/jackt/OneDrive/Desktop/NetworkViz")

# === 1. Load Libraries ===
library(tidyverse)
library(igraph)
library(ggraph)
library(ggrepel)

# === 2. Load Data ===
nodes <- read_csv("nodes.csv")
edges <- read_csv("edges.csv")

# Preview data structure
print("Node column names:")
print(names(nodes))
print("Edge column names:")
print(names(edges))

# === 2.1 Check for and Handle Duplicates ===
# Identify the ID column (adjust if your column has a different name)
id_column <- names(nodes)[1]  # assumes first column is ID
print(paste("Using", id_column, "as ID column"))

# Check for duplicates
print(paste("Total nodes:", nrow(nodes)))
print(paste("Unique nodes:", length(unique(nodes[[id_column]]))))

# Remove duplicates (keep first occurrence)
nodes_clean <- nodes[!duplicated(nodes[[id_column]]), ]
print(paste("Nodes after removing duplicates:", nrow(nodes_clean)))

# === 3. Fix Edge-Node Mismatch ===
# Get all unique artists mentioned in edges
edge_artists <- unique(c(edges[[1]], edges[[2]]))  # first two columns of edges
node_artists <- nodes_clean[[1]]  # first column of nodes

# Find missing artists
missing_in_nodes <- setdiff(edge_artists, node_artists)
print(paste("Artists in edges but not in nodes:", length(missing_in_nodes)))

# Filter edges to only include artists that exist in nodes
edges_clean <- edges[edges[[1]] %in% node_artists & edges[[2]] %in% node_artists, ]
print(paste("Edges before filtering:", nrow(edges)))
print(paste("Edges after filtering:", nrow(edges_clean)))

# === 4. Create Graph Object ===
print("Creating graph object...")
graph <- graph_from_data_frame(d = edges_clean, vertices = nodes_clean, directed = FALSE)
print("Graph created successfully!")

# === 5. FAST Centrality Measures (Skip slow ones) ===
print("Calculating degree centrality...")
V(graph)$degree <- degree(graph)

# SKIP these slow calculations for large networks:
# V(graph)$betweenness <- betweenness(graph)  # Takes too long
# V(graph)$closeness <- closeness(graph)      # Takes too long

# === 6. Community Detection ===
print("Detecting communities...")
communities <- cluster_louvain(graph)
V(graph)$community <- communities$membership

# === 7. Component Analysis ===
print("Analyzing components...")
components <- components(graph)
V(graph)$component <- components$membership

# === 8. Network Summary ===
print("=== NETWORK SUMMARY ===")
print(paste("Total nodes:", vcount(graph)))
print(paste("Total edges:", ecount(graph)))
print(paste("Number of communities:", max(V(graph)$community)))
print(paste("Average degree:", round(mean(V(graph)$degree), 2)))

# === 9. Create Subgraph for Better Visualization ===
# For large networks, show only well-connected nodes
min_degree <- 3  # Adjust this number based on your network
high_degree_nodes <- which(V(graph)$degree >= min_degree)

print(paste("Creating subgraph with nodes having degree >=", min_degree))
print(paste("Subgraph will have", length(high_degree_nodes), "nodes"))

if(length(high_degree_nodes) > 1000) {
  # If still too many nodes, take top 1000 by degree
  top_nodes <- order(V(graph)$degree, decreasing = TRUE)[1:1000]
  subgraph <- induced_subgraph(graph, top_nodes)
  print("Using top 1000 nodes by degree")
} else if(length(high_degree_nodes) > 0) {
  subgraph <- induced_subgraph(graph, high_degree_nodes)
} else {
  # If no high-degree nodes, use full graph but sample it
  sample_size <- min(500, vcount(graph))
  sampled_nodes <- sample(V(graph), sample_size)
  subgraph <- induced_subgraph(graph, sampled_nodes)
  print(paste("Using random sample of", sample_size, "nodes"))
}

# === 10. VISUALIZATION ===
print("Creating visualization...")
set.seed(123)

# Main network plot
network_plot <- ggraph(subgraph, layout = "fr") +
  geom_edge_link(alpha = 0.2, color = "gray", width = 0.5) +
  geom_node_point(aes(size = degree, color = as.factor(community)), alpha = 0.7) +
  scale_size_continuous(range = c(1, 6), name = "Degree") +
  scale_color_discrete(name = "Community") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Spotify Artist Network",
       subtitle = paste("Showing", vcount(subgraph), "most connected artists"))

# Display the plot
print(network_plot)

# === 11. Alternative Simple Plot ===
print("Creating simple visualization...")
simple_plot <- ggraph(subgraph, layout = "fr") +
  geom_edge_link(alpha = 0.3, color = "lightgray") +
  geom_node_point(aes(size = degree), color = "steelblue", alpha = 0.7) +
  scale_size_continuous(range = c(2, 8)) +
  theme_void() +
  labs(title = "Spotify Artist Network (Simple Version)")

print(simple_plot)

# === 12. Display Plots in RStudio ===
print("Displaying plots in RStudio...")

# Force display the main plot
dev.new()
print(network_plot)

# Wait a moment then show simple plot
Sys.sleep(2)
dev.new() 
print(simple_plot)

# Also try direct plot() function as backup
plot(network_plot)
plot(simple_plot)

# === 13. Export Results ===
print("Exporting results...")
node_metrics <- data.frame(
  id = V(graph)$name,
  degree = V(graph)$degree,
  community = V(graph)$community,
  component = V(graph)$component
)

write_csv(node_metrics, "spotify_artist_metrics.csv")

# Show top artists by degree
print("=== TOP 10 MOST CONNECTED ARTISTS ===")
top_artists <- head(node_metrics[order(-node_metrics$degree), ], 10)
print(top_artists)

print("=== ANALYSIS COMPLETE ===")
print("Check your Plots tab in RStudio!")
print("PNG files saved to your working directory")
print("Metrics exported to spotify_artist_metrics.csv")
