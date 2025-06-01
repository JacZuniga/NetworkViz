<<<<<<< HEAD
Spotify Artist Network Analysis
Authors
Jack T Zuniga & Sakaria Dirie


=======
Spotify Artist Network Analysis 

Authors 
Jack T Zuniga & Sakaria Dirie

Project Summary

This R script creates network visualizations of Spotify artist relationships using node and edge data from CSV files. The script loads artist data, cleans duplicates and mismatched connections, then generates network graphs showing how artists are connected through collaborations or similarities. It calculates network metrics like degree centrality and detects artist communities using the Louvain algorithm. The script produces two visualizations: a main network plot with community colors and a simple version with uniform styling. For large networks, it intelligently filters to show only the most connected artists for better readability. All plots are displayed in RStudio and automatically saved as high-resolution PNG files. Required files are nodes.csv (artist information) and edges.csv (artist relationships). The script outputs spotify_network_main.png, spotify_network_simple.png, and spotify_artist_metrics.csv with network statistics. Simply place your CSV files in the working directory, update the file path, and run the script to explore artist collaboration networks and similarity clusters.
>>>>>>> making changes
