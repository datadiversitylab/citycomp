library(here)

cities<-read.csv(here("data", "cities_merged.csv"))


anthro_pca<-read.csv(here("data","human_only_cluster_data","pca_vals_anthro.csv"))
climate_pca<-read.csv(here("data","clim_only_30s_cluster_data","pca_vals_climate.csv"))
combined_pca<-read.csv(here("data","combined_30s_cluster_data","pca_vals_combined.csv"))

anthro_stab<-read.csv(here("data","human_only_cluster_data","per_city_stability_anthro.csv"))
climate_stab<-read.csv(here("data","clim_only_30s_cluster_data","per_city_stability_climate.csv"))
combined_stab<-read.csv(here("data","combined_30s_cluster_data","per_city_stability_combined.csv"))


climate_cities<-data.frame(cities[,1:25],climate_pca[,3:4],climate_stab[,3:5])
anthro_cities<-data.frame(cities[,1:5],cities[,25:33],anthro_pca[,3:4],anthro_stab[,3:5])
combined_cities<-data.frame(cities,combined_pca[,3:4],combined_stab[,3:5])


pca_dists<-function(dataset){
  city_list<-dataset$City
  city_dim<-nrow(dataset)
  initial_matrix<-matrix(0,nrow=city_dim,ncol=city_dim,dimnames=list(city_list,city_list))
  for(i in seq_len(city_dim)){
    x1<-dataset[[3]][i]
    y1<-dataset[[3]][i]
    for(j in seq_len(city_dim)){
      x2<-dataset[[3]][j]
      y2<-dataset[[4]][j]
      euclidean_distance<-sqrt(((x1-x2)^2)+((y1-y2)^2))
    
      initial_matrix[i,j]<-round(euclidean_distance,3)
    }
  } 
  return(initial_matrix)
}

pca_dists2<-function(dataset){
  pc_coords <- as.matrix(dataset[, 3:4])
  distances <- round(as.matrix(dist(pc_coords)),4)
  rownames(distances) <- dataset$City
  colnames(distances) <- dataset$City
  return(distances)
}

dists_anthro<-pca_dists2(anthro_pca)
dists_combined<-pca_dists2(combined_pca)
dists_climate<-pca_dists2(climate_pca)

write.csv(climate_cities,here("data","clim_only_30s_cluster_data","climate_dataset.csv"))
write.csv(combined_cities,here("data","combined_30s_cluster_data","combined_dataset.csv"))
write.csv(anthro_cities,here("data","human_only_cluster_data","anthro_dataset.csv"))

write.csv(dists_climate,here("data","clim_only_30s_cluster_data","climate_dists.csv"))
write.csv(dists_combined,here("data","combined_30s_cluster_data","combined_dists.csv"))
write.csv(dists_anthro,here("data","human_only_cluster_data","anthro_dists.csv"))






