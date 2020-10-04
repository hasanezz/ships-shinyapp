source("functions.R")
library(testthat)
ships_data<-read_csv('ships.csv') 
ships_data_oneobservation<-ships_data %>% group_by(SHIPNAME) %>% summarise(n=n()) %>% filter(n==1)
ships_data<-ships_data %>%filter(!(SHIPNAME%in%c(ships_data_oneobservation$SHIPNAME)))



for (shipname in unique(ships_data$SHIPNAME)) {
  print(shipname)
  test_that("checkin if we are able to calculate max distance for every ship 
            it should return one row with distance greater than zero and having multiple observations", {
    expect_gte((max_consecutive_observation_distance(ships_data,shipname,TRUE))$distance_in_meters,0)
            })
  }

for (shipname in unique(ships_data$SHIPNAME)) {
  print(shipname)
  test_that("checkin if a ship has two identical max distance calculated that its returning the one with recent date", {
              expect_true(max_consecutive_observation_distance_with_equal_distances(ships_data,shipname,TRUE))
})
}

#View(max_consecutive_observation_distance_with_equal_distances(ships_data,"[SAT-AIS]",TRUE))
