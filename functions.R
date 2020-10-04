
render_header <- function() {
  
  div(class="ui three statistics",
    div(class="statistic",
        div(class='value',n_distinct(ships_type_names$ship_type)),
        div(class="label","Ships types")
        ),
    div(class="statistic",
        div(class='text value',toupper(high_traffic_port_name$PORT)),
        div(class="label","High traffic port")
    ),
    div(class="statistic",
        div(class='value',icon(class = 'ship icon'),n_distinct(ships_type_names$SHIPNAME)),
        div(class="label","Ships")
    )
  )
  
}
render_dropdown <- function(name, label, choices, default_text,value) {
  div(class = "column",
      
      dropdown_input(input_id = name, choices = choices, default_text = default_text,value=value)
  )
}
render_ship_types <- function() {
  render_dropdown("ship_type", "Ship Type", unique(ships_data$ship_type),
                  "Select Ship Type...","Tug")
}
render_ship_names_dropdown <- function(ships_type_names, input) {
  
  ship_names_choices <- eventReactive(input$ship_type, {
    if (!is.null(input$ship_type)) {
      ships_type_names %>% filter(ship_type%in%c(input$ship_type)) %>% select(SHIPNAME)
    } else {
      ships_type_names %>%  select(SHIPNAME)
    }
    
  })
  
  default_text <- reactive({
    if (length(ship_names_choices()) != 0) {
      "Choose Ship Name…"
    } else {
      "Select a ship type first "
    }
  })
  
  renderUI({
    #ship_names
    render_dropdown("ship_names", "Ships Names", ship_names_choices(),
                    default_text(),"AGIS")
  })
}
render_both_dropdowns <- function() {
  renderUI({
    div(class = "ui center aligned three column grid",
        render_ship_types(),
        div(class = "column",
            uiOutput("ship_names")
        ),
        div(class = "column",
            checkbox_input("is_parked",label = "Parked",is_marked = TRUE)
        )
        
    )
  })
}
render_dropdowns_ui <- function() {
  div(class = "ui raised segment",
      uiOutput("dropdowns")
  )
}
render_profile_ship_map <- function(data, input) {
  
  renderLeaflet({
    if(!is.null(input$ship_names))
    {  
    filtered_data <- max_distance_calculation(data, input)
    if(is.null(filtered_data)){
      leaflet()%>%
        addTiles()  
    }
    else{
    leaflet()%>%
      addTiles() %>%
      addCircleMarkers(lng = filtered_data$LON,lat =filtered_data$LAT ,color = "green",label = "start") %>%
      addCircleMarkers(lng = filtered_data$next_lon,lat =filtered_data$next_lat ,color = "red",label = "End")
    }
    }
  })
}
render_segment_ui <- function(label, output_ui) {
  if(label=="ship map"){
    div(class = "ui raised segment",
        div(class = "ui infobox",
            htmlOutput("info_box")),
        output_ui %>%
          withSpinner()
        
    )  
  }
  else{
    div(class = "ui raised segment",
        output_ui %>%
          withSpinner()
        
    )
  }
  
}
render_map_ui <- function() {
  render_segment_ui("ship map", leafletOutput("ship_map"))
}
render_table_ships <- function(ships_data, input) {
  renderDataTable({
    if (is.null(input$ship_names)) {
      ships_data
      
      
    }
    else{
      
      a1 <-ships_data %>% filter(SHIPNAME == input$ship_names, is_parked == input$is_parked) %>% arrange(DATETIME)
      a1
    }
  }, options = list(scrollX = TRUE))

}
max_distance_calculation<-function(ships_data, input) {
if(is.null(input$ship_names)){
  NULL
}
  max_consecutive_observation_distance(ships_data,input$ship_names,input$is_parked)
}
render_info_box_text<- function(data, input) {
  renderUI({
  
    if(is.null(input$ship_names)){
      text_1<-div(h4("No ship selected"))
    }
    else
      {
        d<-max_distance_calculation(data, input)
        
        if(!is.null(d)){
            text_1<-div(class = "ui two column grid",
                        div(class="row",
                            div(class="two wide column",
                                icon(class = "passport icon")
                                ),
                            div(class="four wide column","Ship_id:"),
                            div(class="ten wide column",d$SHIP_ID)
                            ),
                        div(class="row",
                            div(class="two wide column",
                                icon(class = "compass icon")
                            ),
                            div(class="four wide column","Destination:"),
                            div(class="ten wide column",d$DESTINATION)
                        ),
                        div(class="row",
                            div(class="two wide column",
                                icon(class = "tachometer alternate icon")
                            ),
                            
                            div(class="four wide column","Speed:"),
                            div(class="ten wide column",d$SPEED)
                        ),
                        div(class="row",
                            div(class="two wide column",
                                icon(class = "road icon")
                            ),
                            div(class="four wide column","Max_distance:"),
                            div(class="ten wide column",paste0((round(d$distance_in_meters,2))," m"))
                        ),
                        div(class="row",
                            div(class="two wide column",
                                icon(class = "ruler icon")
                            ),
                            div(class="four wide column","Ship Length:"),
                            div(class="ten wide column",paste0((round(d$LENGTH,2))," m"))
                        )
                        )
        }
        else{
          text_1<-div(h4("No available data"))
        }
      
      }
    
      text_1
  })
} 
max_consecutive_observation_distance<-function(ships_data,ship_name,is_parked){
  a1<-ships_data %>% filter(SHIPNAME==ship_name,is_parked==is_parked) %>% arrange(DATETIME)
  if(dim(a1)[1]>0){
    a2<-a1[-1,1:2] %>% rename(next_lat=LAT,next_lon=LON)
    a3<-a1[1:(dim(a1)[1]-1),]
    a4<-cbind(a3,a2) 
    a5<-a4 %>% mutate(distance_in_meters=distHaversine(cbind(LON, LAT), cbind(next_lon, next_lat) ) ) %>% filter(distance_in_meters==max(distance_in_meters)) %>% arrange(DATETIME)
    a5<-a5[(dim(a5)[1]),]
    a5
  }
  else{
    NULL
  }

}
max_consecutive_observation_distance_with_equal_distances<-function(ships_data,ship_name,is_parked){
  a1<-ships_data %>% filter(SHIPNAME==ship_name,is_parked==is_parked) %>% arrange(DATETIME)
  if(dim(a1)[1]>0){
    a2<-a1[-1,1:2] %>% rename(next_lat=LAT,next_lon=LON)
    a3<-a1[1:(dim(a1)[1]-1),]
    a4<-cbind(a3,a2) 
  a5<-a4 %>% mutate(distance_in_meters=distHaversine(cbind(LON, LAT), cbind(next_lon, next_lat) ) ) %>% filter(distance_in_meters==max(distance_in_meters)) %>% arrange(DATETIME)
    (((((a5 %>% filter(DATETIME==max(a5$DATETIME)))[1,])$distance_in_meters)==((a5[(dim(a5)[1]),])$distance_in_meters)))
    #a5[(dim(a5)[1]),]
    #(a5 %>% filter(DATETIME==max(a5$DATETIME)))[1,]
  }
  else{
    NULL
  }
  
  
}
