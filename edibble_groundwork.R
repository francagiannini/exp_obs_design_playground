library(edibble)

groundwork_obs <-
  design(name = "LL Groundwork (observational)") |>
  
  # Observational unit assignment 
  set_units(
    living_lab =5,
    farm  = nested_in(living_lab,10),
    field = nested_in(farm,3), # In the rest, there are 3 fields
    depth  = nested_in(field, 3)) |>
  
  set_trts(treatment = c("Treatment_RLGP", "Control_Reference")) |> 
  # Records are matched to the correct unit level
  set_rcrds_of(living_lab =c("manager"),
  farm=c("farmer", "type", "survey..."),
  field=c("Lat", "Long", "CropRotation"),
  depth= c("SoilIndicators...")) |> 
  
  allot_trts(treatment ~ field) |> 
  assign_trts("random")

# Look at the design
groundwork_obs

plot(groundwork_obs)

# Generate the simulated data collection table
simulated_sampling_plan <- serve_table(groundwork_obs)

