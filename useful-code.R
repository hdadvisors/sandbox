# Pick out final values in line chart to add as labels

d_ends <- df |> 
  top_n(1, X) |> 
  pull(Y)

# Calculate cumulative percent change

chg <- df |> 
  group_by(group) |> # Assign group(s)
  arrange(time) |> # Get in correct order
  mutate(chg = x - first(x), # Cumulative change
         pct_chg = chg / first(x)) # Percent change

# Calculate mortgage payment

pmt =  principal*((r/12)/(1-(1+(r/12))^(-360)))