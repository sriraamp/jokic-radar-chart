# Install necessary packages
install.packages("fmsb")
install.packages("scales")
library(fmsb)
library(scales)

# Step 1: Create the dataset
players <- data.frame(
  row.names = c("Max", "Min", "Wilt Chamberlin", "Bill Russell", "Kareem Abdul Jabbar", 
                "Hakeem Olajuwan", "Shaquille O'Neal", "Nikola Jokic", "Victory Wembenyama"),
  Height = c(2.4, 2.0, 2.16, 2.08, 2.18, 2.13, 2.16, 2.11, 2.21),
  Weight = c(160, 100, 113, 100, 102, 116, 147, 129, 107),
  Points = c(34, 24, 30.1, 15.1, 24.6, 21.8, 23.7, 29.6, 24.3),
  Rebounds = c(22, 10, 22.9, 22.5, 11.2, 11.1, 10.9, 12.7, 11),
  Assists = c(12, 2, 4.4, 4.3, 3.6, 2.5, 2.5, 10.2, 3.7),
  Blocks = c(10, 2, 8.8, 8.1, 2.6, 3.09, 2.3, 0.6, 3.7),
  Rings = c(12, 0, 2, 11, 6, 2, 4, 1, 0)
)

# Step 2: Normalize data for radar chart
players_scaled <- as.data.frame(scale(players[-c(1,2), ], center = players["Min", ], scale = players["Max", ] - players["Min", ]))
players_scaled <- rbind(rep(1, 7), rep(0, 7), players_scaled)
rownames(players_scaled)[3:nrow(players_scaled)] <- rownames(players)[3:nrow(players)]

# Step 3: Assign team-inspired colors
colors <- c(
  "Wilt Chamberlin" = "#006BB6",
  "Bill Russell" = "#007A33",
  "Kareem Abdul Jabbar" = "#552583",
  "Hakeem Olajuwan" = "#CE1141",
  "Shaquille O'Neal" = "#FDB927",
  "Nikola Jokic" = "#0E2240",
  "Victory Wembenyama" = "#C4CED4"
)

# Step 4: Export chart to PNG
png("Basketball_Legends_Radar_Chart.png", width = 1200, height = 1200)

par(mar = c(1, 2, 2, 10))  # Add margin on right for legend
radarchart(players_scaled,
           axistype = 0,
           pcol = sapply(rownames(players_scaled)[3:9], function(x) colors[x]),
           pfcol = sapply(rownames(players_scaled)[3:9], function(x) alpha(colors[x], 0.2)),
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = NA,
           caxislabels = NA,
           cglwd = 0.8,
           vlcex = 1.2)

# Step 5: Define dominant stat per player (manually selected)
dominant_stat <- c(
  "Wilt Chamberlin" = "Points",
  "Bill Russell" = "Rings",
  "Kareem Abdul Jabbar" = "Rebounds",
  "Hakeem Olajuwan" = "Blocks",
  "Shaquille O'Neal" = "Weight",
  "Nikola Jokic" = "Assists",
  "Victory Wembenyama" = "Height"
)

# Step 6: Position player names near their dominant stat axis
vars <- colnames(players_scaled)
n_vars <- length(vars)
stat_angles <- setNames(seq(0, 2 * pi, length.out = n_vars + 1)[-1], vars)

for (i in 3:9) {
  player <- rownames(players_scaled)[i]
  stat <- dominant_stat[player]
  value <- players_scaled[player, stat]
  angle <- stat_angles[stat]
  x <- (value + 0.1) * sin(angle)
  y <- (value + 0.1) * cos(angle)
  text(x, y, labels = player, cex = 0.9, font = 2, col = colors[player])
}

# Step 7: Add a legend on the right
legend("topright", inset = c(-0.35, 0),
       legend = rownames(players_scaled)[3:9],
       col = sapply(rownames(players_scaled)[3:9], function(x) colors[x]),
       lty = 1, lwd = 2, cex = 0.8, bty = "n")

dev.off()
grid::grid.raster(png::readPNG("Nikola_Jokic_Radar_Chart.png"))
