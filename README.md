# PPR-Fantasy-Points-Prediction

A fun side project where I built 8 separate models (split by years in the league) to project fantasy points for my fantasy football league.

## Model Breakdown
Games Played Projections

I created 4 models to estimate the number of games a player will play, based on years in the league:
- 1 season played
- 2 seasons played
- 3 seasons played
- 3+ seasons played

## Fantasy Points Per Game (PPG) Projections
I also built 4 additional models (same breakdown as above) to project fantasy points per game.
This approach:

- Helps reduce the impact of injuries (since PPG reflects a player’s skill, not just missed games).
- Separates skill evaluation from availability.

## Methodology

Model Type: Simple linear regression (to capture general trends).
Regression to the Mean: Many player projections naturally regress toward the mean.

Business Rule Adjustment:
If a player played 16+ games in prior seasons, I removed the regression adjustment to better reflect consistency.

## Final Projection Formula

Fantasy Points Per Game Projection × Projected Games
