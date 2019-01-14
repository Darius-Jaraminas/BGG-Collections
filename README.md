# BGG-Collections

> Extract [Board Game Geek](http://boardgamegeek.com) data from selected users collections and format it nicely.

# Background

The main purpose of this initially was to create a list of all board games in our game group. We sometimes use this list to select the games we will be playing next time.

## Install

The code is in [R](https://www.r-project.org/). Also the project is employing [RStudio's](https://www.rstudio.com/) R project feature (.Rproj file in the directory).
So to run this smoothly you will need both [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/)

## Usage

#### Inputs

All inputs are stored in `input` folder. There are 3 inputs:

1. `Nick Mapping.csv` - list of nicknames
2. `Max Player Adjustment.csv` - manual adjustment of max player count due to expansions
3. `Meta.csv` - meta file for changing variable names, ordering variables and dropping unnecessary variables

#### Ouput

Final data is written `output` folder.

#### Running code

1. Lauch [RStudio](https://www.rstudio.com/)
2. Go to File -> Open Project -> select `BGG-Collections.Rproj`
3. Source `01_Read_All_Collections.R`
4. Source `02_Read_Game_Info.R`

## Contributing

PRs accepted.

## License

MIT