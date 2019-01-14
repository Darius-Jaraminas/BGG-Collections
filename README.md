# BGG-Collections

> Extract [Board Game Geek](http://boardgamegeek.com) data from selected users collections and format it nicely.

# Background

The main purpose of this initially was to create a list of all board games in our game group. We sometimes use this list to select the games we will be playing next time.

## Install

The code is in [R](https://www.r-project.org/). Also the project is employing [RStudio's](https://www.rstudio.com/) R project feature (.Rproj file in the directory).
So to run this smoothly you will need both [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/)

## Usage

1. Lauch [RStudio](https://www.rstudio.com/)
2. Go to File -> Open Project -> select `BGG-Collections.Rproj`
2. Source `01_Read_All_Collections.R`
3. Source `02_Read_Game_Info.R`

At the moment output file  is being written into `output` folder, which you will need to create in the project folder.

## Contributing

PRs accepted.

## License

MIT