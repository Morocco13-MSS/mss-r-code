# mss-r-code

# R
## What is R?

R is an open source statistical computing language. Along with Python, it is now a standard in data science and analytics.

See (About R)[https://www.r-project.org/about.html]

## How do set R on my computer?

- [ ] Download R

https://cran.r-project.org/mirrors.html
You choose the mirror or the location you will download packages from. I chose https://cloud.r-project.org/ If you choose this mirror as well then follow https://cloud.r-project.org/ and select the installation based on your OS.

- [ ] Download RStudio
This is highly recommended unless you want to do troubleshooting via command line.
https://www.rstudio.com/products/rstudio/download/#download

## Cloning Repository

- [ ] In RStudio go to File --> New Project...Version Control --> Git and enter in the Repository URL according to the below and the directory on your computer you wish to put this repo (under Create project as subdirectory of:)

`git clone git@github.com:Morocco13-MSS/mss-r-code.git` (ssh) or `git clone https://github.com/Morocco1` (https)

- Afterwards, go to View --> Show Git in order to interact with Git via RStudio

## Package Installation
You will need to open a script in R and run the following commands

```
install.packages("ggplot2")
install.packages("needs")
install.packages("RMySQL")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("funnelR")
install.packages("qcc")
```

Please see @johnlin89 for help.

## Character Encoding

We need to be able to handle French characters like è, é, ç, œ, ë etc.

When opening a .R file, please specify the encoding to be UTF-8.

https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

Need to set environment to use UTF-8??

## Windows Special Instructions

In order for NodeJS to call R, you need to add R to PATH.

https://stackoverflow.com/questions/10077689/r-cmd-on-windows-7-error-r-is-not-recognized-as-an-internal-or-external-comm

