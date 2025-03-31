## Make links, then generate MD's

# Part 1: make links:

mainPath <- "/Users/juliest/Github folder/jasp-gif-library"

filePaths <- list.files(path = mainPath, full.names = FALSE, pattern = "\\.mp4", recursive = TRUE)

myFileLinks <- list()

for (filePath in filePaths) {
  
  folderName <- dirname(filePath)

  fileName <- tools::file_path_sans_ext(basename(filePath))
  
  mp4Link <- paste0("[.mp4](https://julia-stadnik.github.io/jasp-gif-library/", 
                    folderName, "/", fileName, ".mp4)")

  if (is.null(myFileLinks[[folderName]])) {
    # If the folder doesn't exist in the list, create a new entry
    myFileLinks[[folderName]] <- list(mp4Link = mp4Link)
  } else {
    # If the folder already exists, append the new link
    myFileLinks[[folderName]] <- append(myFileLinks[[folderName]], list(mp4Link = mp4Link))
  }
}

# Part 2: generate MDs:

outputFile <- "output.md"

fileWrite <- file(outputFile, "w")

writeLines("# JASP GIF Library\n", fileWrite)

for (folderName in names(myFileLinks)) {
  writeLines(paste0("## ", folderName, "\n"), fileWrite)  # Add folder name as a subheading
  
  for (link in myFileLinks[[folderName]]) {
    writeLines(paste0("- ", link, "\n"), fileWrite)  # Add each link as a list item
  }
  
  writeLines("\n", fileWrite)  # Add a blank line between folders
}

close(fileWrite)


