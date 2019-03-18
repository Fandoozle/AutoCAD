# AutoCAD-Scripts
A collection of helpful AutoCAD scripts used to speed up the set up process and overall design of projects.
Also included is a dwg file for dynamic MEP blocks

 	DrawingScales.xlsx 
  A list of view port scaling key factors
  
	Dynamic Blocks and Such
  A series of MEP and Architectural dynamic blocks
  
	ExportToAutoCAD
  A simple script that exports the drawing 
  
	ImageFrameRemoval
  Easy fix for the "Image Frame Bug"
  
	allowexplodeblocks
  Allows all blocks in a drawings to be burstable/exploadable
  
	changeColors
  This is a script that will thaw and unhide all layers then change the layer colors. This is purely an example of how to write the     
  script. Layer colors can be anything. You can even change linetype with this example.
  
	continuosarc
  Creates a way to make arcs continuously. 
  
	continuousLine
  Creates a way to make lines continuously. Different from a polyline.
  
	deleteAroundArea
  This allows you to create a box and everything outside this box will be deleted
  
	drawlines
  Draws lines for all layers of a drawing
  
	portme
  Lisp routine similar to the script "ExportToAutocad" which allows you to export the existing drawing to a 2013 

 	rename-multiple-files
  This is a simple batch file that can rename files in a folder
	
   	rotateMultipleBlocks
  Select specific block and you can rotate them around their basepoint

	setup
  This is a tool that preps a CAD base file to be used for MEP drawing. It clears useless data within the file making it a smaller size.
 
 	breakAtIntersection
  This routine breaks a line at a specified gap distance. 
  To use it enter the distance desired and select the point at intersection then select the line to use to break with.
    
 	dwgFoo
This cleans up the drawing by thawing and making all layers visible, then bursting all blocks in the drawing.
Only use the bursting section for smaller drawings. It will take a while on larger ones.
Overkills any overlap after the bursting
Resets the layer properties to "ByLayer"
Audits the drawing
Purgest the drawing twice
Zooms to extents then saves
Pauses during each command to allow for processing

 	dwgClean
This routine cleans up a drawing by running and audit and a purge VCD (very clean drawing)
