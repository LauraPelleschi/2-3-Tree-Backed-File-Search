# 2-3-Tree-Backed-File-Search

This file search program fully supports boolean searches with the functions `and_not` and `or_not` . 
Returns list of files matching user's search query

Example of how to use in utop:
1)  create a folder in source code `desired_folder_name`
2)  place arbitrary amount of .txt files you wish to search in this folder
3)  navigate to source code folder in command terminal
4)  run `make compile`
5)  enter utop
6)  run ` #use "engine.ml" ;; `
7)  run ` let arbitrary_name = TreeEngine.index_of_dir "desired_folder_name";; `
8)  create string list of words you do want included in your search:     `and_list`
9)  create string list of words you do not want included in your search: `not_list`
10) run `TreeEngine.and_not arbitrary_name and_list not_list`
11) enjoy association list mapping desired words to file names 

Acknowledgements:
Collaborated with Daniel Vicuña (dav74@cornell.edu) 

